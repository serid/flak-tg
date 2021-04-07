{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NamedFieldPuns #-}

module Flak.Api where

import Data.IORef
import Data.Maybe
import Data.Functor

import Network.HTTP.Types.Status (statusCode)
import Network.HTTP.Client (method, requestBody, requestHeaders)
import qualified Network.HTTP.Client as HTTP
import qualified Network.HTTP.Client.TLS as TLS

import qualified Data.Aeson as A
import qualified Data.ByteString.Lazy.Char8 as B
import qualified Safe

import qualified Flak.Argument as Argument
import qualified Flak.Model as Model

import Flak.Util

data Api = Api { manager :: HTTP.Manager
               , token :: String
               , lastUpdate :: IORef (Maybe Int)
               }

type ApiAction = Api -> IO ()

new :: String -> IO Api
new token = do
    manager <- HTTP.newManager TLS.tlsManagerSettings
    lastUpdate <- newIORef Nothing
    pure $ Api { manager, token, lastUpdate }

requestAction :: Api -> ApiAction -> IO ()
requestAction api f = f api

-- Peeks updates without scheduling them to be removed from the update queue
requestGetUpdates :: Argument.GetUpdates -> Api -> IO Model.GetUpdatesResponse
requestGetUpdates args api = do
    lastUpdateId <- readIORef $ lastUpdate api
    upds <- request api "getUpdates" $ A.encode (args { Argument.offset = lastUpdateId })
    pure $ fromRight $ A.eitherDecode $ upds

-- Gets updates and schedules them to be removed from the update queue on next "getUpdate"
requestYankUpdates :: Argument.GetUpdates -> Api -> IO Model.GetUpdatesResponse
requestYankUpdates args api = do
    upds <- requestGetUpdates args api

    -- If there are any updates, new id of "api.lastUpdate" would be "update_id" of the last update + 1, otherwise set it to Nothing
    let newLastUpdateId = (upds |> Model.result |> Safe.lastMay) <&> \lastUpdateFromUpds ->
                          lastUpdateFromUpds |> Model.update_id + 1

    writeIORef (lastUpdate api) newLastUpdateId

    pure upds

-- Send a message
requestSendMessage :: Argument.SendMessage -> Api -> IO Model.Message
requestSendMessage args api = do
    upds <- request api "sendMessage" $ A.encode args
    pure $ fromRight $ A.eitherDecode $ upds

-- Send a sticker
requestSendSticker :: Argument.SendSticker -> Api -> IO Model.Message
requestSendSticker args api = do
    upds <- request api "sendSticker" $ A.encode args
    pure $ fromRight $ A.eitherDecode $ upds

-- Performs scheduled actions like removing updates from queue
flush :: Api -> IO ()
flush api = do
    putStrLn "Cleaning up"
    lastUpdateId <- readIORef $ lastUpdate api
    let arg = Argument.GetUpdates { Argument.offset = lastUpdateId
                                                         , Argument.limit = Nothing
                                                         , Argument.timeout = Nothing
                                                         , Argument.allowed_updates = Nothing
                                                         }
    _upds <- requestYankUpdates arg api
    pure ()

-- Generic Telegram API request
request :: Api -> String -> B.ByteString -> IO B.ByteString
request api method requestObject = do
                                      let requestUrl = "https://api.telegram.org/bot" ++ token api ++ "/" ++ method
                                      putStrLn $ "Requesting URL: " ++ requestUrl
                                      putStrLn $ "With body: " ++ (B.unpack requestObject)

                                      requestSettings <- HTTP.parseRequest requestUrl
                                      let customHeaders = [("Content-Type", "application/json")]
                                      let request = requestSettings { method = "POST", requestBody = HTTP.RequestBodyLBS $ requestObject, requestHeaders = customHeaders}

                                      response <- HTTP.httpLbs request (manager api)
                                      putStrLn $ "Response status code: " ++ (show $ statusCode $ HTTP.responseStatus response)
                                      pure $ HTTP.responseBody response

