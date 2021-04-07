{-# LANGUAGE OverloadedStrings #-}

module Main where

import Network.HTTP.Client
import Network.HTTP.Client.TLS

import Network.HTTP.Types.Status (statusCode)

import Data.Aeson (object, (.=), encode, decode)
import Data.Text (Text)

import qualified Data.Aeson as A
import qualified Data.ByteString.Lazy.Char8 as B

getMe :: IO ()
getMe = do putStrLn "Hello, Haskell!"
           manager <- newManager tlsManagerSettings

           request <- parseRequest "http://httpbin.org/get"
           response <- httpLbs request manager

           putStrLn $ "The status code was: " ++ (show $ statusCode $ responseStatus response)
           print $ responseBody response

postExample :: IO ()
postExample = do putStrLn "Hello, Haskell!"
                 manager <- newManager tlsManagerSettings

                 let requestObject = object [("name"::Text) .= ("Michael"::Text), ("age"::Text) .= (30::Int)]

                 initialRequest <- parseRequest "http://httpbin.org/post"
                 let request = initialRequest { method = "POST", requestBody = RequestBodyLBS $ encode requestObject }

                 response <- httpLbs request manager
                 putStrLn $ "The status code was: " ++ (show $ statusCode $ responseStatus response)
                 let decoded = A.eitherDecode $ responseBody response :: Either String A.Value
                 print decoded

-- nowLemmeTryMyself :: IO ()
-- nowLemmeTryMyself = do putStrLn "Hello, Haskell!"
--                        manager <- newManager tlsManagerSettings

--                        let requestObject = object [("name"::Text) .= ("Michael"::Text), ("age"::Text) .= (30::Int)]
--                        let requestObject = object []

--                        initialRequest <- parseRequest $ "https://api.telegram.org/bot" ++ token ++ "/getMe"
--                        let request = initialRequest { method = "POST", requestBody = RequestBodyLBS $ encode requestObject }

--                        response <- httpLbs request manager
--                        putStrLn $ "The status code was: " ++ (show $ statusCode $ responseStatus response)
--                        let decoded = A.eitherDecode $ responseBody response :: Either String A.Value
--                        print decoded

main :: IO ()
main = do postExample
