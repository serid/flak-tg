{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE NamedFieldPuns #-}

-- Structures representing arguments for Api methods

module Flak.Argument where

import GHC.Generics

import Data.Text (Text)
import qualified Data.Aeson as A

import Flak.AesonUtils

-- Argument for /getUpdates
data GetUpdates = GetUpdates
    { offset :: Maybe Int
    , limit :: Maybe Int
    , timeout :: Maybe Int
    , allowed_updates :: Maybe [Text]
    } deriving (Generic, Show)

-- Argument for /sendMessage
-- TODO: add missing fields
data SendMessage = SendMessage
    { chat_id :: Int
    , text :: Text
    , disable_notification :: Maybe Bool
    , reply_to_message_id :: Maybe Int
    } deriving (Generic, Show)

-- Argument for /sendSticker
-- TODO: add missing fields
data SendSticker = SendSticker
    { ss'chat_id :: Int
    , ss'sticker :: Text
    } deriving (Generic, Show)

instance A.ToJSON GetUpdates where
    toJSON = A.genericToJSON A.defaultOptions
        { A.omitNothingFields = True }

instance A.ToJSON SendMessage where
    toJSON = A.genericToJSON A.defaultOptions
        { A.omitNothingFields = True }

instance A.ToJSON SendSticker where
    toJSON = A.genericToJSON trimLabel
        { A.omitNothingFields = True }

defaultSendMessage :: Int -> Text -> SendMessage
defaultSendMessage chat_id text = SendMessage
    { chat_id
    , text
    , disable_notification = Nothing
    , reply_to_message_id = Nothing
    }

sendMessageWithReply :: Int -> Text -> Int -> SendMessage
sendMessageWithReply chat_id text reply_id =
    (defaultSendMessage chat_id text) { reply_to_message_id = Just reply_id }
