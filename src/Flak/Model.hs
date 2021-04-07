{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module Flak.Model where

import GHC.Generics

import Data.Text (Text)
import qualified Data.Text as T
import Control.Applicative

import qualified Data.Aeson as A

import Flak.AesonUtils

data GetUpdatesResponse = GetUpdatesResponse
    { ok :: Bool
    , result :: [Update]
    } deriving (Generic, Show)

-- TODO: fill in other fields
data Update = Update
    { update_id :: Int
    , message :: Maybe Message
    } deriving (Generic, Show)

-- TODO: fill in other fields
data Message = Message
    { message_id :: Int
    , from :: User
    , date :: Int
    , chat :: Chat
    , text :: Maybe Text
    , sticker :: Maybe Sticker
    } deriving (Generic, Show)

-- TODO: fill in other fields
data User = User
    { user'id :: Int
    , user'first_name :: String
    , user'last_name :: Maybe String
    , user'username :: Maybe Text
    } deriving (Generic, Show)

-- TODO: fill in other fields
data Chat = Chat
    { chat'id :: Int
    , chat'type :: Text
    , chat'username :: Maybe Text
    , chat'first_name :: Maybe String
    , chat'last_name :: Maybe String
    } deriving (Generic, Show)

-- TODO: fill in other fields
data Sticker = Sticker
    { file_id :: Text
    } deriving (Generic, Show)

instance A.FromJSON GetUpdatesResponse

instance A.ToJSON GetUpdatesResponse

instance A.FromJSON Update

instance A.ToJSON Update

instance A.FromJSON Message

instance A.ToJSON Message

instance A.FromJSON User where
    parseJSON = A.genericParseJSON trimLabel

instance A.ToJSON User

instance A.FromJSON Chat where
    parseJSON = A.genericParseJSON trimLabel

instance A.ToJSON Chat

instance A.FromJSON Sticker

instance A.ToJSON Sticker
