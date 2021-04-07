{-# LANGUAGE DeriveGeneric #-}

module App.Persistence where

import GHC.Generics

import qualified Data.Map as Map

import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.ByteString.Lazy.Char8 as B

import qualified Data.Aeson as A
import qualified Data.Aeson.Encode.Pretty as P

type FileId = Text

type EmojiBindings = Map.Map Text FileId

data Persistence = Persistence
    { p'emojiBindings :: EmojiBindings
    } deriving (Generic, Show)


instance A.FromJSON Persistence

instance A.ToJSON Persistence

loadPersistence :: IO (Either String Persistence)
loadPersistence = do
    text <- readFile "persistence.json"
    pure $ A.eitherDecode $ B.pack text

savePersistence :: Persistence -> IO ()
savePersistence p = do
    let text = B.unpack $ P.encodePretty p
    writeFile "persistence.json" text
