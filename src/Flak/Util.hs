module Flak.Util where

import Control.Monad

import Data.Maybe
import Data.List (dropWhileEnd)
import qualified Data.Aeson as A
import qualified Data.ByteString.Lazy.Char8 as B
import qualified Data.Aeson.Encode.Pretty as P
import qualified Safe as S

(<|) :: (a -> b) -> a -> b
f <| x = f x

(|>) :: a -> (a -> b) -> b
x |> f = f x

fromRight :: Either String b -> b
fromRight (Right x) = x
fromRight (Left x) = error x

prettyJson :: B.ByteString -> IO ()
prettyJson string = do let decoded = A.decode $ string :: Maybe A.Value
                       putStrLn $ B.unpack $ P.encodePretty $ decoded

iterateM :: (a -> IO a) -> a -> IO b
iterateM k state1 = do
    state2 <- k state1
    iterateM k state2

iterateM' :: (a -> IO a) -> a -> IO a
iterateM' k state = foldM (\x () -> k x) state (repeat ())

-- Like iterateM but the iteration can be terminated by returning Break
iterateM'' :: (a -> IO (ControlFlow a b)) -> a -> IO b
iterateM'' k state = do
    cf <- k state
    case cf of
        Continue state' -> iterateM'' k state'
        Break r -> pure r

-- Trim string from left until needle (drops needle too)
trimStart :: Eq a => a -> [a] -> [a]
trimStart needle str = fromMaybe str $ S.tailMay $ dropWhile (/= needle) str

-- Trim string from right until needle (drops needle too)
trimEnd :: Eq a => a -> [a] -> [a]
trimEnd needle str = fromMaybe str $ S.initMay $ dropWhileEnd (/= needle) str

data ControlFlow a b = Continue a | Break b
