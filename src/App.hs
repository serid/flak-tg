{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings #-}

module App where

import Data.Functor
import Data.Function
import Data.Maybe
import Data.IORef
import Control.Monad
--import Control.Monad.State.Lazy
import Control.Monad.Trans.State
import Control.Monad.Trans.Maybe
import Control.Monad.Trans.Class (lift)

import GHC.OldList (find)

import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Map as Map

import qualified Data.List.Extra as E

import qualified Flak.Api as Api
import qualified Flak.App as App
import qualified Flak.Model as Model
import qualified Flak.Argument as Argument

import Flak.Util (trimEnd, fromRight)
import Flak.Signals

import qualified App.Persistence as Persistence

data Mode = Start | WaitingStickerInfo | WaitingAddEmoji String
data AppState = AppState
    { as'mode :: Mode
    , as'persistence :: Persistence.Persistence
    , as'continueEh :: Bool
    }

type Handler = App.UpdateHandler AppState

mapMode :: (Mode -> Mode) -> AppState -> AppState
mapMode f state = state { as'mode = (f $ as'mode state) }

voidIO :: (b -> IO a) -> (b -> IO ())
voidIO k = void . k

hoistMaybe :: Monad m => Maybe a -> MaybeT m a
hoistMaybe = MaybeT . pure

handleRust :: Handler
handleRust = App.UpdateHandler $ \update -> do
    pure $ fromMaybe [] (actions update)
    where
    actions :: Model.Update -> Maybe [Api.ApiAction]
    actions update = do
        message <- Model.message update
        --let (Model.User userid _ _ _) = Model.from message
        let userid = Model.user'id $ Model.from $ message
        let chatid = Model.chat'id $ Model.chat $ message
        let messageid = Model.message_id message
        text <- Model.text message

        guard (userid == 415280808)
        word <- find (`T.isInfixOf` text) forbiddenWords
        let str = "Опять " `T.append` word `T.append` "..."
        pure [voidIO $ Api.requestSendMessage $ Argument.sendMessageWithReply chatid str messageid]
 
    forbiddenWords = ["раст", "Rust", "rust"]

handleStickerInfo :: Handler
handleStickerInfo = App.UpdateHandler $ \update -> do
    modify $ mapMode (const Start)
    pure (fromMaybe [] $ actions update)
    where
    actions :: Model.Update -> Maybe [Api.ApiAction]
    actions update = do
        message <- Model.message update
        sticker <- Model.sticker message
        let file = Model.file_id sticker
        let chatid = Model.chat'id $ Model.chat $ message
        let messageid = Model.message_id message

        let str = "file_id: " `T.append` file
        pure [voidIO $ Api.requestSendMessage $ Argument.sendMessageWithReply chatid str messageid]

handleAddEmoji :: Handler
handleAddEmoji = App.UpdateHandler $ \update ->
    fmap (fromMaybe []) $ runMaybeT (act update)
    where
    act :: Model.Update -> MaybeT (State AppState) [Api.ApiAction]
    act update = do
        message <- hoistMaybe $ Model.message update
        sticker <- hoistMaybe $ Model.sticker message
        let file = Model.file_id sticker
        let chatid = Model.chat'id $ Model.chat $ message
        let messageid = Model.message_id message

        state <- lift $ get
        let (WaitingAddEmoji emojitag) = as'mode state

        -- modify state
        lift $ modify (\state ->
            let persistence = as'persistence state in
            let emojiBindings = Persistence.p'emojiBindings persistence in
            state {
            as'mode = Start,
            as'persistence = persistence
                { Persistence.p'emojiBindings = Map.insert (T.pack emojitag) file emojiBindings
                }
            }
            )

        let str = "Emoji " `T.append` (T.pack emojitag) `T.append` " added!"
        pure [voidIO $ Api.requestSendMessage $ Argument.sendMessageWithReply chatid str messageid]
        

handleEmoji :: Handler
handleEmoji = App.UpdateHandler $ \update ->
    fmap (fromMaybe []) $ runMaybeT (act update)
    where
    act :: Model.Update -> MaybeT (State AppState) [Api.ApiAction]
    act update = do
        message <- hoistMaybe $ Model.message update
        let chatid = Model.chat'id $ Model.chat $ message
        let messageid = Model.message_id message
        text <- hoistMaybe $ Model.text message

        persistence <- lift $ gets as'persistence
        let emojiBindings = Persistence.p'emojiBindings persistence

        file <- hoistMaybe $ Map.lookup text emojiBindings

        pure [voidIO $ Api.requestSendSticker $ Argument.SendSticker chatid file]

handleCommand :: Handler
handleCommand  = App.UpdateHandler $ \update ->
    fmap (fromMaybe []) $ runMaybeT (act update)
    where
    act :: Model.Update -> MaybeT (State AppState) [Api.ApiAction]
    act update = do
        message <- hoistMaybe $ Model.message update
        text <- hoistMaybe $ Model.text message
        let chatid = Model.chat'id $ Model.chat $ message
        let messageid = Model.message_id message

        (newMode, str) <- hoistMaybe $ commandMapping text
        lift $ modify (mapMode $ const newMode)
        pure [voidIO $ Api.requestSendMessage $ Argument.sendMessageWithReply chatid str messageid]

commandMap :: Map.Map String (String -> (Mode, Text))
commandMap = Map.fromList
    [ ("/stickerinfo", \_ -> (WaitingStickerInfo, "Good, now send the sticker"))
    , ("/addemoji", \s -> (WaitingAddEmoji s, "Good, now send the sticker"))
    ]

commandMapping :: Text -> Maybe (Mode, Text)
commandMapping text =
    let (command, argument) = span (/= ' ') (T.unpack text) in
    let (command', argument') = (trimEnd '@' command, tail argument) in
    do
        f <- Map.lookup command' commandMap
        pure $ f argument'

mapText :: (String -> String) -> Text -> Text
mapText f = T.pack . f . T.unpack

handleText :: Handler
handleText = handleRust <> handleEmoji  <> handleCommand

-- Choose handler based on AppState
handler :: Handler
handler = App.UpdateHandler $ \update -> do
    mode <- gets as'mode
    case mode of
        Start -> App.runHandler handleText update
        WaitingStickerInfo -> App.runHandler handleStickerInfo update
        WaitingAddEmoji _ -> App.runHandler handleAddEmoji update

example2 :: IO ()
example2 = do
    -- Read token
    token <- E.trim <$> readFile "token.txt"
    putStrLn $ "Token: " ++ token
    -- Create api configuration
    api <- Api.new token

    -- Create a flag that indicates wheter iteration should continue
    continueEh <- newIORef True

    setKeyboardInterruptHandler (atomicWriteIORef continueEh False)

    let performIO = \state -> do
        { bool <- readIORef continueEh
        ; pure $ state { as'continueEh = bool }
        }

    let app = App.App api handler performIO as'continueEh

    persistence <- fromRight <$> Persistence.loadPersistence
    --let persistence = Persistence.Persistence Map.empty

    let startingState = AppState Start persistence True
    finalState <- App.loop app startingState

    Api.flush api
    Persistence.savePersistence $! as'persistence finalState

main :: IO ()
main = [example2] !! 0
