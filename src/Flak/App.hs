module Flak.App where

import qualified Flak.Api as Api
import qualified Flak.Model as Model
import qualified Flak.Argument as Argument

import qualified Data.Aeson.Encode.Pretty as P
import qualified Data.ByteString.Lazy.Char8 as B

import Control.Concurrent ( threadDelay )

import Flak.Util (iterateM'', ControlFlow (Continue, Break))

type StatefulHandler s = (s, Model.Update) -> (s, [Api.ApiAction])
type StatelessHandler = Model.Update -> [Api.ApiAction]

makeStateful :: StatelessHandler -> StatefulHandler s
makeStateful h = \(state, upd) -> (state, h upd)

newtype UpdateHandler s = UpdateHandler {
    runHandler :: (s, Model.Update) -> (s, [Api.ApiAction])
}

instance Semigroup (UpdateHandler s) where
    h1 <> h2 = UpdateHandler $ \(state, upd) ->
        let (state1, actions1) = runHandler h1 (state, upd) in
        let (state2, actions2) = runHandler h2 (state1, upd) in
        (state2, actions1 ++ actions2)

instance Monoid (UpdateHandler s) where
    mempty = UpdateHandler $ \(state, upd) -> (state, [])

data App s = App
    { app'api :: Api.Api
    , app'handler :: UpdateHandler s
    , app'performIO :: s -> IO s
    , app'continueEh :: s -> Bool
    }

-- example iterator
iterator :: App s -> s -> IO (ControlFlow s s)
iterator app state = do
    -- Receive updates
    let arg = Argument.GetUpdates { Argument.offset = Nothing
                                                    , Argument.limit = Nothing
                                                    , Argument.timeout = Nothing
                                                    , Argument.allowed_updates = Nothing
                                                    }
    upds <- Api.requestYankUpdates arg (app'api app)
    -- Print them
    putStrLn $ B.unpack $ P.encodePretty $ upds

    -- Compute new application state and actions
    let (newState, actions) = concatFoldUpdates (app'handler app) state (Model.result upds)

    -- Perform actions
    sequence $ map (Api.requestAction (app'api app)) $ actions

    -- Perform arbitrary IO as guided by app
    newNewState <- app'performIO app newState

    -- Sleep to throttle request rate
    putStrLn $ "Sleepin"
    threadDelay 1000000

    -- Ask the app whether iteration should continue
    if app'continueEh app newNewState
    then pure (Continue newNewState)
    else pure (Break newNewState)

-- Iterate through application states and handle updates
loop :: App s -> s -> IO s
loop app start = iterateM'' (iterator app) start

concatFoldUpdates :: UpdateHandler s -> s -> [Model.Update] -> (s, [Api.ApiAction])
concatFoldUpdates f start xs = foldl fun (start, []) xs
    where
        --fun :: (s, [Api.ApiAction]) -> Model.Update -> (s, [Api.ApiAction])
        fun (state, actions) upd =
            let (newState, moreActions) = runHandler f (state, upd) in
            (newState, actions ++ moreActions)
