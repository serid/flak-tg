{-# LANGUAGE ScopedTypeVariables #-}

module Flak.App where

--import Control.Monad.State.Lazy
import Control.Monad
import Control.Monad.Trans.State
import Control.Concurrent ( threadDelay )

import qualified Data.Aeson.Encode.Pretty as P
import qualified Data.ByteString.Lazy.Char8 as B

import qualified Flak.Api as Api
import qualified Flak.Model as Model
import qualified Flak.Argument as Argument
import Flak.Util (iterateM'', ControlFlow (Continue, Break))

{-
type StatefulHandler s = Model.Update -> State s [Api.ApiAction]
type StatelessHandler = Model.Update -> [Api.ApiAction]

makeStateful :: StatelessHandler -> StatefulHandler s
makeStateful h = \upd -> state $ \s -> (h upd, s)
-}

newtype UpdateHandler s = UpdateHandler {
    runHandler :: Model.Update -> State s [Api.ApiAction]
}

instance Semigroup (UpdateHandler s) where
    h1 <> h2 = UpdateHandler $ \upd -> do
        actions1 <- runHandler h1 upd
        actions2 <- runHandler h2 upd
        pure $ actions1 ++ actions2

instance Monoid (UpdateHandler s) where
    mempty = UpdateHandler $ \upd -> pure []

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
    let (actions, newState) = runState (concatFoldUpdates (app'handler app) (Model.result upds)) state

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

concatFoldUpdates :: forall s. UpdateHandler s -> [Model.Update] -> State s [Api.ApiAction]
concatFoldUpdates f upds = foldM folder [] upds
    where
        folder :: [Api.ApiAction] -> Model.Update -> State s [Api.ApiAction]
        folder accumulator upd = do
            actions <- runHandler f upd
            pure $ accumulator ++ actions
