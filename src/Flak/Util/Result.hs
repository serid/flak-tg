module Flak.Util.Result where

--import Data.Functor.Identity
import Control.Monad.Trans.Class (MonadTrans, lift)

data Result e t =
    Ok t |
    Err e

instance Functor (Result e) where
    fmap f (Ok t) = Ok $ f t
    fmap _ (Err e) = Err e

instance Applicative (Result e) where
    pure = Ok
    (Ok f) <*> m = fmap f m
    (Err e) <*> _ = Err e

instance Monad (Result e) where
    return = pure
    (Ok t) >>= k = k t
    (Err e) >>= _ = Err e

resultOk :: e -> Maybe a -> Result e a
resultOk e (Just a) = Ok a
resultOk e Nothing = Err e

newtype ResultT e m a = ResultT { runResultT :: m (Result e a) }

mapResultT :: (m (Result e a) -> n (Result e b)) -> ResultT e m a -> ResultT e n b
mapResultT f = ResultT . f . runResultT

hoistResult :: Monad m => (Result e a) -> ResultT e m a
hoistResult = ResultT . pure

instance MonadTrans (ResultT e) where
    -- lift :: m a -> ResultT e m a
    lift = ResultT . fmap Ok

instance Functor m => Functor (ResultT e m) where
    fmap f = mapResultT (fmap (fmap f))

instance Monad m => Applicative (ResultT e m) where
    pure = ResultT . pure . Ok

    mf <*> mx = ResultT $ do
        mfv <- runResultT mf
        case mfv of
            Err e -> pure $ Err e
            Ok ff -> do
                mxv <- runResultT mx
                -- pure $ fmap ff mxv
                pure $ case mxv of
                    Err e -> Err e
                    Ok t -> Ok $ ff t

instance Monad m => Monad (ResultT e m) where
    x >>= k = ResultT $ do
        v <- runResultT x
        case v of
            Err e -> pure $ Err e
            Ok t -> runResultT (k t)
