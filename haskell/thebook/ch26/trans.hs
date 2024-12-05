{-# LANGUAGE InstanceSigs #-}

module Transformers where

import Data.Maybe (fromMaybe)

newtype Identity a =
    Identity { runIdentity :: a } deriving (Show)

newtype MaybeT m a =
    MaybeT { runMaybeT :: m (Maybe a) }

instance (Functor m) => Functor (MaybeT m) where
    fmap f (MaybeT ma) = MaybeT $ (fmap . fmap) f ma

instance (Applicative m) => Applicative (MaybeT m) where
    pure x = MaybeT (pure (pure x))
    (MaybeT fab) <*> (MaybeT mma) = 
        MaybeT $ (<*>) <$> fab <*> mma

instance (Monad m) => Monad (MaybeT m) where
    return = pure
    (>>=) :: MaybeT m a
            -> (a -> MaybeT m b)
            -> MaybeT m b
    (MaybeT ma) >>= f =
        MaybeT $ do
            v <- ma
            case v of
                Nothing -> return Nothing
                Just y -> runMaybeT (f y)

newtype EitherT e m a =
    EitherT { runEitherT :: m (Either e a) }

instance Functor m => Functor (EitherT e m) where
    fmap f (EitherT em) = EitherT $ (fmap . fmap) f em

-- embedded :: EitherT String (IO) Int
-- embedded = return 1

-- instance (Applicative m) => Applicative (EitherT m) where
--     pure x = EitherT (pure (pure x))
--     (EitherT fab) <*> (EitherT mma) =
--         EitherT $ fab <*> mma

-- main :: IO()
-- main = do