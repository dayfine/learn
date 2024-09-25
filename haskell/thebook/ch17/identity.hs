module Identityentity where

newtype Identity a = Identity a
    deriving (Eq, Ord, Show) 

instance Functor Identity where
    fmap f (Identity a) = Identity (f a)

instance Applicative Identity where
    pure = Identity
    (<*>) :: Identity (a -> b) -> Identity a -> Identity b
    (<*>) (Identity f) (Identity a) = Identity (f a)
