module Validation where

data Validation e a = 
    Failure e | Success a 
    deriving (Eq, Show)

instance Functor (Validation e) where 
    fmap f (Success a) = Success (f a)
    fmap _ (Failure e) = Failure e

instance Monoid e => Applicative (Validation e) where
    pure a = Success a
    Failure e <*> Failure e' = Failure (e <> e')
    Failure e <*> _ = Failure e
    _ <*> Failure e = Failure e
    Success f <*> Success a = Success (f a)
