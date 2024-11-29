{-# LANGUAGE InstanceSigs #-}

module Typeclass where

import Control.Applicative
import Test.QuickCheck
import Test.QuickCheck.Checkers
import Test.QuickCheck.Classes

newtype Moi s a =
    Moi { runMoi :: s -> (a, s) }

instance Functor (Moi s) where
    fmap :: (a -> b) -> Moi s a -> Moi s b
    fmap f (Moi g) = Moi (\s -> let (a, s') = g s in (f a, s'))

instance Applicative (Moi s) where
    pure :: a -> Moi s a
    pure a = Moi (\s -> (a, s))
    (<*>) :: Moi s (a -> b)
        -> Moi s a
        -> Moi s b
    (Moi f) <*> (Moi g) = 
        Moi (\s ->
                let (a, s1) = g s
                    (fn, s2) = f s1
                in (fn a, s2))

instance Monad (Moi s) where
    return = pure
    (>>=) :: Moi s a
        -> (a -> Moi s b)
        -> Moi s b
    (Moi f) >>= g =
         Moi (\s ->
                let (a, s1) = f s
                    m = g a
                in runMoi m s1)