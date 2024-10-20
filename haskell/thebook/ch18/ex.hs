module Ex where

import Test.QuickCheck
import Test.QuickCheck.Checkers
import Test.QuickCheck.Classes

data Nope a = NopeDotJpg deriving (Eq, Show)

instance Functor Nope where
    fmap f NopeDotJpg = NopeDotJpg

instance Applicative Nope where
    pure = \x -> NopeDotJpg
    NopeDotJpg <*> NopeDotJpg = NopeDotJpg

instance Monad Nope where
    return = pure
    NopeDotJpg >>= f = NopeDotJpg

instance Arbitrary a => Arbitrary (Nope a) where
    arbitrary = return NopeDotJpg

instance Eq a => EqProp (Nope a) where 
    (=-=) = eq

data BahEither b a = PLeft a | PRight b deriving (Eq, Show)

instance Functor (BahEither b) where
    fmap f (PLeft a) = PLeft (f a)
    fmap _ (PRight b) = PRight b

instance Applicative (BahEither b) where
    pure = PLeft
    PLeft f <*> PLeft a = PLeft (f a)
    PRight b <*> _ = PRight b
    _ <*> PRight b = PRight b

instance Monad (BahEither b) where
    return = pure
    PLeft a >>= f = f a
    PRight b >>= _ = PRight b

instance (Arbitrary a, Arbitrary b) => Arbitrary (BahEither b a) where
    arbitrary = do
        a <- arbitrary
        b <- arbitrary
        elements [PLeft a, PRight b]

instance (Eq a, Eq b) => EqProp (BahEither b a) where
    (=-=) = eq

newtype Identity a = Identity a deriving (Eq, Ord, Show)

instance Functor Identity where 
    fmap f (Identity a) = Identity (f a)

instance Applicative Identity where
    pure = Identity
    Identity f <*> Identity a = Identity (f a)

instance Monad Identity where
    return = pure
    Identity a >>= f = f a

instance Arbitrary a => Arbitrary (Identity a) where
    arbitrary = do
        a <- arbitrary
        return (Identity a)

instance Eq a => EqProp (Identity a) where 
    (=-=) = eq

data List a = Nil | Cons a (List a)
    deriving (Eq, Show)

instance Eq a => EqProp (List a) where
    (=-=) = eq

instance Functor List where
    fmap f Nil = Nil
    fmap f (Cons a as) = Cons (f a) (fmap f as)

append :: List a -> List a -> List a
append Nil ys = ys
append (Cons x xs) ys = Cons x $ xs `append` ys

fold :: (a -> b -> b) -> b -> List a -> b
fold _ b Nil = b
fold f b (Cons h t) = f h (fold f b t)

concat' :: List (List a) -> List a
concat' = fold append Nil

instance Applicative List where
    pure a = Cons a Nil
    fs <*> as = concat' $ fmap (\f -> fmap f as) fs

instance Monad List where
    return = pure
    Nil >>= f = Nil
    Cons a as >>= f = f a `append` (as >>= f)

instance Arbitrary a => Arbitrary (List a) where
    arbitrary = do
        a <- arbitrary
        b <- arbitrary
        frequency [(1, return Nil), (3, return (Cons a b))]

main = do
    let nope_trigger :: Nope (Int, String, Int)
        nope_trigger = undefined
    quickBatch $ functor nope_trigger
    quickBatch $ applicative nope_trigger
    quickBatch $ monad nope_trigger
    let either_trigger :: BahEither String (Int, String, Int)
        either_trigger = undefined
    quickBatch $ functor either_trigger
    quickBatch $ applicative either_trigger
    quickBatch $ monad either_trigger
    let id_trigger :: Identity (Int, String, Int)
        id_trigger = undefined
    quickBatch $ functor id_trigger
    quickBatch $ applicative id_trigger
    quickBatch $ monad id_trigger
    let list_trigger :: List (Int, String, Int)
        list_trigger = undefined
    quickBatch $ functor list_trigger
    quickBatch $ applicative list_trigger
    quickBatch $ monad list_trigger
