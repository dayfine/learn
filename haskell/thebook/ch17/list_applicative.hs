module List where

import Control.Applicative
import Data.Monoid
import Test.QuickCheck
import Test.QuickCheck.Checkers
import Test.QuickCheck.Classes

data List a = Nil | Cons a (List a)
    deriving (Eq, Show)

instance Eq a => EqProp (List a) where
    (=-=) = eq

append :: List a -> List a -> List a
append Nil ys = ys
append (Cons x xs) ys = Cons x $ xs `append` ys

fold :: (a -> b -> b) -> b -> List a -> b
fold _ b Nil = b
fold f b (Cons h t) = f h (fold f b t)

concat' :: List (List a) -> List a
concat' = fold append Nil

flatMap :: (a -> List b) -> List a -> List b
flatMap f as = concat' $ fmap f as

instance Functor List where
    fmap f Nil = Nil
    fmap f (Cons a as) = Cons (f a) (fmap f as)

instance Applicative List where
    pure a = Cons a Nil
    Nil <*> _ = Nil
    _ <*> Nil = Nil
    fs <*> as = flatMap (\f -> fmap f as) fs

instance Arbitrary a => Arbitrary (List a) where
    arbitrary = do
        a <- arbitrary
        b <- arbitrary
        frequency [(1, return Nil), (3, return (Cons a b))]

type SSI = (String, String, Int)
l :: List SSI
l = undefined

main :: IO ()
main = quickBatch $ applicative l
