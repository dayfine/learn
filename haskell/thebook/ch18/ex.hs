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

main = do
    let nope_trigger :: Nope (Int, String, Int)
        nope_trigger = undefined
    quickBatch $ functor nope_trigger
    quickBatch $ applicative nope_trigger
    quickBatch $ monad nope_trigger
