module ZipList where

import Control.Applicative
import Data.Monoid
import Test.QuickCheck
import Test.QuickCheck.Checkers
import Test.QuickCheck.Classes


newtype ZipList' a = ZipList' [a] deriving (Eq, Show)

instance Eq a => EqProp (ZipList' a) where
  xs =-= ys = xs' `eq` ys'
    where xs' = let (ZipList' l) = xs in take 3000 l
          ys' = let (ZipList' l) = ys in take 3000 l

instance Functor ZipList' where
    fmap f (ZipList' xs) = ZipList' $ fmap f xs

instance Applicative ZipList' where
    pure a = ZipList' $ repeat a
    (ZipList' xs) <*> (ZipList' ys) = ZipList' $ map (\(f, x) -> f x) (zip xs ys)

instance Arbitrary a => Arbitrary (ZipList' a) where
    arbitrary = ZipList' <$> arbitrary

type SSI = (String, String, Int)
l :: ZipList' SSI
l = undefined

main :: IO ()
main = quickBatch $ applicative l


