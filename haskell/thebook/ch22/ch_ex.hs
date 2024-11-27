module ReaderPractice where

import Control.Applicative
import Data.Maybe

x = [1, 2, 3]
y = [4, 5, 6]
z = [7, 8, 9]

xs :: Maybe Integer
xs = lookup 3 $ zip x y

ys :: Maybe Integer
ys = lookup 6 $ zip y z

zs :: Maybe Integer
zs = lookup 4 $ zip x y

z' :: Integer -> Maybe Integer
z' k = lookup k $ zip x z

x1 :: Maybe (Integer, Integer)
x1 = (,) <$> xs <*> ys

x2 :: Maybe (Integer, Integer)
x2 = (,) <$> ys <*> zs

x3 :: Integer -> (Maybe Integer, Maybe Integer)
x3 k = (,) (z' k) (z' k)

_uncurry :: (a -> b -> c) -> (a, b) -> c
_uncurry f (a, b) = f a b

summed :: Num c => (c, c) -> c
summed = _uncurry (+)

bolt :: Integer -> Bool
-- use &&, >3, <8
bolt = (&&) <$> (>3) <*> (<8)

_fromMaybe :: a -> Maybe a -> a
_fromMaybe d Nothing = d
_fromMaybe _ (Just x) = x

sequA :: Integral a => a -> [Bool]
sequA m = sequenceA [(>3), (<8), even] m

s' = summed <$> ((,) <$> xs <*> ys)

main :: IO ()
main = do
    print $ sequA 7 
    print $ sequA 11
    print $ and $ sequA 7 
    print $ or $ sequA 11
    print $ fmap sequA s'
    print $ sequA $ _fromMaybe 0 s'
    print $ bolt $ _fromMaybe 0 ys
