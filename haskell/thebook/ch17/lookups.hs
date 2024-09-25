module Lookups where

import Data.List (elemIndex)

added :: Maybe Integer
added =
    (+3) <$> (lookup 3 $ zip [1, 2, 3] [4, 5, 6])

y :: Maybe Integer
y = lookup 3 $ zip [1, 2, 3] [4, 5, 6]

z :: Maybe Integer
z = lookup 2 $ zip [1, 2, 3] [4, 5, 6]

tupled :: Maybe (Integer, Integer)
tupled = (,) <$> y <*> z
tupled' :: Maybe (Integer, Integer)
tupled' = liftA2 (,) y z

a :: Maybe Int
a = elemIndex 3 [1, 2, 3, 4, 5]
b :: Maybe Int
b = elemIndex 4 [1, 2, 3, 4, 5]
max' :: Int -> Int -> Int
max' = max
maxed :: Maybe Int
maxed = max' <$> a <*> b
maxed' :: Maybe Int
maxed' = liftA2 max' a b

xs = [1, 2, 3]
ys = [4, 5, 6]
x1 :: Maybe Integer
x1 = lookup 3 $ zip xs ys
y1 :: Maybe Integer
y1 = lookup 2 $ zip xs ys
summed :: Maybe Integer
summed = (+) <$> x1 <*> y1
