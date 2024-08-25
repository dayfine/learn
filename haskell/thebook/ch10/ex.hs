myOr :: [Bool] -> Bool
myOr = foldr (||) False

myAny :: (a -> Bool) -> [a] -> Bool
myAny f xs = myOr $ map f xs

myElem :: Eq a => a -> [a] -> Bool
myElem x xs = myAny (\x' -> x' == x) xs

myElem2 :: Eq a => a -> [a] -> Bool
myElem2 x xs = any (\x' -> x' == x) xs

myReverse :: [a] -> [a]
myReverse xs =
    foldr (\x acc -> concat [acc, [x]]) [] xs

myMap :: (a -> b) -> [a] -> [b]
myMap f = foldr (\x acc -> f x : acc) []

myFilter :: (a -> Bool) -> [a] -> [a]
myFilter f = foldr (\x acc -> if f x then x : acc else acc) []

squish :: [[a]] -> [a]
squish = foldr (\x acc -> concat [x, acc]) []

squishMap :: (a -> [b]) -> [a] -> [b]
squishMap f = foldr (\x acc -> concat [f x, acc]) []

squishAgain :: [[a]] -> [a]
squishAgain = squishMap id

myMaximumBy :: (a -> a -> Ordering) -> [a] -> a
myMaximumBy cmp xs = foldl' (\acc x -> if cmp acc x == GT then acc else x) (head xs) xs

myMinimumBy :: (a -> a -> Ordering) -> [a] -> a
myMinimumBy cmp xs = foldl' (\acc x -> if cmp acc x == LT then acc else x) (head xs) xs
