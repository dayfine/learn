myOr :: [Bool] -> Bool
myOr xs = foldr (||) False xs

myAny :: (a -> Bool) -> [a] -> Bool
myAny f xs = myOr $ map f xs

myElem :: Eq a => a -> [a] -> Bool
myElem x xs = myAny (\x' -> x' == x) xs

myElem2 :: Eq a => a -> [a] -> Bool
myElem2 x xs = any (\x' -> x' == x) xs