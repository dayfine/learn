
f :: Show a => (a, b) -> IO (a, b) 
f t@(a, _) = do
  print a
  return t

doubleUp :: [a] -> [a]
doubleUp [] = []
doubleUp xs@(x:_) = x : xs


isSubseqOf :: (Eq a) => [a] -> [a] -> Bool
isSubseqOf [] _ = True
isSubseqOf _ [] = False
isSubseqOf a@(x : xs) b@(y : ys) =
    if x == y 
    then isSubseqOf xs ys
    else isSubseqOf a ys
