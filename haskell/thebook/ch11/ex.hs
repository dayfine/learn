import Data.Char

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

capitalizeWord :: String -> String
capitalizeWord (x : xs) = toUpper x : xs

capitalizeWords :: String -> [(String, String)]
capitalizeWords = map (\w@(x : xs) -> (w, capitalizeWord w)) . words

dropLeadingSpace :: String -> String
dropLeadingSpace = dropWhile isSpace

wordsWhen     :: (Char -> Bool) -> String -> [String]
wordsWhen p s =  case dropWhile p s of
                      "" -> []
                      s' -> w : wordsWhen p s''
                            where (w, s'') = break p s'

asSentence :: String -> String
asSentence s = s ++ "."

joinWith :: Char -> [String] -> String
joinWith c [] = ""
joinWith c [x] = x
joinWith c (x : xs) = x ++ [c] ++ joinWith c xs

capitalizeParagraph :: String -> String
capitalizeParagraph = joinWith ' ' . map go . wordsWhen (== '.')
    where go = asSentence . capitalizeWord . dropLeadingSpace
