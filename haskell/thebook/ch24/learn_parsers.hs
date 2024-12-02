{-# LANGUAGE TypeApplications #-}

module LearnParsers where

import Control.Applicative
import Text.Trifecta

stop :: Parser a
stop = unexpected "stop"

one = char '1'

one' = one >> stop

oneTwo = char '1' >> char '2'
oneTwo' = oneTwo >> stop

str123 = string "123" <|> string "12" <|> string "1" 

stringWithChar :: String -> Parser Char
stringWithChar [] = stop
stringWithChar (x:[]) = char x
stringWithChar (x:xs) = char x >> (stringWithChar xs)

testParseWith :: forall a. Show a => Parser a -> String -> IO ()
testParseWith p s = print $ parseString p mempty s

testParse :: forall a. Show a => Parser a -> IO ()
testParse p = testParseWith p "123"


pNL s = putStrLn ('\n' : s)
main = do
    pNL "stop:"
    testParse @Char stop

    pNL "one:"
    testParse @Char one
    pNL "one with EOF:"
    testParse @() (one >> eof)

    pNL "one':"
    testParse @Char one'

    pNL "oneTwo:"
    testParse @Char oneTwo

    pNL "oneTwo':"
    testParse @Char oneTwo'

    pNL "string 123-1':"
    testParseWith @String str123 "1"
    pNL "string 123-12':"
    testParseWith @String str123 "12"
    pNL "string 123-123':"
    testParse @String (str123)

    pNL "string with char"
    testParse @Char (stringWithChar "123")