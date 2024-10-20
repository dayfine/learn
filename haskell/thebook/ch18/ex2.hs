module Ex2 where

import Test.Hspec

j :: Monad m => m (m a) -> m a
j m = m >>= id

l1 :: Monad m => (a -> b) -> m a -> m b
l1 f m = f <$> m

l2 :: Monad m => (a -> b -> c) -> m a -> m b -> m c
l2 f m1 m2 = f <$> m1 <*> m2

a :: Monad m => m a -> m (a -> b) -> m b
a = flip (<*>)

meh :: Monad m => [a] -> (a -> m b) -> m [b]
meh as f = flipType $ f <$> as

flipType :: (Monad m) => [m a] -> m [a]
flipType = foldr (l2 (:)) (return [])

main :: IO ()
main = hspec $ do
    describe "j" $ do
        it "joins list of lists" $ do
            j [[1, 2], [], [3]] `shouldBe` [1,2,3]
        it "flattens structure" $ do
            j (Just (Just 1)) `shouldBe` Just 1
        it "flattens structure of zero value" $ do
            j (Just (Nothing :: Maybe Int)) `shouldBe` (Nothing :: Maybe Int)
        it "keeps values untouched" $ do
            j Nothing `shouldBe` (Nothing :: Maybe Int)
