{-# LANGUAGE InstanceSigs #-}

module Ex where

import Test.Hspec

myLiftA2 :: Applicative f =>
    (a -> b -> c) -> f a -> f b -> f c
myLiftA2 f a b = f <$> a <*> b

newtype Reader r a =
  Reader { runReader :: r -> a }

ask :: Reader a a
ask = Reader $ id

asks :: (r -> a) -> Reader r a
asks f = Reader $ f

instance Functor (Reader r) where
  fmap :: (a -> b)
    -> Reader r a
    -> Reader r b
  fmap f (Reader ra) = Reader $ (f. ra)

instance Applicative (Reader r) where
  pure :: a -> Reader r a
  pure a = Reader $ \r -> a

  (<*>) :: Reader r (a -> b)
    -> Reader r a
    -> Reader r b
  (Reader rab) <*> (Reader ra) =
    Reader $ \r -> rab r $ ra r

instance Monad (Reader r) where
  return = pure
  (>>=) :: Reader r a
    -> (a -> Reader r b)
    -> Reader r b
  (Reader ra) >>= aRb = Reader $ \r -> runReader (aRb $ ra r) r


main :: IO ()
main = hspec $ do
  describe "liftA2" $ do
    it "Apply Optional" $ do
      myLiftA2 (,) (Just 3) (Just 5) `shouldBe` Just (3, 5)
    it "Apply List" $ do
      myLiftA2 (+) [1, 2, 3] [4, 5, 6] `shouldBe` [5,6,7,6,7,8,7,8,9]
