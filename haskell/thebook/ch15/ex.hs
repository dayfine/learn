import Test.QuickCheck

semigroupAssoc :: (Eq m, Semigroup m)
               => m -> m -> m -> Bool

semigroupAssoc a b c =
    (a <> (b <> c)) == ((a <> b) <> c)

monoidLeftIdentity :: (Eq m, Monoid m) => m -> Bool
monoidLeftIdentity a = (mempty <> a) == a

monoidRightIdentity :: (Eq m, Monoid m) => m -> Bool
monoidRightIdentity a = (a <> mempty) == a

-- Trivial 
data Trivial = Trivial deriving (Eq, Show)

instance Semigroup Trivial where
  _ <> _ = Trivial

instance Monoid Trivial where
  mempty = Trivial
  mappend = (<>)

instance Arbitrary Trivial where
  arbitrary = return Trivial

type TrivAssoc =
    Trivial -> Trivial -> Trivial -> Bool

-- Identity
newtype Identity a = Identity a deriving (Eq, Show)

identityGen :: Arbitrary a => Gen (Identity a)
identityGen = do
  a <- arbitrary
  return (Identity a)

instance Arbitrary a => Arbitrary (Identity a) where
  arbitrary = identityGen

instance Semigroup a => Semigroup (Identity a) where
  Identity x <> Identity y = Identity (x <> y)

instance Monoid a => Monoid (Identity a) where
  mempty = Identity mempty
  mappend = (<>)

type IdentityAssoc a =
    Identity a -> Identity a -> Identity a -> Bool

-- Two
data Two a b = Two a b deriving (Eq, Show)

twoGen :: (Arbitrary a, Arbitrary b) => Gen (Two a b)
twoGen = do
  a <- arbitrary
  b <- arbitrary
  return (Two a b)

instance (Arbitrary a, Arbitrary b) => Arbitrary (Two a b) where
  arbitrary = twoGen

instance (Semigroup a, Semigroup b) => Semigroup (Two a b) where
  Two x y <> Two x' y' = Two (x <> x') (y <> y')

instance (Monoid a, Monoid b) => Monoid (Two a b) where
  mempty = Two mempty mempty
  mappend = (<>)

type TwoAssoc a b =
    Two a b -> Two a b -> Two a b -> Bool

-- Three
data Three a b c = Three a b c deriving (Eq, Show)

threeGen :: (Arbitrary a, Arbitrary b, Arbitrary c) => Gen (Three a b c)
threeGen = do
  a <- arbitrary
  b <- arbitrary
  c <- arbitrary
  return (Three a b c)

instance (Arbitrary a, Arbitrary b, Arbitrary c) => Arbitrary (Three a b c) where
  arbitrary = threeGen

instance (Semigroup a, Semigroup b, Semigroup c) => Semigroup (Three a b c) where
  Three x y z <> Three x' y' z' = Three (x <> x') (y <> y') (z <> z')

instance (Monoid a, Monoid b, Monoid c) => Monoid (Three a b c) where
  mempty = Three mempty mempty mempty
  mappend = (<>)

type ThreeAssoc a b c =
    Three a b c -> Three a b c -> Three a b c -> Bool

-- Four
data Four a b c d = Four a b c d deriving (Eq, Show)

fourGen :: (Arbitrary a, Arbitrary b, Arbitrary c, Arbitrary d) => Gen (Four a b c d)
fourGen = do
  a <- arbitrary
  b <- arbitrary
  c <- arbitrary
  d <- arbitrary
  return (Four a b c d)

instance (Arbitrary a, Arbitrary b, Arbitrary c, Arbitrary d) => Arbitrary (Four a b c d) where
  arbitrary = fourGen

instance (Semigroup a, Semigroup b, Semigroup c, Semigroup d) => Semigroup (Four a b c d) where
  Four w x y z <> Four w' x' y' z' = Four (w <> w') (x <> x') (y <> y') (z <> z')

instance (Monoid a, Monoid b, Monoid c, Monoid d) => Monoid (Four a b c d) where
  mempty = Four mempty mempty mempty mempty
  mappend = (<>)

type FourAssoc a b c d =
    Four a b c d -> Four a b c d -> Four a b c d -> Bool

-- BoolConj
newtype BoolConj = BoolConj Bool deriving (Eq, Show)

boolConjGen :: Gen BoolConj
boolConjGen = do
  b <- choose (False, True) 
  return (BoolConj b)

instance Arbitrary BoolConj where
  arbitrary = boolConjGen

instance Semigroup BoolConj where
  BoolConj x <> BoolConj y = BoolConj (x && y)

instance Monoid BoolConj where
  mempty = BoolConj True
  mappend = (<>)

prop_bool_conj :: BoolConj -> BoolConj -> Bool
prop_bool_conj (BoolConj x) (BoolConj y) 
  = (BoolConj x) <> (BoolConj y) == BoolConj (x && y)

type BoolConjAssoc =
    BoolConj -> BoolConj -> BoolConj -> Bool

-- BoolDisj
newtype BoolDisj = BoolDisj Bool deriving (Eq, Show)

boolDisjGen :: Gen BoolDisj
boolDisjGen = do
  b <- choose (False, True) 
  return (BoolDisj b)

instance Arbitrary BoolDisj where
  arbitrary = boolDisjGen

instance Semigroup BoolDisj where
  BoolDisj x <> BoolDisj y = BoolDisj (x || y)

instance Monoid BoolDisj where
  mempty = BoolDisj False
  mappend = (<>)

prop_bool_disj :: BoolDisj -> BoolDisj -> Bool
prop_bool_disj (BoolDisj x) (BoolDisj y) 
  = (BoolDisj x) <> (BoolDisj y) == BoolDisj (x || y)

type BoolDisjAssoc =
    BoolDisj -> BoolDisj -> BoolDisj -> Bool
  
-- Or
data Or a b = Fst a | Snd b deriving (Eq, Show)

orGen :: (Arbitrary a, Arbitrary b) => Gen (Or a b)
orGen = do
  a <- arbitrary
  b <- arbitrary
  oneof [return $ Fst a, return $ Snd b]

instance (Arbitrary a, Arbitrary b) => Arbitrary (Or a b) where
  arbitrary = orGen

instance Semigroup (Or a b) where
  Fst _ <> x = x
  Snd x <> _ = Snd x

type OrAssoc a b =
    Or a b -> Or a b -> Or a b -> Bool

-- Combine
newtype Combine a b = Combine { unCombine :: (a -> b) }

instance Show (Combine a b) where
  show _ = "Combine a b"

instance Arbitrary b => Arbitrary (Combine a b) where
  arbitrary = do
    x <- arbitrary
    return (Combine (\_ -> x))

instance Semigroup b => Semigroup (Combine a b) where
  Combine{unCombine = f} <> Combine{unCombine = g} = 
    Combine{unCombine = \x -> f x <> g x}

instance Monoid b => Monoid (Combine a b) where
  mempty = Combine (\_ -> mempty)
  mappend = (<>)

combineSemiGroupAssoc :: (Eq b, Semigroup b)
               => a -> Combine a b -> Combine a b -> Combine a b -> Bool
combineSemiGroupAssoc x c1 c2 c3 = 
    semigroupAssoc (unCombine c1 $ x) (unCombine c2 $ x) (unCombine c3 $ x)

type CombineAssoc a b =
   a -> Combine a b -> Combine a b -> Combine a b -> Bool

-- Comp
newtype Comp a = Comp { unComp :: (a -> a) }

instance Semigroup (Comp a) where
  Comp{unComp = f} <> Comp{unComp = g} = 
    Comp{unComp = f . g}

instance Monoid (Comp a) where
  mempty = Comp id
  mappend = (<>)

-- Validation
data Validation a b = Failure' a | Success' b deriving (Eq, Show)

instance Semigroup a => Semigroup (Validation a b) where
  Failure' x <> Failure' y = Failure' (x <> y)
  Failure' _ <> Success' y = Success' y
  Success' x <> _ = Success' x

-- Mem
newtype Mem s a = Mem { runMem :: s -> (a, s) }

instance Semigroup a => Semigroup (Mem s a) where
  Mem f <> Mem g = Mem $ \s -> 
    let (a, s') = f s
        (a', s'') = g s'
    in (a <> a', s'')

instance Monoid a => Monoid (Mem s a) where
  mempty = Mem $ \s -> (mempty, s)
  mappend = (<>)

main :: IO ()
main = do
    quickCheck (semigroupAssoc :: TrivAssoc)
    quickCheck (monoidLeftIdentity :: Trivial -> Bool)
    quickCheck (monoidRightIdentity :: Trivial -> Bool)
    quickCheck (semigroupAssoc :: IdentityAssoc String)
    quickCheck (monoidLeftIdentity :: Identity String -> Bool)
    quickCheck (monoidRightIdentity :: Identity String -> Bool)
    quickCheck (semigroupAssoc :: TwoAssoc String String)
    quickCheck (monoidLeftIdentity :: Two String String -> Bool)
    quickCheck (monoidRightIdentity :: Two String String -> Bool)
    quickCheck (semigroupAssoc :: ThreeAssoc String String String)
    quickCheck (monoidLeftIdentity :: Three String String String -> Bool)
    quickCheck (monoidRightIdentity :: Three String String String -> Bool)
    quickCheck (semigroupAssoc :: FourAssoc String String String String)
    quickCheck (monoidLeftIdentity :: Four String String String String -> Bool)
    quickCheck (monoidRightIdentity :: Four String String String String -> Bool)
    quickCheck (semigroupAssoc :: BoolConjAssoc)
    quickCheck prop_bool_conj
    quickCheck (monoidLeftIdentity :: BoolConj -> Bool)
    quickCheck (monoidRightIdentity :: BoolConj -> Bool)
    quickCheck (semigroupAssoc :: BoolDisjAssoc)
    quickCheck prop_bool_disj
    quickCheck (monoidLeftIdentity :: BoolDisj -> Bool)
    quickCheck (monoidRightIdentity :: BoolDisj -> Bool)
    quickCheck (semigroupAssoc :: OrAssoc String String)
    quickCheck (semigroupAssoc :: OrAssoc Integer String)
    quickCheck (semigroupAssoc :: OrAssoc String Integer)
    quickCheck (combineSemiGroupAssoc :: CombineAssoc String String)

    let failure :: String -> Validation String Int
        failure = Failure'
        success :: Int -> Validation String Int
        success = Success'
    print $ success 1 <> failure "blah"
    print $ failure "woot" <> failure "blah"
    print $ success 1 <> success 2
    print $ failure "woot" <> success 2
