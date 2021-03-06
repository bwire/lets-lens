{-# LANGUAGE RankNTypes #-}

module Lets.Lens (
  fmapT
, over
, fmapTAgain
, Set
, sets
, mapped
, set
, foldMapT
, foldMapOf
, foldMapTAgain
, Fold
, folds
, folded
, Get
, get
, Traversal
, both
, traverseLeft
, traverseRight
, Traversal'
, Lens
, Prism
, _Left
, _Right
, prism
, _Just
, _Nothing
, setP
, getP
, Prism'
, modify
, (%~)
, (.~)
, fmodify
, (|=)
, fstL
, sndL
, mapL
, setL
, compose
, (|.)
, identity
, product
, (***)
, choice
, (|||)
, Lens'
, cityL
, stateL
, countryL
, streetL
, suburbL
, localityL
, ageL
, nameL
, addressL
, intAndIntL
, intAndL
, getSuburb
, setStreet
, getAgeAndCountry
, setCityAndLocality
, getSuburbOrCity
, setStreetOrState
, modifyCityUppercase
, modifyIntAndLengthEven
, traverseLocality
, intOrIntP
, intOrP
, intOrLengthEven
) where

import Control.Applicative(Applicative((<*>), pure))
import Data.Char(toUpper)
import Data.Foldable(Foldable(foldMap))
import Data.Functor((<$>))
import Data.Map(Map)
import qualified Data.Map as Map(insert, delete, lookup)
import Data.Monoid(Monoid)
import qualified Data.Set as Set(Set, insert, delete, member)
import Data.Traversable(Traversable(traverse))
import Lets.Data(AlongsideLeft(AlongsideLeft, getAlongsideLeft), AlongsideRight(AlongsideRight, getAlongsideRight), Identity(Identity, getIdentity), Const(Const, getConst), Tagged(Tagged, getTagged), IntOr(IntOrIs, IntOrIsNot), IntAnd(IntAnd), Person(Person), Locality(Locality), Address(Address), bool)
import Lets.Choice(Choice(left, right))
import Lets.Profunctor(Profunctor(dimap))
import Prelude hiding (product)

import Data.Function (on)

-- $setup
-- >>> import qualified Data.Map as Map(fromList)
-- >>> import qualified Data.Set as Set(fromList)
-- >>> import Data.Char(ord)
-- >>> import Lets.Data

-- Let's remind ourselves of Traversable, noting Foldable and Functor.
--
-- class (Foldable t, Functor t) => Traversable t where
--   traverse ::
--     Applicative f => 
--     (a -> f b)
--     -> t a
--     -> f (t b)

-- | Observe that @fmap@ can be recovered from @traverse@ using @Identity@.
--
-- /Reminder:/ fmap :: Functor t => (a -> b) -> t a -> t b
fmapT :: Traversable t => (a -> b) -> t a -> t b
fmapT f = getIdentity . traverse (Identity . f)

-- | Let's refactor out the call to @traverse@ as an argument to @fmapT@.
over :: ((a -> Identity b) -> s -> Identity t) -> (a -> b) -> s -> t
over t f = getIdentity . t (Identity . f)

-- | Here is @fmapT@ again, passing @traverse@ to @over@.
fmapTAgain :: Traversable t => (a -> b) -> t a -> t b
fmapTAgain = over traverse 

-- | Let's create a type-alias for this type of function.
type Set s t a b = (a -> Identity b) -> s -> Identity t

-- | Let's write an inverse to @over@ that does the @Identity@ wrapping &
-- unwrapping.
sets :: ((a -> b) -> s -> t) -> Set s t a b 
sets ul = \f -> Identity . ul (getIdentity . f) 

mapped :: Functor f => Set (f a) (f b) a b 
mapped = sets fmap

set :: Set s t a b -> s -> b -> t
set l s = getIdentity . flip l s . const . Identity


-- | Observe that @foldMap@ can be recovered from @traverse@ using @Const@.
--
-- /Reminder:/ foldMap :: (Foldable t, Monoid b) => (a -> b) -> t a -> b
foldMapT :: (Traversable t, Monoid b) => (a -> b) -> t a -> b
foldMapT f = getConst . traverse (Const . f)

-- | Let's refactor out the call to @traverse@ as an argument to @foldMapT@.
foldMapOf :: ((a -> Const r b) -> s -> Const r t) -> (a -> r) -> s -> r
foldMapOf l f = getConst . l (Const . f)

-- | Here is @foldMapT@ again, passing @traverse@ to @foldMapOf@.
foldMapTAgain :: (Traversable t, Monoid b) => (a -> b) -> t a -> b
foldMapTAgain = foldMapOf traverse

-- | Let's create a type-alias for this type of function.
type Fold s t a b = forall r. Monoid r => (a -> Const r b) -> s -> Const r t

-- | Let's write an inverse to @foldMapOf@ that does the @Const@ wrapping &
-- unwrapping.
folds :: ((a -> b) -> s -> t) -> (a -> Const b a) -> s -> Const t s
folds ul = \f -> Const . ul (getConst . f)

folded :: Foldable f => Fold (f a) (f a) a a
folded = folds foldMap

-- | @Get@ is like @Fold@, but without the @Monoid@ constraint.
type Get r s a = (a -> Const r a) -> s -> Const r s

get :: Get a s a -> s -> a
get l = getConst . l Const

----
-- | Let's generalise @Identity@ and @Const r@ to any @Applicative@ instance.
type Traversal s t a b = forall f. Applicative f => (a -> f b) -> s -> f t

-- | Traverse both sides of a pair.
both :: Traversal (a, a) (b, b) a b
both f (a, b) = (,) <$> f a <*> f b

-- | Traverse the left side of @Either@.
traverseLeft :: Traversal (Either a x) (Either b x) a b
traverseLeft f (Left v) = Left <$> f v
traverseLeft _ (Right v) = pure (Right v)

-- | Traverse the right side of @Either@.
traverseRight :: Traversal (Either x a) (Either x b) a b
traverseRight f (Right v) = Right <$> f v
traverseRight _ (Left v) = pure (Left v)

type Traversal' a b = Traversal a a b b

-- | @Const r@ is @Applicative@, if @Monoid r@, however, without the @Monoid@
-- constraint (as in @Get@), the only shared abstraction between @Identity@ and
-- @Const r@ is @Functor@.
--
-- Consequently, we arrive at our lens derivation:
type Lens s t a b = forall f. Functor f => (a -> f b) -> s -> f t

-- | A prism is a less specific type of traversal.
type Prism s t a b = forall p f. (Choice p, Applicative f) => p a (f b) -> p s (f t)

prism :: (b -> t) -> (s -> Either t a) -> Prism s t a b
prism bt sa = dimap sa (either pure (fmap bt)) . right

_Left :: Prism (Either a x) (Either b x) a b
_Left = prism Left (either Right (Left . Right))

_Right :: Prism (Either x a) (Either x b) a b 
_Right = prism Right (either (Left . Left) Right)

_Just :: Prism (Maybe a) (Maybe b) a b
_Just = prism Just (maybe (Left Nothing) Right)

_Nothing :: Prism (Maybe a) (Maybe a) () ()
_Nothing = prism (const Nothing) Left

getP :: Prism s t a b -> b -> t
getP p = getIdentity . getTagged . p . Tagged . Identity

setP :: Monoid s => Prism s t a b -> s -> Either t a
setP p = either Right Left . p Left 

type Prism' a b = Prism a a b b

-- |
--
-- >>> modify fstL (+1) (0 :: Int, "abc")
-- (1,"abc")
--
-- >>> modify sndL (+1) ("abc", 0 :: Int)
-- ("abc",1)
--
-- prop> let types = (x :: Int, y :: String) in modify fstL id (x, y) == (x, y)
--
-- prop> let types = (x :: Int, y :: String) in modify sndL id (x, y) == (x, y)
modify :: Lens s t a b -> (a -> b) -> s -> t
modify l f = getIdentity . l (Identity . f)

-- | An alias for @modify@.
(%~) :: Lens s t a b -> (a -> b) -> s -> t
(%~) = modify

infixr 4 %~

-- |
--
-- >>> fstL .~ 1 $ (0 :: Int, "abc")
-- (1,"abc")
--
-- >>> sndL .~ 1 $ ("abc", 0 :: Int)
-- ("abc",1)
--
-- prop> let types = (x :: Int, y :: String) in set fstL (x, y) z == (fstL .~ z $ (x, y))
--
-- prop> let types = (x :: Int, y :: String) in set sndL (x, y) z == (sndL .~ z $ (x, y))
(.~) :: Lens s t a b -> b -> s -> t
(.~) l v = getIdentity . l (Identity . const v)

infixl 5 .~

-- |
--
-- >>> fmodify fstL (+) (5 :: Int, "abc") 8
-- (13,"abc")
--
-- >>> fmodify fstL (\n -> bool Nothing (Just (n * 2)) (even n)) (10, "abc")
-- Just (20,"abc")
--
-- >>> fmodify fstL (\n -> bool Nothing (Just (n * 2)) (even n)) (11, "abc")
-- Nothing
fmodify :: Functor f => Lens s t a b -> (a -> f b) -> s -> f t 
fmodify = ($)

-- |
--
-- >>> fstL |= Just 3 $ (7, "abc")
-- Just (3,"abc")
--
-- >>> (fstL |= (+1) $ (3, "abc")) 17
-- (18,"abc")
(|=) :: Functor f => Lens s t a b -> f b -> s -> f t
(|=) l = l . const
  

infixl 5 |=

-- |
--
-- >>> modify fstL (*10) (3, "abc")
-- (30,"abc")
fstL :: Lens (a, x) (b, x) a b
fstL = \f (a, b) -> flip (,) b <$> f a

-- |
--
-- >>> modify sndL (++ "def") (13, "abc")
-- (13,"abcdef")
sndL :: Lens (x, a) (x, b) a b
sndL = \f (a, b) -> (,) a <$> f b

-- |
--
-- To work on `Map k a`:
--   Map.lookup :: Ord k => k -> Map k a -> Maybe a
--   Map.insert :: Ord k => k -> a -> Map k a -> Map k a
--   Map.delete :: Ord k => k -> Map k a -> Map k a
--
-- >>> get (mapL 3) (Map.fromList (map (\c -> (ord c - 96, c)) ['a'..'d']))
-- Just 'c'
--
-- >>> get (mapL 33) (Map.fromList (map (\c -> (ord c - 96, c)) ['a'..'d']))
-- Nothing
--
-- >>> set (mapL 3) (Map.fromList (map (\c -> (ord c - 96, c)) ['a'..'d'])) (Just 'X')
-- fromList [(1,'a'),(2,'b'),(3,'X'),(4,'d')]
--
-- >>> set (mapL 33) (Map.fromList (map (\c -> (ord c - 96, c)) ['a'..'d'])) (Just 'X')
-- fromList [(1,'a'),(2,'b'),(3,'c'),(4,'d'),(33,'X')]
--
-- >>> set (mapL 3) (Map.fromList (map (\c -> (ord c - 96, c)) ['a'..'d'])) Nothing
-- fromList [(1,'a'),(2,'b'),(4,'d')]
--
-- >>> set (mapL 33) (Map.fromList (map (\c -> (ord c - 96, c)) ['a'..'d'])) Nothing
-- fromList [(1,'a'),(2,'b'),(3,'c'),(4,'d')]
mapL :: Ord k => k -> Lens (Map k v) (Map k v) (Maybe v) (Maybe v)
mapL k = \f m -> 
  let f' v = case v of
        Just v' -> Map.insert k v' m
        _       -> Map.delete k m
  in f' <$> f (Map.lookup k m)
  
-- |
--
-- To work on `Set a`:
--   Set.insert :: Ord a => a -> Set a -> Set a
--   Set.member :: Ord a => a -> Set a -> Bool
--   Set.delete :: Ord a => a -> Set a -> Set a
--
-- >>> get (setL 3) (Set.fromList [1..5])
-- True
--
-- >>> get (setL 33) (Set.fromList [1..5])
-- False
--
-- >>> set (setL 3) (Set.fromList [1..5]) True
-- fromList [1,2,3,4,5]
--
-- >>> set (setL 3) (Set.fromList [1..5]) False
-- fromList [1,2,4,5]
--
-- >>> set (setL 33) (Set.fromList [1..5]) True
-- fromList [1,2,3,4,5,33]
--
-- >>> set (setL 33) (Set.fromList [1..5]) False
-- fromList [1,2,3,4,5]
setL :: Ord k => k -> Lens (Set.Set k) (Set.Set k) Bool Bool
setL k = \f s ->
  let f' True = Set.insert k s
      f' False = Set.delete k s
  in f' <$> f (Set.member k s)
              
-- |
--
-- >>> get (compose fstL sndL) ("abc", (7, "def"))
-- 7
--
-- >>> set (compose fstL sndL) ("abc", (7, "def")) 8
-- ("abc",(8,"def"))
compose :: Lens s t a b -> Lens q r s t -> Lens q r a b
compose = flip (.)

-- | An alias for @compose@.
(|.) :: Lens s t a b -> Lens q r s t -> Lens q r a b
(|.) = compose

infixr 9 |.

-- |
--
-- >>> get identity 3
-- 3
--
-- >>> set identity 3 4
-- 4
identity :: Lens a b a b
identity = id

-- |
--
-- >>> get (product fstL sndL) (("abc", 3), (4, "def"))
-- ("abc","def")
--
-- >>> set (product fstL sndL) (("abc", 3), (4, "def")) ("ghi", "jkl")
-- (("ghi",3),(4,"jkl"))
product :: Lens s t a b -> Lens q r c d -> Lens (s, q) (t, r) (a, c) (b, d)
product l1 l2 = 
  let get' l = getConst . l Const
  in \f (a, b) -> (\(v1, v2) -> (set l1 a v1, set l2 b v2)) <$> f (get' l1 a, get' l2 b)
  

-- | An alias for @product@.
(***) :: Lens s t a b -> Lens q r c d -> Lens (s, q) (t, r) (a, c) (b, d)
(***) = product

infixr 3 ***

-- |
--
-- >>> get (choice fstL sndL) (Left ("abc", 7))
-- "abc"
--
-- >>> get (choice fstL sndL) (Right ("abc", 7))
-- 7
--
-- >>> set (choice fstL sndL) (Left ("abc", 7)) "def"
-- Left ("def",7)
--
-- >>> set (choice fstL sndL) (Right ("abc", 7)) 8
-- Right ("abc",8)
choice :: Lens s t a b -> Lens q r a b -> Lens (Either s q) (Either t r) a b
choice l1 l2 = \f c ->
  case c of
    Left v -> Left <$> l1 f v
    Right v -> Right <$> l2 f v  
  

-- | An alias for @choice@.
(|||) :: Lens s t a b -> Lens q r a b -> Lens (Either s q) (Either t r) a b
(|||) = choice

infixr 2 |||

----

type Lens' a b = Lens a a b b

cityL :: Lens' Locality String
cityL p (Locality c t y) = fmap (\c' -> Locality c' t y) (p c)

stateL :: Lens' Locality String
stateL p (Locality c t y) = fmap (\t' -> Locality c t' y) (p t)

countryL :: Lens' Locality String
countryL p (Locality c t y) = fmap (\y' -> Locality c t y') (p y)

streetL :: Lens' Address String
streetL p (Address t s l) = fmap (\t' -> Address t' s l) (p t)

suburbL :: Lens' Address String
suburbL p (Address t s l) = fmap (\s' -> Address t s' l) (p s)

localityL :: Lens' Address Locality
localityL p (Address t s l) = fmap (\l' -> Address t s l') (p l)

ageL :: Lens' Person Int
ageL p (Person a n d) = fmap (\a' -> Person a' n d) (p a)

nameL :: Lens' Person String
nameL p (Person a n d) = fmap (\n' -> Person a n' d) (p n)

addressL :: Lens' Person Address
addressL p (Person a n d) = fmap (\d' -> Person a n d') (p d)

intAndIntL :: Lens' (IntAnd a) Int
intAndIntL p (IntAnd n a) = fmap (\n' -> IntAnd n' a) (p n)

-- lens for polymorphic update
intAndL ::
  Lens (IntAnd a) (IntAnd b) a b
intAndL p (IntAnd n a) =
  fmap (\a' -> IntAnd n a') (p a)

-- |
--
-- >>> getSuburb fred
-- "Fredville"
--
-- >>> getSuburb mary
-- "Maryland"
getSuburb :: Person -> String
getSuburb = get $ suburbL |. addressL

-- |
--
-- >>> setStreet fred "Some Other St"
-- Person 24 "Fred" (Address "Some Other St" "Fredville" (Locality "Fredmania" "New South Fred" "Fredalia"))
--
-- >>> setStreet mary "Some Other St"
-- Person 28 "Mary" (Address "Some Other St" "Maryland" (Locality "Mary Mary" "Western Mary" "Maristan"))
setStreet ::
  Person
  -> String
  -> Person
setStreet = set $ streetL |. addressL 

-- |
--
-- >>> getAgeAndCountry (fred, maryLocality)
-- (24,"Maristan")
--
-- >>> getAgeAndCountry (mary, fredLocality)
-- (28,"Fredalia")
getAgeAndCountry :: (Person, Locality) -> (Int, String)
getAgeAndCountry = get $ ageL *** countryL

-- |
--
-- >>> setCityAndLocality (fred, maryAddress) ("Some Other City", fredLocality)
-- (Person 24 "Fred" (Address "15 Fred St" "Fredville" (Locality "Some Other City" "New South Fred" "Fredalia")),Address "83 Mary Ln" "Maryland" (Locality "Fredmania" "New South Fred" "Fredalia"))
--
-- >>> setCityAndLocality (mary, fredAddress) ("Some Other City", maryLocality)
-- (Person 28 "Mary" (Address "83 Mary Ln" "Maryland" (Locality "Some Other City" "Western Mary" "Maristan")),Address "15 Fred St" "Fredville" (Locality "Mary Mary" "Western Mary" "Maristan"))
setCityAndLocality :: (Person, Address) -> (String, Locality) -> (Person, Address)
setCityAndLocality = set $ cityL |. localityL |. addressL *** localityL
  
-- |
--
-- >>> getSuburbOrCity (Left maryAddress)
-- "Maryland"
--
-- >>> getSuburbOrCity (Right fredLocality)
-- "Fredmania"
getSuburbOrCity :: Either Address Locality -> String
getSuburbOrCity = get $ suburbL ||| cityL

-- |
--
-- >>> setStreetOrState (Right maryLocality) "Some Other State"
-- Right (Locality "Mary Mary" "Some Other State" "Maristan")
--
-- >>> setStreetOrState (Left fred) "Some Other St"
-- Left (Person 24 "Fred" (Address "Some Other St" "Fredville" (Locality "Fredmania" "New South Fred" "Fredalia")))
setStreetOrState :: Either Person Locality -> String -> Either Person Locality
setStreetOrState = set $ streetL |. addressL ||| stateL

-- |
--
-- >>> modifyCityUppercase fred
-- Person 24 "Fred" (Address "15 Fred St" "Fredville" (Locality "FREDMANIA" "New South Fred" "Fredalia"))
--
-- >>> modifyCityUppercase mary
-- Person 28 "Mary" (Address "83 Mary Ln" "Maryland" (Locality "MARY MARY" "Western Mary" "Maristan"))
modifyCityUppercase :: Person -> Person
modifyCityUppercase = modify (cityL |. localityL |. addressL) (fmap toUpper)

-- |
--
-- >>> modifyIntAndLengthEven (IntAnd 10 "abc")
-- IntAnd 10 False
--
-- >>> modifyIntAndLengthEven (IntAnd 10 "abcd")
-- IntAnd 10 True
modifyIntAndLengthEven :: IntAnd [a] -> IntAnd Bool
modifyIntAndLengthEven = intAndL %~ even . length

----

-- |
--
-- >>> over traverseLocality (map toUpper) (Locality "abc" "def" "ghi")
-- Locality "ABC" "DEF" "GHI"
traverseLocality :: Traversal' Locality String
traverseLocality f (Locality ci st co) = Locality <$> f ci <*> f st <*> f co    


-- |
--
-- >>> over intOrIntP (*10) (IntOrIs 3)
-- IntOrIs 30
--
-- >>> over intOrIntP (*10) (IntOrIsNot "abc")
-- IntOrIsNot "abc"
intOrIntP :: Prism' (IntOr a) Int
intOrIntP = 
  prism 
    IntOrIs 
    (\io -> 
      case io of
        IntOrIs i -> Right i
        IntOrIsNot v -> Left (IntOrIsNot v))

intOrP :: Prism (IntOr a) (IntOr b) a b
intOrP = 
  prism 
    IntOrIsNot
    (\io ->
      case io of
        IntOrIsNot a -> Right a
        IntOrIs i -> Left (IntOrIs i))

--
-- >> over intOrP (even . length) (IntOrIsNot "abc")
-- IntOrIsNot False
--
-- >>> over intOrP (even . length) (IntOrIsNot "abcd")
-- IntOrIsNot True
--
-- >>> over intOrP (even . length) (IntOrIs 10)
-- IntOrIs 10
intOrLengthEven :: IntOr [a] -> IntOr Bool
intOrLengthEven = over intOrP (even . length)
  
