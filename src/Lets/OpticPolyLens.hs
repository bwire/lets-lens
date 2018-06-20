{-# LANGUAGE RankNTypes #-}

module Lets.OpticPolyLens (
  Lens(..)
, getsetLaw
, setgetLaw
, setsetLaw
, get
, set
, modify
, (%~)
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
, cityL
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
, modifyIntandLengthEven
) where

import Data.Char(toUpper)
import Data.Map(Map)
import qualified Data.Map as Map(insert, delete, lookup)
import Data.Set(Set)
import qualified Data.Set as Set(insert, delete, member)
import Lets.Data(AlongsideLeft(AlongsideLeft, getAlongsideLeft), AlongsideRight(AlongsideRight, getAlongsideRight), Identity(Identity, getIdentity), Const(Const, getConst), IntAnd(IntAnd), Person(Person), Locality(Locality), Address(Address), bool)
import Prelude hiding (product)

-- $setup
-- >>> import qualified Data.Map as Map(fromList)
-- >>> import qualified Data.Set as Set(fromList)
-- >>> import Data.Char(ord)
-- >>> import Lets.Data

data Lens s t a b = Lens (forall f. Functor f => (a -> f b) -> s -> f t)

get :: Lens s t a b -> s -> a
get (Lens f) = getConst . f Const

set :: Lens s t a b -> s -> b -> t
set (Lens f) s v = getIdentity $ f (const $ Identity v) s

-- | The get/set law of lenses. This function should always return @True@.
getsetLaw :: Eq s => Lens s s a a -> s -> Bool
getsetLaw l s = set l s (get l s) == s 

-- | The set/get law of lenses. This function should always return @True@.
setgetLaw :: Eq a => Lens s s a a -> s -> a -> Bool
setgetLaw l s v = get l (set l s v) == v

-- | The set/set law of lenses. This function should always return @True@.
setsetLaw :: Eq s => Lens s s a b -> s -> b -> b -> Bool 
setsetLaw l a b1 b2 = set l (set l a b1) b2 == set l a b2 

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
modify l f = set l <*> f . get l 

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
(.~) = flip . set

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
fmodify (Lens f) = f

-- |
--
-- >>> fstL |= Just 3 $ (7, "abc")
-- Just (3,"abc")
--
-- >>> (fstL |= (+1) $ (3, "abc")) 17
-- (18,"abc")
(|=) :: Functor f => Lens s t a b -> f b -> s -> f t
(|=) (Lens f) = f . const 

infixl 5 |=

-- |
--
-- >>> modify fstL (*10) (3, "abc")
-- (30,"abc"
--
-- prop> let types = (x :: Int, y :: String) in getsetLaw fstL (x, y)
--
-- prop> let types = (x :: Int, y :: String) in setgetLaw fstL (x, y) z
--
-- prop> let types = (x :: Int, y :: String) in setsetLaw fstL (x, y) z
fstL :: Lens (a, x) (b, x) a b
fstL = Lens $ \f (a, b) -> (\v -> (v, b)) <$> f a

-- |
--
-- >>> modify sndL (++ "def") (13, "abc")
-- (13,"abcdef")
--
-- prop> let types = (x :: Int, y :: String) in getsetLaw sndL (x, y)
--
-- prop> let types = (x :: Int, y :: String) in setgetLaw sndL (x, y) z
--
-- prop> let types = (x :: Int, y :: String) in setsetLaw sndL (x, y) z
sndL :: Lens (x, a) (x, b) a b
sndL = Lens $ \f (a, b) -> (\v -> (a, v)) <$> f b

-- |
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
mapL k = Lens $ \f m -> 
  let f' v = case v of
        Just v' -> Map.insert k v' m
        _       -> Map.delete k m
  in f' <$> f (Map.lookup k m)

-- |
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
setL :: Ord k => k -> Lens (Set k) (Set k) Bool Bool
setL k = Lens $ \f s -> 
  let f' v = case v of
        True  -> Set.insert k s
        False -> Set.delete k s
  in f' <$> f (Set.member k s)

-- |
--
-- >>> get (compose fstL sndL) ("abc", (7, "def"))
-- 7
--
-- >>> set (compose fstL sndL) ("abc", (7, "def")) 8
-- ("abc",(8,"def"))
compose :: Lens s t a b -> Lens q r s t -> Lens q r a b
compose l1 l2 = Lens $ \f c -> 
  set l2 c . (set l1 (get l2 c)) <$> f (get l1 . get l2 $ c) 

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
identity = Lens $ id <$> ($)

-- |
--
-- >>> get (product fstL sndL) (("abc", 3), (4, "def"))
-- ("abc","def")
--
-- >>> set (product fstL sndL) (("abc", 3), (4, "def")) ("ghi", "jkl")
-- (("ghi",3),(4,"jkl"))
product :: Lens s t a b -> Lens q r c d -> Lens (s, q) (t, r) (a, c) (b, d)
product l1 l2 = Lens $ \f (a, b) -> (\(v1, v2) -> (set l1 a v1, set l2 b v2)) <$> f (get l1 a, get l2 b)

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
choice l1 l2 = Lens $ \f ei ->  
  case ei of 
    Left v -> (Left . set l1 v) <$> f (get l1 v)
    Right v -> (Right . set l2 v) <$> f (get l2 v)

-- | An alias for @choice@.
(|||) :: Lens s t a b -> Lens q r a b -> Lens (Either s q) (Either t r) a b
(|||) = choice

infixr 2 |||

-------------------------------

type Lens' a b = Lens a a b b

cityL :: Lens' Locality String
cityL = Lens (\f (Locality c t y) -> (\v -> Locality v t y) <$> (f c))

stateL :: Lens' Locality String
stateL = Lens (\f (Locality c t y) -> (\v -> Locality c v y) <$> (f t))

countryL :: Lens' Locality String
countryL = Lens (\f (Locality c t y) -> (\v -> Locality c t v) <$> (f y))

streetL :: Lens' Address String
streetL = Lens (\f (Address t s l) -> (\v -> Address v s l) <$> (f t))

suburbL :: Lens' Address String
suburbL = Lens (\f (Address t s l) -> (\v -> Address t v l) <$> (f s))

localityL :: Lens' Address Locality
localityL = Lens (\f (Address t s l) -> (\v -> Address t s v) <$> (f l))

ageL :: Lens' Person Int
ageL = Lens (\f (Person a n d) -> (\v -> Person v n d) <$> (f a))

nameL :: Lens' Person String
nameL = Lens (\f (Person a n d) -> (\v -> Person a v d) <$> (f n))

addressL :: Lens' Person Address
addressL = Lens (\f (Person a n d) -> (\v -> Person a n v) <$> (f d))

intAndIntL :: Lens' (IntAnd a) Int
intAndIntL = Lens (\f (IntAnd n a) -> (\v -> IntAnd v a) <$> (f n))

intAndL :: Lens (IntAnd a) (IntAnd b) a b
intAndL = Lens (\f (IntAnd n a) -> (\v -> IntAnd n v) <$> (f a))

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
setStreet :: Person -> String -> Person
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
setCityAndLocality = set $ (cityL |. localityL |. addressL) *** localityL

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
setStreetOrState = set $ (streetL |. addressL) ||| stateL

-- |
--
-- >>> modifyCityUppercase fred
-- Person 24 "Fred" (Address "15 Fred St" "Fredville" (Locality "FREDMANIA" "New South Fred" "Fredalia"))
--
-- >>> modifyCityUppercase mary
-- Person 28 "Mary" (Address "83 Mary Ln" "Maryland" (Locality "MARY MARY" "Western Mary" "Maristan"))
modifyCityUppercase :: Person -> Person
modifyCityUppercase = cityL |. localityL |. addressL %~ map toUpper

-- |
--
-- >>> modify intAndL (even . length) (IntAnd 10 "abc")
-- IntAnd 10 False
--
-- >>> modify intAndL (even . length) (IntAnd 10 "abcd")
-- IntAnd 10 True
modifyIntandLengthEven :: IntAnd [a] -> IntAnd Bool
modifyIntandLengthEven = intAndL %~ even . length











