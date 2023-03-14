{-# LANGUAGE MagicHash #-}
{-# LANGUAGE UndecidableInstances #-}

-- | This module provides a type-level representation for term-level
-- 'P.Integer's. This type-level representation is also named 'P.Integer',
-- So import this module qualified to avoid name conflicts.
--
-- @
-- import "KindInteger" qualified as K
-- @
--
-- The implementation details are the same as the ones for type-level 'Natural's
-- in "GHC.TypeNats" as of @base-4.18@, and it will continue to evolve together
-- with @base@, trying to follow its API as much as possible until the day
-- @base@ provides its own type-level integer, making this module redundant.
module KindInteger
  ( -- * Integer Kind
    Integer
  , type P
  , type N
  , Normalize

    -- * Linking type and value level
  , KnownInteger(integerSing), integerVal, integerVal'
  , SomeInteger(..)
  , someIntegerVal
  , sameInteger

    -- ** Singleton values
  , SInteger
  , pattern SInteger
  , fromSInteger
  , withSomeSInteger
  , withKnownInteger

    -- * Arithmethic
  , type (+), type (*), type (^), type (-)
  , Negate, Div, Mod, Quot, Rem, Log2

    -- * Comparisons
  , CmpInteger
  , cmpInteger
  , type (==?), type (==), type (/=?), type (/=)
  ) where

import GHC.Base (WithDict(..))
import GHC.Types (TYPE, Constraint)
import GHC.Show (appPrec, appPrec1)
import GHC.Prim (Proxy#)
import GHC.TypeLits qualified as L
import Data.Proxy
import Data.Type.Coercion
import Data.Type.Equality (TestEquality(..), (:~:)(..))
import Data.Type.Bool (If)
import Data.Type.Ord
import Numeric.Natural (Natural)
import Prelude hiding (Integer, (==), (/=))
import Prelude qualified as P
import Unsafe.Coerce(unsafeCoerce)

--------------------------------------------------------------------------------

-- | Type-level version of 'P.Integer', only ever used as a /kind/
-- for 'P' and 'N'
--
-- * A positive number /+x/ is represented as @'P' x@.
--
-- * A negative number /-x/ is represented as @'N' x@.
--
-- * /Zero/ can be represented as @'P' 0@ or @'N' 0@. For consistency, all
-- /zero/ outputs from type families in this "KindInteger" module use the
-- @'P' 0@, but don't assume that this will be the case elsewhere. So, if you
-- need to treat /zero/ specially in some situation, be sure to handle both the
-- @'P' 0@ and @'N' 0@ cases.
data Integer
  = Positive Natural
  | Negative Natural

-- | * A positive number /+x/ is represented as @'P' x@.
--
-- * /Zero/ can be represented as @'P' 0@ (see notes at 'Integer').
type P (x :: Natural) = 'Positive x :: Integer

-- | * A negative number /-x/ is represented as @'N' x@.
--
-- * /Zero/ can be represented as @'N' 0@ (but often isn't, see notes at 'Integer').
type N (x :: Natural) = 'Negative x :: Integer

-- Not used:
--
-- typeIntegerToTermInteger :: Integer -> P.Integer
-- typeIntegerToTermInteger (P n) = toInteger n
-- typeIntegerToTermInteger (N n) = negate (toInteger n)

termIntegerToTypeInteger :: P.Integer -> Integer
termIntegerToTypeInteger i = let n = fromInteger i
                             in  if i >= 0 then Positive n else Negative n

-- | We are not interested in giving any instance to type-level 'Integer's,
-- so we implement 'showsPrec' here.
showsPrecInteger :: Int -> Integer -> ShowS
showsPrecInteger p i = showParen (p > appPrec) $ case i of
  Positive x -> showString "P " . shows x
  Negative x -> showString "N " . shows x

--------------------------------------------------------------------------------

-- | This class gives the integer associated with a type-level integer.
-- There are instances of the class for every integer.
class KnownInteger (i :: Integer) where
  integerSing :: SInteger i

-- | Positive numbers and zero.
instance L.KnownNat x => KnownInteger (P x) where
  integerSing = UnsafeSInteger (L.natVal (Proxy @x))

-- | Negative numbers and zero.
instance L.KnownNat x => KnownInteger (N x) where
  integerSing = UnsafeSInteger (negate (L.natVal (Proxy @x)))

-- | Term-level 'P.Integer' representation of the type-level 'Integer' @i@.
integerVal :: forall i proxy. KnownInteger i => proxy i -> P.Integer
integerVal _ = case integerSing :: SInteger i of UnsafeSInteger x -> x

-- | Term-level 'P.Integer' representation of the type-level 'Integer' @i@.
integerVal' :: forall i. KnownInteger i => Proxy# i -> P.Integer
integerVal' _ = case integerSing :: SInteger i of UnsafeSInteger x -> x

-- | This type represents unknown type-level 'Integer'.
data SomeInteger = forall n. KnownInteger n => SomeInteger (Proxy n)

-- | Convert a term-level 'P.Integer' into an unknown type-level 'Integer'.
someIntegerVal :: P.Integer -> SomeInteger
someIntegerVal i = withSomeSInteger i (\(si :: SInteger i) ->
                   withKnownInteger si (SomeInteger @i Proxy))

instance Eq SomeInteger where
  SomeInteger x == SomeInteger y = integerVal x P.== integerVal y

instance Ord SomeInteger where
  compare (SomeInteger x) (SomeInteger y) =
    compare (integerVal x) (integerVal y)

instance Show SomeInteger where
  showsPrec p (SomeInteger x) = showsPrec p (integerVal x)

instance Read SomeInteger where
  readsPrec p xs = do (a, ys) <- readsPrec p xs
                      [(someIntegerVal a, ys)]

--------------------------------------------------------------------------------
-- Within this module, we use these “normalization” tools to make sure that
-- /zero/ is always represented as @'P' 0@. We don't export any of these
-- normalization tools to end-users because it seems like we can't make them
-- reliable enough so as to offer a decent user experience. So, we just tell
-- users to deal with the fact that both @'P' 0@ and @'N' 0@ mean /zero/.

-- | Make sure /zero/ is represented as @'P' 0@, not as @'N' 0@
--
-- Notice that all the tools in the "KindInteger" can readily handle
-- non-'Normalize'd inputs. This 'Normalize' type-family is offered offered
-- only as a convenience in case you want to simplify /your/ dealing with
-- /zeros/.
type family Normalize (i :: Integer) :: Integer where
  Normalize (N 0) = P 0
  Normalize i     = i

-- | Construct a 'Normalize'd 'N'egative type-level 'Integer'.
--
-- To be used for producing all negative outputs in this module.
type NN (a :: Natural) = Normalize (N a) :: Integer

--------------------------------------------------------------------------------

infixl 6 +, -
infixl 7 *, `Div`, `Mod`, `Quot`, `Rem`
infixr 8 ^

-- | Negation of type-level 'Integer's.
type family Negate (x :: Integer) :: Integer where
  Negate (P 0) = P 0
  Negate (P x) = N x
  Negate (N x) = P x

-- | Addition of type-level 'Integer's.
type (a :: Integer) + (b :: Integer) = Add_ (Normalize a) (Normalize b) :: Integer
type family Add_ (a :: Integer) (b :: Integer) :: Integer where
  Add_ (P a) (P b) = P (a L.+ b)
  Add_ (N a) (N b) = NN (a L.+ b)
  Add_ (P a) (N b) = If (b <=? a) (P (a L.- b)) (NN (b L.- a))
  Add_ (N a) (P b) = Add_ (P b) (N a)

-- | Multiplication of type-level 'Integer's.
type (a :: Integer) * (b :: Integer) = Mul_ (Normalize a) (Normalize b) :: Integer
type family Mul_ (a :: Integer) (b :: Integer) :: Integer where
  Mul_ (P a) (P b) = P (a L.* b)
  Mul_ (N a) (N b) = Mul_ (P a) (P b)
  Mul_ (P a) (N b) = NN (a L.* b)
  Mul_ (N a) (P b) = Mul_ (P a) (N b)

-- | Exponentiation of type-level 'Integer's.
--
-- * Exponentiation by negative 'Integer' doesn't type-check.
type (a :: Integer) ^ (b :: Integer) = Pow_ (Normalize a) (Normalize b) :: Integer
type family Pow_ (a :: Integer) (b :: Integer) :: Integer where
  Pow_ (P a) (P b) = P (a L.^ b)
  Pow_ (N a) (P b) = NN (a L.^ b)
  Pow_ _     (N _) = L.TypeError ('L.Text "KindInteger.(^): Negative exponent")

-- | Subtraction of type-level 'Integer's.
type (a :: Integer) - (b :: Integer) = a + Negate b :: Integer

-- | Division ('floor'ed) of type-level 'Integer's.
--
-- @
-- forall (a :: 'Integer') (b :: 'Integer').
--   /such that/ (b '/=' 0).
--     a  '=='  'Div' a b '*' 'Negate' b '+' 'Mod' a b
-- @
--
-- * Division by /zero/ doesn't type-check.
type Div (a :: Integer) (b :: Integer) = Div_ (Normalize a) (Normalize b) :: Integer
type family Div_ (a :: Integer) (b :: Integer) :: Integer where
  Div_ _ (P 0) = L.TypeError ('L.Text "KindInteger.Div: Division by zero")
  Div_ (P a) (P b) = P (L.Div a b)
  Div_ (N a) (N b) = Div_ (P a) (P b)
  Div_ (P a) (N b) = NN (If (b L.* (L.Div a b) ==? a) (L.Div a b) (L.Div a b L.+ 1))
  Div_ (N a) (P b) = Div_ (P a) (N b)

-- | Modulus ('floor'ed division) of type-level 'Integer's.
--
-- @
-- forall (a :: 'Integer') (b :: 'Integer').
--   /such that/ (b '/=' 0).
--     a  '=='  'Div' a b '*' 'Negate' b '+' 'Mod' a b
-- @
--
-- * Modulus by /zero/ doesn't type-check.
type Mod (a :: Integer) (b :: Integer) = Div a b * Negate b + a :: Integer

-- | Division ('truncate'd) of type-level 'Integer's.
--
-- @
-- forall (a :: 'Integer') (b :: 'Integer').
--   /such that/ (b '/=' 0).
--     a  '=='  'Quot' a b '*' 'Negate' b '+' 'Rem' a b
-- @
--
-- * Division by /zero/ doesn't type-check.
type Quot (a :: Integer) (b :: Integer) = Quot_ (Normalize a) (Normalize b) :: Integer
type family Quot_ (a :: Integer) (b :: Integer) :: Integer where
  Quot_ _ (P 0) = L.TypeError ('L.Text "KindInteger.Quot: Division by zero")
  Quot_ (P a) (P b) = P (L.Div a b)
  Quot_ (N a) (N b) = Quot_ (P a) (P b)
  Quot_ (P a) (N b) = Negate (Quot_ (P a) (P b))
  Quot_ (N a) (P b) = Quot_ (P a) (N b)

-- | Remainder ('truncate'd division) of type-level 'Integer's.
--
-- @
-- forall (a :: 'Integer') (b :: 'Integer').
--   /such that/ (b '/=' 0).
--     a  '=='  'Quot' a b '*' 'Negate' b '+' 'Rem' a b
-- @
--
-- * Remainder by /zero/ doesn't type-check.
type Rem (a :: Integer) (b :: Integer) = Quot a b * Negate b + a :: Integer

-- | Log base 2 ('floor'ed) of type-level 'Integer's.
--
-- * Logarithm of /zero/ doesn't type-check.
--
-- * Logarithm of negative number doesn't type-check.
type Log2 (a :: Integer) = Log2_ (Normalize a) :: Integer
type family Log2_ (a :: Integer) :: Integer where
  Log2_ (P 0) = L.TypeError ('L.Text "KindInteger.Log2: Logarithm of zero")
  Log2_ (P a) = P (L.Log2 a)
  Log2_ (N a) = L.TypeError ('L.Text "KindInteger.Log2: Logarithm of negative number")

-- | Comparison of type-level 'Integer's, as a function.
type CmpInteger (a :: Integer) (b :: Integer) = CmpInteger_ (Normalize a) (Normalize b) :: Ordering
type family CmpInteger_ (a :: Integer) (b :: Integer) :: Ordering where
  CmpInteger_ a a = 'EQ
  CmpInteger_ (P a) (P b) = Compare a b
  CmpInteger_ (N a) (N b) = Compare b a
  CmpInteger_ (N _) (P _) = 'LT
  CmpInteger_ (P _) (N _) = 'GT

-- | "Data.Type.Ord" support for type-level 'Integer's.
type instance Compare (a :: Integer) (b :: Integer) = CmpInteger a b :: Ordering

--------------------------------------------------------------------------------

-- | We either get evidence that this function was instantiated with the
-- same type-level 'Integer's, or 'Nothing'.
sameInteger
  :: forall a b proxy1 proxy2
  .  (KnownInteger a, KnownInteger b)
  => proxy1 a
  -> proxy2 b
  -> Maybe (a :~: b)
sameInteger _ _ = testEquality (integerSing @a) (integerSing @b)

-- | Like 'sameInteger', but if the type-level 'Integer's aren't equal, this
-- additionally provides proof of 'LT' or 'GT'.
cmpInteger
  :: forall a b proxy1 proxy2
  .  (KnownInteger a, KnownInteger b)
  => proxy1 a
  -> proxy2 b
  -> OrderingI a b
cmpInteger x y = case compare (integerVal x) (integerVal y) of
  EQ -> case unsafeCoerce (Refl, Refl) :: (CmpInteger a b :~: 'EQ, a :~: b) of
    (Refl, Refl) -> EQI
  LT -> case unsafeCoerce Refl :: (CmpInteger a b :~: 'LT) of
    Refl -> LTI
  GT -> case unsafeCoerce Refl :: (CmpInteger a b :~: 'GT) of
    Refl -> GTI

--------------------------------------------------------------------------------

-- | Singleton type for a type-level 'Integer' @i@.
newtype SInteger (i :: Integer) = UnsafeSInteger P.Integer

-- | A explicitly bidirectional pattern synonym relating an 'SInteger' to a
-- 'KnownInteger' constraint.
--
-- As an __expression__: Constructs an explicit @'SInteger' i@ value from an
-- implicit @'KnownInteger' i@ constraint:
--
-- @
-- 'SInteger' @i :: 'KnownInteger' i => 'SInteger' i
-- @
--
-- As a __pattern__: Matches on an explicit @'SInteger' i@ value bringing
-- an implicit @'KnownInteger' i@ constraint into scope:
--
-- @
-- f :: 'SInteger' i -> ..
-- f SInteger = {- SInteger i in scope -}
-- @
pattern SInteger :: forall i. () => KnownInteger i => SInteger i
pattern SInteger <- (knownIntegerInstance -> KnownIntegeregerInstance)
  where SInteger = integerSing

-- | An internal data type that is only used for defining the 'SInteger' pattern
-- synonym.
data KnownIntegeregerInstance (i :: Integer) where
  KnownIntegeregerInstance :: KnownInteger i => KnownIntegeregerInstance i

-- | An internal function that is only used for defining the 'SInteger' pattern
-- synonym.
knownIntegerInstance :: SInteger i -> KnownIntegeregerInstance i
knownIntegerInstance si = withKnownInteger si KnownIntegeregerInstance

instance Show (SInteger i) where
  showsPrec p (UnsafeSInteger i) = showParen (p > appPrec) $
    showString "SInteger @" .
    showsPrecInteger appPrec1 (termIntegerToTypeInteger i)

instance TestEquality SInteger where
  testEquality (UnsafeSInteger x) (UnsafeSInteger y)
    | x P.== y  = Just (unsafeCoerce Refl)
    | otherwise = Nothing

instance TestCoercion SInteger where
  testCoercion x y = fmap (\Refl -> Coercion) (testEquality x y)

-- | Return the type-level 'P.Integer' number corresponding to @i@ in
-- a @'SInteger' i@ value.
fromSInteger :: SInteger i -> P.Integer
fromSInteger (UnsafeSInteger i) = i

-- | Convert an explicit @'SInteger' i@ value into an implicit
-- @'KnownInteger' i@ constraint.
withKnownInteger
  :: forall i rep (r :: TYPE rep). SInteger i -> (KnownInteger i => r) -> r
withKnownInteger = withDict @(KnownInteger i)

-- | Convert a 'P.Integer' number into an @'SInteger' n@ value, where @n@ is a
-- fresh type-level 'Integer'.
withSomeSInteger
  :: forall rep (r :: TYPE rep). P.Integer -> (forall n. SInteger n -> r) -> r
withSomeSInteger n k = k (UnsafeSInteger n)
-- It's very important to keep this NOINLINE! See the docs at "GHC.TypeNats"
{-# NOINLINE withSomeSInteger #-}

--------------------------------------------------------------------------------
-- Extras

infixr 4 /=, /=?, ==, ==?

-- | This should be exported by "Data.Type.Ord".
type (a :: k) ==? (b :: k) = OrdCond (Compare a b) 'False 'True 'False :: Bool

-- | This should be exported by "Data.Type.Ord".
type (a :: k) == (b :: k) = (a ==? b) ~ 'True :: Constraint

-- | This should be exported by "Data.Type.Ord".
type (a :: k) /=? (b :: k) = OrdCond (Compare a b) 'True 'False 'True :: Bool

-- | This should be exported by "Data.Type.Ord".
type (a :: k) /= (b :: k) = (a /=? b) ~ 'True :: Constraint

