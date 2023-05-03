{-# LANGUAGE StrictData #-}
{-# LANGUAGE UndecidableInstances #-}

-- | This module provides a type-level representation for term-level
-- 'P.Integer's. This type-level representation is also named 'P.Integer',
-- So import this module qualified to avoid name conflicts.
--
-- @
-- import "KindInteger" qualified as KI
-- @
--
-- The implementation details are the same as the ones for type-level 'Natural's
-- in "GHC.TypeNats" as of @base-4.18@, and it will continue to evolve together
-- with @base@, trying to follow its API as much as possible until the day
-- @base@ provides its own type-level integer, making this module redundant.
module KindInteger {--}
  ( -- * Integer kind
    Integer
  , type Z
  , type N
  , type P
  , Normalize
  , showsPrecTypeLit
  , readPrecTypeLit

    -- * Types ⇔ Terms
  , KnownInteger
  , integerSing
  , integerVal
  , SomeInteger(..)
  , someIntegerVal
  , sameInteger

    -- * Singletons
  , SInteger
  , pattern SInteger
  , fromSInteger
  , withSomeSInteger
  , withKnownInteger

    -- * Arithmethic
  , type (+), type (*), type (^), type (-)
  , Odd, Even, Abs, Sign, Negate, GCD, LCM, Log2

    -- ** Division
  , Div
  , Rem
  , DivRem
  , Round(..)
    -- *** Term-level
  , div
  , rem
  , divRem

    -- * Comparisons
  , CmpInteger
  , cmpInteger

    -- * Extra
  , type (==?), type (==), type (/=?), type (/=)
  ) --}
  where

import Control.Applicative
import Control.Exception qualified as Ex
import Control.Monad
import Data.Bits
import Data.Char qualified as Char
import Data.Proxy
import Data.Singletons
import Data.Singletons.Decide
import Data.Type.Bool (If)
import Data.Type.Coercion
import Data.Type.Equality (TestEquality(..))
import Data.Type.Ord
import GHC.Base (WithDict(..))
import GHC.Exts (TYPE, Constraint)
import GHC.Real qualified as P
import GHC.Show (appPrec1)
import GHC.TypeLits qualified as L hiding (someNatVal)
import GHC.TypeNats qualified as L (someNatVal)
import Numeric.Natural (Natural)
import Prelude hiding (Integer, (==), (/=), div, rem)
import Prelude qualified as P
import Text.ParserCombinators.ReadP qualified as ReadP
import Text.ParserCombinators.ReadPrec qualified as ReadPrec
import Text.Read qualified as Read
import Unsafe.Coerce (unsafeCoerce)

--------------------------------------------------------------------------------

-- | Type-level version of 'P.Integer', used as a /kind/ for 'Z', 'N' and 'P'.
--
-- * Zero is represented as 'Z'.
--
-- * A positive number /+x/ is represented as @'P' x@.
--
-- * A negative number /-x/ is represented as @'N' x@.

-- Note: We could use a better internal representation that guarantees that
-- Negative and Positive don't contain zero. However, this one looks better
-- on compiler errors, that's why we use it. The KindInteger module doesn't
-- export anything that would allow the construction of a non-'Normalize'd
-- 'Integer' anyway, so throughout this module we can make some assumptions.
data Integer
  = Z         -- ^ This is the only way to represent 0.
  | N Natural -- ^ Never 0.
  | P Natural -- ^ Never 0.

-- | Zero is represented as 'Z'.
type Z = 'Z :: Integer
-- | A negative number /-x/ is represented as @'N' x@.
type N (x :: Natural) = 'N x :: Integer
-- | A positive number /+x/ is represented as @'P' x@.
type P (x :: Natural) = 'P x :: Integer

-- | Displays the "Prelude".'P.Integer' as it would appear
-- literally in the types (@\"Z\"@, @\"P 1\"@, @\"N 1\"@, etc.)
-- as a type of kind "KindInteger".'Integer'.
showsPrecTypeLit :: Int -> P.Integer -> ShowS
showsPrecTypeLit p = \case
  0         -> showChar 'Z'
  i | i > 0 -> showParen (p >= appPrec1) (showString "P " . shows i)
  i         -> showParen (p >= appPrec1) (showString "N " . shows (abs i))

-- | Inverse of 'showsPrecTypeLit'.
readPrecTypeLit :: ReadPrec.ReadPrec P.Integer
readPrecTypeLit = Read.parens $ asum
    [ 0 <$ ReadPrec.lift (ReadP.char 'Z')
    , do ReadPrec.lift $ ReadP.char 'N' >> pSkipSpaces1
         fmap (negate . fromIntegral) $ Read.parens (ReadPrec.lift pNatural)
    , do ReadPrec.lift $ ReadP.char 'P' >> pSkipSpaces1
         fmap toInteger $ Read.parens (ReadPrec.lift pNatural)
    ]
  where
    pNatural :: ReadP.ReadP Natural
    pNatural = read <$> ReadP.munch1 (\c -> c >= '0' && c <= '9')

--------------------------------------------------------------------------------

-- | This class gives the 'SInteger' associated with a type-level 'Integer'.
--
-- There are instances for every 'Normalize'd 'Integer'.

-- Note: Ideally,  @'Normalize' i ~ i@ would be a superclass. However,
-- 'withDict' doesn't allow having a superclass here, so we treat
-- 'KnownInteger_' as internal an export 'KnownInteger' only.
class KnownInteger_ (i :: Integer) where
  integerSing_ :: SInteger i

-- | Type-level 'Integer's satisfying 'KnownInteger' can be converted to
-- 'SInteger's using 'integerSing'.
type KnownInteger (i :: Integer) =
  (KnownInteger_ i, Normalize i ~ i, L.KnownNat (Abs_ i))

-- | Convert an implicit 'KnownInteger' to an explicit 'SInteger'.
integerSing :: KnownInteger i => SInteger i
integerSing = integerSing_ -- The difference is in the constraint.
{-# INLINE integerSing #-}

-- | Positive.
instance (KnownInteger (P x)) => KnownInteger_ (P x) where
  integerSing_ = UnsafeSInteger (L.natVal (Proxy @x))

-- | Negative.
instance (KnownInteger (N x)) => KnownInteger_ (N x) where
  integerSing_ = UnsafeSInteger (negate (L.natVal (Proxy @x)))

-- | Zero.
instance KnownInteger Z => KnownInteger_ Z where
  integerSing_ = UnsafeSInteger 0

-- | Term-level "Prelude".'P.Integer' representation of the type-level
-- 'Integer'.
integerVal :: forall i proxy. KnownInteger i => proxy i -> P.Integer
integerVal _ = case integerSing :: SInteger i of UnsafeSInteger x -> x
{-# INLINE integerVal #-}

-- | Term-level representation of an existentialized 'KnownInteger'.
data SomeInteger = forall i. KnownInteger i => SomeInteger (Proxy i)

-- | Convert a term-level "Prelude".'P.Integer' into an
-- extistentialized 'KnownInteger' wrapped in 'SomeInteger'.
someIntegerVal :: P.Integer -> SomeInteger
someIntegerVal = \i ->
  withSomeSInteger i $ \(si :: SInteger i) ->
  withKnownInteger si $ SomeInteger (Proxy @i)

instance Eq SomeInteger where
  SomeInteger x == SomeInteger y =
    integerVal x P.== integerVal y
  {-# INLINE (==) #-}

instance Ord SomeInteger where
  compare (SomeInteger x) (SomeInteger y) =
    compare (integerVal x) (integerVal y)
  {-# INLINE compare #-}

-- | As for "Prelude".'P.Integer'.
instance Show SomeInteger where
  showsPrec p (SomeInteger i) = showsPrec p (integerVal i)

-- | As for "Prelude".'P.Integer'.
instance Read SomeInteger where
  readPrec = fmap someIntegerVal Read.readPrec

--------------------------------------------------------------------------------

-- | Make sure /zero/ is represented as @'P' 0@, not as @'N' 0@
--
-- Notice that all type-families in the "KindInteger" module can readily handle
-- non-'Normalize'd inputs. You may need to use 'Normalize' when dealing with
-- 'KnownInteger', though.
type family Normalize (i :: Integer) :: Integer where
  Normalize (N 0) = Z
  Normalize (P 0) = Z
  Normalize i     = i

-- | Construct a 'Normalize'd 'N'egative type-level 'Integer'.
--
-- | To be used for producing all negative outputs in this module.
type NN (a :: Natural) = Normalize (N a) :: Integer

-- | Construct a 'Normalize'd 'P'ositive type-level 'Integer'.
--
-- To be used for producing all positive outputs in this module.
type PP (a :: Natural) = Normalize (P a) :: Integer

--------------------------------------------------------------------------------

infixl 6 +, -
infixl 7 *, `Div`, `Rem`
infixr 8 ^

-- | Whether a type-level 'Natural' is odd. /Zero/ is not considered odd.
type Odd (x :: Integer) = L.Mod (Abs x) 2 ==? 1 :: Bool

-- | Whether a type-level 'Natural' is even. /Zero/ is considered even.
type Even (x :: Integer) = L.Mod (Abs x) 2 ==? 0 :: Bool

-- | Negation of type-level 'Integer's.
type Negate (x :: Integer) = Negate_ (Normalize x) :: Integer
type family Negate_ (x :: Integer) :: Integer where
  Negate_ Z     = Z
  Negate_ (P x) = N x
  Negate_ (N x) = P x

-- | Sign of type-level 'Integer's.
--
-- * 'Z' if zero.
--
-- * @'P' 1@ if positive.
--
-- * @'N' 1@ if negative.
type Sign (x :: Integer) = Sign_ (Normalize x) :: Integer
type family Sign_ (x :: Integer) :: Integer where
  Sign_  Z    = Z
  Sign_ (P _) = P 1
  Sign_ (N _) = N 1

-- | Absolute value of a type-level 'Integer', as a type-level 'Natural'.
type Abs (x :: Integer) = Abs_ (Normalize x) :: Natural
type family Abs_ (x :: Integer) :: Natural where
  Abs_  Z    = 0
  Abs_ (P x) = x
  Abs_ (N x) = x

-- | Addition of type-level 'Integer's.
type (a :: Integer) + (b :: Integer) = Add_ (Normalize a) (Normalize b) :: Integer
type family Add_ (a :: Integer) (b :: Integer) :: Integer where
  Add_ Z     b     = b
  Add_ a     Z     = a
  Add_ (P a) (P b) = PP (a L.+ b)
  Add_ (N a) (N b) = NN (a L.+ b)
  Add_ (P a) (N b) = If (b <=? a) (PP (a L.- b)) (NN (b L.- a))
  Add_ (N a) (P b) = Add_ (P b) (N a)

-- | Multiplication of type-level 'Integer's.
type (a :: Integer) * (b :: Integer) = Mul_ (Normalize a) (Normalize b) :: Integer
type family Mul_ (a :: Integer) (b :: Integer) :: Integer where
  Mul_ Z     b     = Z
  Mul_ a     Z     = Z
  Mul_ (P a) (P b) = PP (a L.* b)
  Mul_ (N a) (N b) = Mul_ (P a) (P b)
  Mul_ (P a) (N b) = NN (a L.* b)
  Mul_ (N a) (P b) = Mul_ (P a) (N b)

-- | Exponentiation of type-level 'Integer's.
--
-- * Exponentiation by negative 'Integer' doesn't type-check.
--
-- * @'Z' '^' 'Z'@ doesn't type-check.
type (a :: Integer) ^ (b :: Integer) = Pow_ (Normalize a) (Normalize b) :: Integer
type family Pow_ (a :: Integer) (b :: Integer) :: Integer where
  Pow_ Z      Z    = L.TypeError ('L.Text "KindInteger.(^): 0^0 is undefined")
  Pow_ Z     (P _) = Z
  Pow_ _      Z    = P 1
  Pow_ (P a) (P b) = PP (a L.^ b)
  Pow_ (N a) (P b) = NN (a L.^ b)
  Pow_ _     (N _) = L.TypeError ('L.Text "KindInteger.(^): Negative exponent")

-- | Subtraction of type-level 'Integer's.
type (a :: Integer) - (b :: Integer) = a + Negate b :: Integer

-- | Get both the quotient and the 'Rem'ainder of the 'Div'ision of
-- type-level 'Integer's @a@ and @b@ using the specified 'Round'ing @r@.
--
-- @
-- forall (r :: 'Round') (a :: 'Integer') (b :: 'Integer').
--   (b '/=' 0) =>
--     'DivRem' r a b '=='  '('Div' r a b, 'Rem' r a b)
-- @
type DivRem (r :: Round) (a :: Integer) (b :: Integer) =
  '( Div r a b, Rem r a b ) :: (Integer, Integer)

-- | 'Rem'ainder of the division of type-level 'Integer' @a@ by @b@,
-- using the specified 'Round'ing @r@.
--
-- @
-- forall (r :: 'Round') (a :: 'Integer') (b :: 'Integer').
--   (b '/=' 0) =>
--     'Rem' r a b  '=='  a '-' b '*' 'Div' r a b
-- @
--
-- * Division by /zero/ doesn't type-check.
type Rem (r :: Round) (a :: Integer) (b :: Integer) =
  a - b * Div r a b :: Integer

-- | Divide of type-level 'Integer' @a@ by @b@,
-- using the specified 'Round'ing @r@.
--
-- * Division by /zero/ doesn't type-check.
type Div (r :: Round) (a :: Integer) (b :: Integer) =
  Div_ r (Normalize a) (Normalize b) :: Integer

type family Div_ (r :: Round) (a :: Integer) (b :: Integer) :: Integer where
  Div_ r Z Z = Div__ r Z 0
  Div_ r Z (P b) = Div__ r Z b
  Div_ r Z (N b) = Div__ r Z b
  Div_ r (P a) (P b) = Div__ r (P a) b
  Div_ r (N a) (N b) = Div__ r (P a) b
  Div_ r (P a) (N b) = Div__ r (N a) b
  Div_ r (N a) (P b) = Div__ r (N a) b

type family Div__ (r :: Round) (a :: Integer) (b :: Natural) :: Integer where
  Div__ _ _ 0 = L.TypeError ('L.Text "KindInteger.Div: Division by zero")
  Div__ _ Z _ = Z

  Div__ 'RoundDown (P a) b = PP (L.Div a b)
  Div__ 'RoundDown (N a) b = NN (If (b L.* L.Div a b ==? a)
                                    (L.Div a b)
                                    (L.Div a b L.+ 1))

  Div__ 'RoundUp a b = Negate (Div__ 'RoundDown (Negate a) b)

  Div__ 'RoundZero (P a) b = Div__ 'RoundDown (P a) b
  Div__ 'RoundZero (N a) b = Negate (Div__ 'RoundDown (P a) b)

  Div__ 'RoundAway (P a) b = Div__ 'RoundUp (P a) b
  Div__ 'RoundAway (N a) b = Div__ 'RoundDown (N a) b

  Div__ 'RoundHalfDown a b = If (HalfLT (R a b) (Div__ 'RoundUp a b))
                                (Div__ 'RoundUp a b)
                                (Div__ 'RoundDown a b)

  Div__ 'RoundHalfUp a b = If (HalfLT (R a b) (Div__ 'RoundDown a b))
                              (Div__ 'RoundDown a b)
                              (Div__ 'RoundUp a b)

  Div__ 'RoundHalfEven a b = If (HalfLT (R a b) (Div__ 'RoundDown a b))
                                (Div__ 'RoundDown a b)
                                (If (HalfLT (R a b) (Div__ 'RoundUp a b))
                                    (Div__ 'RoundUp a b)
                                    (If (Even (Div__ 'RoundDown a b))
                                        (Div__ 'RoundDown a b)
                                        (Div__ 'RoundUp a b)))

  Div__ 'RoundHalfOdd a b = If (HalfLT (R a b) (Div__ 'RoundDown a b))
                               (Div__ 'RoundDown a b)
                               (If (HalfLT (R a b) (Div__ 'RoundUp a b))
                                   (Div__ 'RoundUp a b)
                                   (If (Odd (Div__ 'RoundDown a b))
                                       (Div__ 'RoundDown a b)
                                       (Div__ 'RoundUp a b)))

  Div__ 'RoundHalfZero a b = If (HalfLT (R a b) (Div__ 'RoundDown a b))
                                (Div__ 'RoundDown a b)
                                (If (HalfLT (R a b) (Div__ 'RoundUp a b))
                                    (Div__ 'RoundUp a b)
                                    (Div__ 'RoundZero a b))

  Div__ 'RoundHalfAway (P a) b = Div__ 'RoundHalfUp   (P a) b
  Div__ 'RoundHalfAway (N a) b = Div__ 'RoundHalfDown (N a) b


-- | Log base 2 ('floor'ed) of type-level 'Integer's.
--
-- * Logarithm of /zero/ doesn't type-check.
--
-- * Logarithm of negative number doesn't type-check.
type Log2 (a :: Integer) = Log2_ (Normalize a) :: Integer
type family Log2_ (a :: Integer) :: Integer where
  Log2_ Z = L.TypeError ('L.Text "KindInteger.Log2: Logarithm of zero")
  Log2_ (P a) = PP (L.Log2 a)
  Log2_ (N a) = L.TypeError ('L.Text "KindInteger.Log2: Logarithm of negative number")

-- | Greatest Common Divisor of type-level 'Integer' numbers @a@ and @b@.
--
-- Returns a 'Natural', since the Greatest Common Divisor is always positive.
type GCD (a :: Integer) (b :: Integer) = NatGCD (Abs a) (Abs b) :: Natural

-- | Greatest Common Divisor of type-level 'Natural's @a@ and @b@.
type family NatGCD (a :: Natural) (b :: Natural) :: Natural where
  NatGCD a 0 = a
  NatGCD a b = NatGCD b (L.Mod a b)

-- | Least Common Multiple of type-level 'Integer' numbers @a@ and @b@.
--
-- Returns a 'Natural', since the Least Common Multiple is always positive.
type LCM (a :: Integer) (b :: Integer) = NatLCM (Abs a) (Abs b) :: Natural

-- | Least Common Multiple of type-level 'Natural's @a@ and @b@.
type NatLCM (a :: Natural) (b :: Natural) =
  L.Div a (NatGCD a b) L.* b :: Natural

--------------------------------------------------------------------------------

-- | Comparison of type-level 'Integer's, as a function.
type CmpInteger (a :: Integer) (b :: Integer) =
  CmpInteger_ (Normalize a) (Normalize b) :: Ordering
type family CmpInteger_ (a :: Integer) (b :: Integer) :: Ordering where
  CmpInteger_ a a = 'EQ
  CmpInteger_ (N a) (N b) = Compare b a
  CmpInteger_ (N a) Z     = 'LT
  CmpInteger_ (N _) (P _) = 'LT
  CmpInteger_ Z     (N _) = 'GT
  CmpInteger_ Z     (P _) = 'LT
  CmpInteger_ (P _) (N _) = 'GT
  CmpInteger_ (P _) Z     = 'GT
  CmpInteger_ (P a) (P b) = Compare a b

type instance Compare (a :: Integer) (b :: Integer) =
  CmpInteger a b :: Ordering

--------------------------------------------------------------------------------

-- | We either get evidence that this function was instantiated with the
-- same type-level 'Integer's, or 'Nothing'.
sameInteger
  :: forall a b proxy1 proxy2
  .  (KnownInteger a, KnownInteger b)
  => proxy1 a
  -> proxy2 b
  -> Maybe (a :~: b)
sameInteger _ _ = testEquality (sing @a) (sing @b)

-- | Like 'sameInteger', but if the type-level 'Integer's aren't equal, this
-- additionally provides proof of 'LT' or 'GT'.
cmpInteger
  :: forall a b proxy1 proxy2
  .  (KnownInteger a, KnownInteger b)
  => proxy1 a
  -> proxy2 b
  -> OrderingI a b
cmpInteger _ _ = case compare (demote @a) (demote @b) of
  EQ -> case unsafeCoerce Refl :: CmpInteger a b :~: 'EQ of
    Refl -> case unsafeCoerce Refl :: a :~: b of
      Refl -> EQI
  LT -> case unsafeCoerce Refl :: (CmpInteger a b :~: 'LT) of
    Refl -> LTI
  GT -> case unsafeCoerce Refl :: (CmpInteger a b :~: 'GT) of
    Refl -> GTI

--------------------------------------------------------------------------------

-- | Singleton type for a type-level 'Integer' @i@.
newtype SInteger (i :: Integer) = UnsafeSInteger P.Integer
type role SInteger representational

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
pattern SInteger <- (knownIntegerInstance -> KnownIntegerInstance)
  where SInteger = integerSing

-- | An internal data type that is only used for defining the 'SInteger' pattern
-- synonym.
data KnownIntegerInstance (i :: Integer) where
  KnownIntegerInstance :: KnownInteger i => KnownIntegerInstance i

-- | An internal function that is only used for defining the 'SInteger' pattern
-- synonym.
knownIntegerInstance :: SInteger i -> KnownIntegerInstance i
knownIntegerInstance si = withKnownInteger si KnownIntegerInstance

instance Eq (SInteger i) where
  _ == _ = True

instance Ord (SInteger i) where
  compare _ _ = P.EQ

instance Show (SInteger i) where
  showsPrec p (UnsafeSInteger i) =
    showParen (p >= appPrec1) $
      showString "SInteger @" .
      showsPrecTypeLit appPrec1 i

instance forall i. KnownInteger i => Read (SInteger i) where
  readPrec = ReadPrec.lift $ do
    let si = integerSing @i
    _ <- ReadP.string "SInteger" >> pSkipSpaces1
    _ <- ReadP.char '@'
    _ <- ReadP.string (showsPrecTypeLit appPrec1 (fromSInteger si) "")
    pure si

instance TestEquality SInteger where
  testEquality = decideEquality
  {-# INLINE testEquality #-}

instance TestCoercion SInteger where
  testCoercion = decideCoercion
  {-# INLINE testCoercion #-}

-- | Return the term-level "Prelude".'P.Integer' number corresponding to @i@.
fromSInteger :: SInteger i -> P.Integer
fromSInteger (UnsafeSInteger i) = i
{-# INLINE fromSInteger #-}

withKnownInteger_
  :: forall i rep (x :: TYPE rep)
  .  SInteger i
  -> (KnownInteger_ i => x)
  -> x
withKnownInteger_ = withDict @(KnownInteger_ i)

-- | Convert an explicit @'SInteger' i@ value into an implicit
-- @'KnownInteger' i@ constraint.
withKnownInteger
  :: forall i rep (x :: TYPE rep)
  .  SInteger i
  -> (KnownInteger i => x)
  -> x
withKnownInteger si x
  | i <- fromSInteger si
  , a :: Natural <- fromInteger (abs i)
  , L.SomeNat @a _ <- L.someNatVal a
    -- Safe because this module doesn't offer any tool for constructing
    -- non-normalized SIntegers. Very unsafe otherwise.
  , Refl <- unsafeCoerce Refl :: Normalize i :~: i
  , Refl <- unsafeCoerce Refl :: Abs i :~: a
  = withKnownInteger_ si x

-- | Convert a "Prelude".'P.Integer' number into an @'SInteger' n@ value,
-- where @n@ is a fresh type-level 'Integer'.
withSomeSInteger
  :: forall rep (x :: TYPE rep). P.Integer -> (forall i. SInteger i -> x) -> x
withSomeSInteger i k = k (UnsafeSInteger i)
-- It's very important to keep this NOINLINE! See the docs at "GHC.TypeNats"
{-# NOINLINE withSomeSInteger #-}

--------------------------------------------------------------------------------

type instance Sing = SInteger

instance (KnownInteger i) => SingI (i :: Integer) where
  sing = integerSing
  {-# INLINE sing #-}

instance SingKind Integer where
  type Demote Integer = P.Integer
  fromSing = fromSInteger
  {-# INLINE fromSing #-}
  toSing i = withSomeSInteger i SomeSing
  {-# INLINE toSing #-}

instance SDecide Integer where
  UnsafeSInteger l %~ UnsafeSInteger r =
    -- This is safe because this library doesn't expose any tool to construct
    -- non-normalized SInteger . Otherwise, very unsafe.
    case l P.== r of
      True  -> Proved (unsafeCoerce Refl)
      False -> Disproved (\Refl -> error "KindInteger.Integer: SDecide")

--------------------------------------------------------------------------------

data Round
  = RoundUp
  -- ^ Round __up__ towards positive infinity.
  | RoundDown
  -- ^ Round __down__ towards negative infinity.  Also known as "Prelude"'s
  -- 'P.floor'. This is the type of rounding used by "Prelude"'s 'P.div',
  -- 'P.mod', 'P.divMod', 'L.Div', 'L.Mod'.
  | RoundZero
  -- ^ Round towards __zero__.  Also known as "Prelude"'s 'P.truncate'. This is
  -- the type of rounding used by "Prelude"'s 'P.quot', 'P.rem', 'P.quotRem'.
  | RoundAway
  -- ^ Round __away__ from zero.
  | RoundHalfUp
  -- ^ Round towards the closest integer. If __half__way between two integers,
  -- round __up__ towards positive infinity.
  | RoundHalfDown
  -- ^ Round towards the closest integer. If __half__way between two integers,
  -- round __down__ towards negative infinity.
  | RoundHalfZero
  -- ^ Round towards the closest integer. If __half__way between two integers,
  -- round towards __zero__.
  | RoundHalfAway
  -- ^ Round towards the closest integer. If __half__way between two integers,
  -- round __away__ from zero.
  | RoundHalfEven
  -- ^ Round towards the closest integer. If __half__way between two integers,
  -- round towards the closest __even__ integer. Also known as "Prelude"'s
  -- 'P.round'.
  | RoundHalfOdd
  -- ^ Round towards the closest integer. If __half__way between two integers,
  -- round towards the closest __odd__ integer.
  deriving (Eq, Ord, Show, Read, Enum, Bounded)


-- | Divide @a@ by @a@ using the specified 'Round'ing.
-- Return the quotient @q@. See 'divRem'.
div :: Round
    -> P.Integer  -- ^ Dividend @a@.
    -> P.Integer  -- ^ Divisor @b@.
    -> P.Integer  -- ^ Quotient @q@.
div r a b = fst (divRem r a b)

-- | Divide @a@ by @a@ using the specified 'Round'ing.
-- Return the remainder @m@. See 'divRem'.
rem :: Round
    -> P.Integer  -- ^ Dividend @a@.
    -> P.Integer  -- ^ Divisor @b@.
    -> P.Integer  -- ^ Remainder @m@.
rem r a b = snd (divRem r a b)

-- | Divide @a@ by @a@ using the specified 'Round'ing.
-- Return the quotient @q@ and the remainder @m@.
--
-- @
-- forall (r :: 'Round') (a :: 'P.Integer') (b :: 'P.Integer').
--   (b 'P./=' 0) =>
--     case 'divRem' r a b of
--       (q, m) -> m 'P.==' a 'P.-' b 'P.*' q
-- @
divRem
  :: Round
  -> P.Integer  -- ^ Dividend @a@.
  -> P.Integer  -- ^ Divisor @b@.
  -> (P.Integer, P.Integer)  -- ^ Quotient @q@ and remainder @m@.
{-# NOINLINE divRem #-}
divRem RoundZero = \a (errDiv0 -> b) -> P.quotRem a b
divRem RoundDown = \a (errDiv0 -> b) -> P.divMod a b
divRem RoundUp = \a (errDiv0 -> b) -> _divRemRoundUpNoCheck a b
divRem RoundAway = \a (errDiv0 -> b) ->
  if xor (a < 0) (b < 0)
     then P.divMod a b
     else _divRemRoundUpNoCheck a b
divRem RoundHalfUp = _divRemHalf $ \_ _ up -> up
divRem RoundHalfDown = _divRemHalf $ \_ down _ -> down
divRem RoundHalfZero = _divRemHalf $ \neg down up ->
  if neg then up else down
divRem RoundHalfAway = _divRemHalf $ \neg down up ->
  if neg then down else up
divRem RoundHalfEven = _divRemHalf $ \_ down up ->
  if even (fst down) then down else up
divRem RoundHalfOdd = _divRemHalf $ \_ down up ->
  if odd (fst down) then down else up

_divRemRoundUpNoCheck :: P.Integer -> P.Integer -> (P.Integer, P.Integer)
_divRemRoundUpNoCheck a b =
  let q = negate (P.div (negate a) b)
  in (q, a - b * q)
{-# INLINE _divRemRoundUpNoCheck #-}

_divRemHalf
  :: (Bool ->
      (P.Integer, P.Integer) ->
      (P.Integer, P.Integer) ->
      (P.Integer, P.Integer))
  -- ^ Negative -> divRem RoundDown -> divRem RoundDown -> Result
  -> P.Integer  -- ^ Dividend
  -> P.Integer  -- ^ Divisor
  -> (P.Integer, P.Integer)
_divRemHalf f = \a (errDiv0 -> b) ->
  let neg  = xor (a < 0) (b < 0)
      down = P.divMod a b
      up   = _divRemRoundUpNoCheck a b
  in  case compare (a P.% b - toRational (fst down)) (1 P.:% 2) of
        LT -> down
        GT -> up
        EQ -> f neg down up
{-# INLINE _divRemHalf #-}

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

--------------------------------------------------------------------------------
-- Rational tools

data Rat = Rat Integer Natural

type family R (n :: Integer) (d :: Natural) :: Rat where
  R (P n) d = RatNormalize ('Rat (P n) d)
  R (N n) d = RatNormalize ('Rat (N n) d)

type family RatNormalize (r :: Rat) :: Rat where
  RatNormalize ('Rat _ 0) =
    L.TypeError ('L.Text "KindInteger: Denominator is 0")
  RatNormalize ('Rat Z _) = 'Rat Z 1
  RatNormalize ('Rat (P 0) _) = 'Rat Z 1
  RatNormalize ('Rat (N 0) _) = 'Rat Z 1
  RatNormalize ('Rat (P n) d) = 'Rat (P (L.Div n (NatGCD n d)))
                                     (L.Div d (NatGCD n d))
  RatNormalize ('Rat (N n) d) = 'Rat (N (L.Div n (NatGCD n d)))
                                     (L.Div d (NatGCD n d))

type family RatAbs (a :: Rat) :: Rat where
  RatAbs ('Rat n d) = RatNormalize ('Rat (P (Abs n)) d)

type RatAdd (a :: Rat) (b :: Rat) =
  RatNormalize (RatAdd_ (RatNormalize a) (RatNormalize b)) :: Rat
type family RatAdd_ (a :: Rat) (b :: Rat) :: Rat where
  RatAdd_ ('Rat an ad) ('Rat bn bd) = 'Rat (an * P bd + bn * P ad) (ad L.* bd)

type family RatNegate (a :: Rat) :: Rat where
  RatNegate ('Rat n d) = RatNormalize ('Rat (Negate n) d)

type RatMinus (a :: Rat) (b :: Rat) = RatAdd a (RatNegate b)

type instance Compare (a :: Rat) (b :: Rat) = RatCmp a b
type RatCmp (a :: Rat) (b :: Rat) =
  RatCmp_ (RatNormalize a) (RatNormalize b) :: Ordering
type family RatCmp_ (a :: Rat) (b :: Rat) :: Ordering where
  RatCmp_ a a = 'EQ
  RatCmp_ ('Rat an ad) ('Rat bn bd) = CmpInteger (an * P bd) (bn * P ad)

-- | ''True' if the distance between @a@ and @b@ is less than /0.5/.
type HalfLT (a :: Rat) (b :: Integer) =
  (RatAbs (RatMinus a ('Rat b 1))) <? ('Rat (P 1) 2) :: Bool

errDiv0 :: P.Integer -> P.Integer
errDiv0 0 = Ex.throw Ex.DivideByZero
errDiv0 i = i

pSkipSpaces1 :: ReadP.ReadP ()
pSkipSpaces1 = void $ ReadP.munch1 Char.isSpace
