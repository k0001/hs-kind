{-# LANGUAGE StrictData #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE UndecidableInstances #-}

-- | This module provides a type-level representation for term-level
-- 'P.Integer's. This type-level representation is also named 'P.Integer',
-- So import this module qualified to avoid name conflicts.
--
-- @
-- import "KindInteger" qualified as KI
-- @
--
-- The implementation details are similar to the ones for type-level 'Natural's
-- as of @base-4.18@ and @singletons-base-3.1.1@, and they will continue to
-- evolve together with @base@ and @singletons-base@, trying to more or less
-- follow their API.
module KindInteger {--}
  ( -- * Integer kind
    Integer
  , type Z, pattern SZ
  , type N, pattern SN
  , type P, pattern SP
  , FromNatural, fromNatural, sFromNatural
  , KnownInteger
  , integerSing
  , integerVal
  , withKnownInteger
  , SomeInteger(..)
  , someIntegerVal
  , SInteger
  , pattern SInteger
  , fromSInteger
  , withSomeSInteger
    -- * Normalization
  , Normalize, normalize, sNormalize
  , sNormalizeRefl

    -- * Show
    --
    -- | Besides the following /\*Lit/ tools, 'P.PShow' and 'P.SShow' can
    -- be used to display as "Prelude".'P.Integer' does.
  , ShowLit, showLit, sShowLit
  , ShowsLit, showsLit, sShowsLit
  , ShowsPrecLit, showsPrecLit, sShowsPrecLit
  , readPrecLit

    -- * Arithmethic
    --
    -- | Additional arithmetic operations are provided through the 'P.PNum'
    -- and 'P.SNum' instances. Notably 'P.Abs', 'P.sAbs', 'P.Negate',
    -- 'P.sNegate', 'P.Signum', 'P.sSignum', 'P.+', 'P.-', 'P.*', 'P.%+',
    -- 'P.%-', 'P.%*'.
  , type (^), (%^)
  , Odd, sOdd
  , Even, sEven
  , Abs, sAbs, sAbsRefl
  , GCD, sGCD
  , LCM, sLCM
  , Log2, sLog2
  , Div, sDiv, div
  , Rem, sRem, rem
  , DivRem, sDivRem, divRem
    -- ** Rounding
  , Round(..)
  , SRound(..)

    -- * Comparisons
    --
    -- | Additional comparison tools are available at 'SDdecide',
    -- 'TestEquality', 'TestCoercion', 'P.PEq', 'P.SEq', 'P.POrd', 'P.SOrd'
    -- and 'Compare'.
  , CmpInteger
  , cmpInteger
  , sameInteger

    -- * Defunctionalization
  , ZSym0
  , NSym0, NSym1
  , PSym0, PSym1
  , FromNaturalSym0, FromNaturalSym1
  , KnownIntegerSym0, KnownIntegerSym1
  , NormalizeSym0, NormalizeSym1
  , type (^@#@$), type (^@#@$$), type (^@#@$$$)
  , OddSym0, OddSym1
  , EvenSym0, EvenSym1
  , AbsSym0, AbsSym1
  , GCDSym0, GCDSym1
  , LCMSym0, LCMSym1
  , Log2Sym0, Log2Sym1
  , DivSym0, DivSym1, DivSym2, DivSym3
  , RemSym0, RemSym1, RemSym2, RemSym3
  , DivRemSym0, DivRemSym1, DivRemSym2, DivRemSym3
  , RoundUpSym0
  , RoundDownSym0
  , RoundZeroSym0
  , RoundAwaySym0
  , RoundHalfUpSym0
  , RoundHalfDownSym0
  , RoundHalfZeroSym0
  , RoundHalfAwaySym0
  , RoundHalfEvenSym0
  , RoundHalfOddSym0
  ) --}
  where

import Control.Applicative
import Control.Exception qualified as Ex
import Control.Monad
import Data.Bits
import Data.Bool.Singletons (SBool(..))
import Data.Char qualified as Char
import Data.Eq.Singletons qualified as EqS
import Data.Maybe
import Data.Ord.Singletons qualified as OrdS
import Data.Proxy
import Data.Singletons
import Data.Singletons.Decide
import Data.Singletons.TH
import Data.String
import Data.Type.Bool (If)
import Data.Type.Coercion
import Data.Type.Equality (TestEquality(..))
import Data.Type.Ord
import GHC.Base (WithDict(..))
import GHC.Exts (TYPE)
import GHC.Num.Integer (integerLog2)
import GHC.Real qualified as P
import GHC.Show (appPrec1)
import GHC.TypeLits qualified as L hiding (someNatVal)
import GHC.TypeLits.Singletons qualified as L (SNat)
import GHC.TypeNats qualified as L (someNatVal)
import Numeric.Natural (Natural)
import Prelude hiding (Show, Integer, (==), (/=), div, rem)
import Prelude qualified as P
import Prelude.Singletons qualified as P
import Text.ParserCombinators.ReadP qualified as ReadP
import Text.ParserCombinators.ReadPrec qualified as ReadPrec
import Text.Read qualified as Read
import Unsafe.Coerce (unsafeCoerce)

import KindInteger.Round

--------------------------------------------------------------------------------

-- | Type-level version of 'P.Integer', only used as a /kind/.
--
-- * Zero is represented as t'Z'.
--
-- * A positive number /+x/ is represented as @t'P' x@.
--
-- * A negative number /-x/ is represented as @t'N' x@.

-- Note: We could use a better internal representation that guarantees that
-- Negative and Positive don't contain zero. However, this one looks better
-- on compiler errors, that's why we use it. The KindInteger module doesn't
-- export anything that would allow the construction of a non-'Normalize'd
-- 'Integer' anyway, so throughout this module we can make some assumptions.
data Integer
  = Z         -- ^ This is the only way to represent 0.
  | N Natural -- ^ Never 0.
  | P Natural -- ^ Never 0.

-- | Zero is represented as t'Z'.
type Z = 'Z :: Integer
-- | A negative number /-x/ is represented as @t'N' x@.
type N (x :: Natural) = 'N x :: Integer
-- | A positive number /+x/ is represented as @t'P' x@.
type P (x :: Natural) = 'P x :: Integer

-- | @'SZ' == 'sing' \@Z@
pattern SZ :: SInteger Z
pattern SZ <- UnsafeSInteger _
  where SZ = UnsafeSInteger 0
{-# COMPLETE SZ #-}

-- | @'SP' ('sing' @1) == 'sing' \@(P 1)@
pattern SP :: (0 < x) => (L.KnownNat x) => L.SNat x -> SInteger (P x)
pattern SP x <- ((\ !i@SInteger -> SomeKnownNat (sAbs i)) -> SomeKnownNat x)
  where SP = UnsafeSInteger . toInteger . fromSing
{-# COMPLETE SP #-}

-- | @'SN' ('sing' @1) == 'sing' \@(N 1)@
pattern SN :: (0 < x) => (L.KnownNat x) => L.SNat x -> SInteger (N x)
pattern SN x <- ((\ !i@SInteger -> SomeKnownNat (sAbs i)) -> SomeKnownNat x)
  where SN = UnsafeSInteger . negate . toInteger . fromSing
{-# COMPLETE SN #-}

-- | Only used to implement 'SP' and 'SN'.
data SomeKnownNat n = (L.KnownNat n) => SomeKnownNat (L.SNat n)

--------------------------------------------------------------------------------

type ShowsPrec (p :: L.Natural) (i :: Integer) (s :: L.Symbol)
  = ShowsPrec_ p (Normalize i) s :: L.Symbol
type family ShowsPrec_ (p :: L.Natural) (i :: Integer) (s :: L.Symbol) :: L.Symbol where
  ShowsPrec_ p (N n) s = (P.ShowCharSym1 '-' P..@#@$$$ P.ShowsSym1 n) @@ s
  ShowsPrec_ _ (P n) s = P.Shows n s
  ShowsPrec_ _ Z     s = P.Shows 0 s

-- | Displays @i@ as it would show literally as a term.
--
-- @
-- 'P.Show_' ( t'N' 1) ~ \"-1\"
-- 'P.Show_'   t'Z'    ~  \"0\"
-- 'P.Show_' ( t'P' 1) ~  \"1\"
-- @
instance P.PShow Integer where
  type ShowsPrec p i s = ShowsPrec p i s

-- | Displays @i@ as it would show literally as a term.
--
-- @
-- 'fromSing' \@('P.sShow_' ('SN' ('sing' \@1))) == \"-1\"
-- 'fromSing' \@('P.sShow_' 'SZ')             ==  \"0\"
-- 'fromSing' \@('P.sShow_' ('SP' ('sing' \@1))) ==  \"1\"
-- @
instance P.SShow Integer where
  sShowsPrec _ si ss =
    withSomeSing (fromString (show (fromSing si)) <> fromSing ss) unsafeCoerce

--------------------------------------------------------------------------------

-- | Displays @i@ as it would show literally as a type.
--
-- @
-- 'P.ShowLit' ( t'N' 1) ~ \"N 1\"
-- 'P.ShowLit'   t'Z'    ~ \"Z\"
-- 'P.ShowLit' ( t'P' 1) ~ \"P 1\"
-- @
type ShowLit (i :: Integer) = ShowsLit i "" :: L.Symbol
-- | Displays @i@ as it would show literally as a type. Behaves like 'P.Shows'.
type ShowsLit (i :: Integer) (s :: L.Symbol) = ShowsPrecLit 0 i s :: L.Symbol
-- | Displays @i@ as it would show literally as a type. Behaves like 'P.ShowsPrec'.
type ShowsPrecLit (p :: L.Natural) (i :: Integer) (s :: L.Symbol)
  = ShowsPrecLit_ p (Normalize i) s :: L.Symbol
type family ShowsPrecLit_ (p :: L.Natural) (i :: Integer) (s :: L.Symbol) :: L.Symbol where
  ShowsPrecLit_ p (N n) s =
    P.ShowParen (p P.>= 11) (P.ShowStringSym1 "N " P..@#@$$$ P.ShowsSym1 n) s
  ShowsPrecLit_ p (P n) s =
    P.ShowParen (p P.>= 11) (P.ShowStringSym1 "P " P..@#@$$$ P.ShowsSym1 n) s
  ShowsPrecLit_ _ Z s = P.ShowString "Z" s

-- | Singleton version of 'ShowLit'.
--
-- @
-- 'fromSing' \@('sShowLit' ('SN' ('sing' \@1))) == \"N 1\"
-- 'fromSing' \@('sShowLit' 'SZ')             == \"Z"
-- 'fromSing' \@('sShowLit' ('SP' ('sing' \@1))) == \"P 1\"
-- @
sShowLit :: SInteger i -> Sing (ShowLit i)
sShowLit si = sShowsLit si (sing @"")

-- | Demoted version of 'ShowLit'.
showLit :: P.Integer -> String
showLit i = showsLit i ""

-- | Singleton version of 'ShowsLit'.
sShowsLit :: SInteger i -> Sing (s :: P.Symbol) -> Sing (ShowsLit i s)
sShowsLit = sShowsPrecLit (sing @0)

-- | Demoted version of 'ShowsLit'.
showsLit :: P.Integer -> ShowS
showsLit = showsPrecLit 0

-- | Singleton version of 'ShowsPrecLit'.
sShowsPrecLit
  :: L.SNat p -> SInteger i -> Sing (s :: P.Symbol) -> Sing (ShowsPrecLit p i s)
sShowsPrecLit sp si ss =
  let p = fromMaybe (error "sShowsPrecLit: invalid precedence")
                    (toIntegralSized (fromSing sp))
      t = fromString (showsPrecLit p (fromSing si) "")
  in withSomeSing (t <> fromSing ss) unsafeCoerce

-- | Demoted version of 'ShowsPrecLit'.
showsPrecLit :: Int -> P.Integer -> ShowS
showsPrecLit p | p < 0 = error "showsPrecLit: negative precedence"
showsPrecLit p = \case
  0         -> showChar 'Z'
  i | i > 0 -> showParen (p >= appPrec1) (showString "P " . shows i)
  i         -> showParen (p >= appPrec1) (showString "N " . shows (abs i))

-- | Inverse of 'showsPrecLit'.
readPrecLit :: ReadPrec.ReadPrec P.Integer
readPrecLit = Read.parens $ asum
    [ 0 <$ ReadPrec.lift (ReadP.char 'Z')
    , do ReadPrec.lift $ ReadP.char 'N' >> pSkipSpaces1
         n <- Read.parens (ReadPrec.lift pNatural)
         guard (n P./= 0)
         pure $ negate $ fromIntegral n
    , do ReadPrec.lift $ ReadP.char 'P' >> pSkipSpaces1
         n <- Read.parens (ReadPrec.lift pNatural)
         guard (n P./= 0)
         pure $ fromIntegral n
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
instance P.Show SomeInteger where
  showsPrec p (SomeInteger i) = showsPrec p (integerVal i)

-- | As for "Prelude".'P.Integer'.
instance Read SomeInteger where
  readPrec = fmap someIntegerVal Read.readPrec

--------------------------------------------------------------------------------

-- | Construct a type-level 'Integer' from a type-level 'Natural'.
type FromNatural (x :: Natural) = Normalize (P x) :: Integer

-- | Singleton version of 'FromNatural'.
sFromNatural :: L.SNat x -> SInteger (FromNatural x)
sFromNatural sx = withSomeSing (toInteger (fromSing sx)) unsafeCoerce

-- | Demoted version of 'FromNatural'.
fromNatural :: L.Natural -> P.Integer
fromNatural = fromIntegral

-- | Make sure /zero/ is represented as @t'P' 0@, not as @t'N' 0@
--
-- Notice that all type-families in the "KindInteger" module can readily handle
-- non-'Normalize'd inputs. You may need to use 'Normalize' when dealing with
-- 'KnownInteger', though.
type family Normalize (i :: Integer) :: Integer where
  Normalize (N 0) = Z
  Normalize (P 0) = Z
  Normalize i     = i

-- | Singleton version of 'Normalize'.
--
-- This function is just 'id'entity, since 'SInteger's are always normalized.
sNormalize :: SInteger r -> SInteger (Normalize r)
sNormalize !sr | Refl <- sNormalizeRefl sr = sr

-- | Demoted version of 'Normalize'. This function is just 'id'entity,
-- since "Prelude".'P.Integer's are always 'Normalize'd.
normalize :: P.Integer -> P.Integer
normalize = id
{-# INLINE normalize #-}

-- | 'SInteger's always contain a 'Normalize'd 'Integer'.
sNormalizeRefl :: SInteger r -> (r :~: Normalize r)
sNormalizeRefl !_ = unsafeCoerce Refl

-- | Construct a 'Normalize'd t'N'egative type-level 'Integer'.
--
-- | To be used for producing all negative outputs in this module.
type NN (a :: Natural) = Normalize (N a) :: Integer

-- | Construct a 'Normalize'd t'P'ositive type-level 'Integer'.
--
-- To be used for producing all positive outputs in this module.
type PP (a :: Natural) = Normalize (P a) :: Integer

--------------------------------------------------------------------------------

infixl 6 +, -
infixl 7 *, `Div`, `Rem`
infixr 8 ^

-- | Whether a type-level 'Natural' is odd. /Zero/ is not considered odd.
type Odd (x :: Integer) = L.Mod (Abs x) 2 P.== 1 :: Bool

-- | Singleton version of 'Odd'.
sOdd :: SInteger i -> SBool (Odd i)
sOdd si | odd (fromSing si) = unsafeCoerce STrue
        | otherwise         = unsafeCoerce SFalse

-- | Whether a type-level 'Natural' is even. /Zero/ is considered even.
type Even (x :: Integer) = L.Mod (Abs x) 2 P.== 0 :: Bool

-- | Singleton version of 'Even'.
sEven :: SInteger i -> SBool (Even i)
sEven si | even (fromSing si) = unsafeCoerce STrue
         | otherwise          = unsafeCoerce SFalse

-- | Negation of type-level 'Integer's.
type Negate (x :: Integer) = Negate_ (Normalize x) :: Integer
type family Negate_ (x :: Integer) :: Integer where
  Negate_ Z     = Z
  Negate_ (P x) = N x
  Negate_ (N x) = P x

-- | Singleton version of 'Negate'.
sNegate :: SInteger i -> SInteger (Negate i)
sNegate si = UnsafeSInteger (negate (fromSing si))

-- | Sign of type-level 'Integer's.
--
-- * t'Z' if zero.
--
-- * @t'P' 1@ if positive.
--
-- * @t'N' 1@ if negative.
type Signum (x :: Integer) = Signum_ (Normalize x) :: Integer
type family Signum_ (x :: Integer) :: Integer where
  Signum_  Z    = Z
  Signum_ (P _) = P 1
  Signum_ (N _) = N 1

-- | Singleton version of 'Sign'.
sSignum :: SInteger i -> SInteger (Signum i)
sSignum si = case compare (fromSing si) 0 of
             LT -> UnsafeSInteger (-1)
             EQ -> UnsafeSInteger 0
             GT -> UnsafeSInteger 1


-- | Absolute value of a type-level 'Integer', as a type-level 'Natural'.
type Abs (x :: Integer) = Abs_ (Normalize x) :: Natural
type family Abs_ (x :: Integer) :: Natural where
  Abs_  Z    = 0
  Abs_ (P x) = x
  Abs_ (N x) = x

-- | Singleton version of 'Abs'.
sAbs :: SInteger i -> L.SNat (Abs i)
sAbs si = withSomeSing
            (fromInteger (abs (fromSing si)) :: Natural)
            unsafeCoerce

-- | Relationship between a type-level 'Integer' and its
-- 'Abs'olute 'Natural' amount.
sAbsRefl :: SInteger i -> (i :~: FromNatural (Abs i))
sAbsRefl !_ = unsafeCoerce Refl


infixl 6 %+
-- | Singleton version of '+'.
(%+) :: SInteger a -> SInteger b -> SInteger (a + b)
(%+) sa sb = UnsafeSInteger (fromSing sa + fromSing sb)

-- | Addition of type-level 'Integer's.
type (a :: Integer) + (b :: Integer) = Add_ (Normalize a) (Normalize b) :: Integer
type family Add_ (a :: Integer) (b :: Integer) :: Integer where
  Add_ Z     b     = b
  Add_ a     Z     = a
  Add_ (P a) (P b) = PP (a L.+ b)
  Add_ (N a) (N b) = NN (a L.+ b)
  Add_ (P a) (N b) = If (b <=? a) (PP (a L.- b)) (NN (b L.- a))
  Add_ (N a) (P b) = Add_ (P b) (N a)

infixl 7 %*
-- | Singleton version of '*'.
(%*) :: SInteger a -> SInteger b -> SInteger (a * b)
(%*) sa sb = UnsafeSInteger (fromSing sa * fromSing sb)

-- | Multiplication of type-level 'Integer's.
type (a :: Integer) * (b :: Integer) = Mul_ (Normalize a) (Normalize b) :: Integer
type family Mul_ (a :: Integer) (b :: Integer) :: Integer where
  Mul_ Z     b     = Z
  Mul_ a     Z     = Z
  Mul_ (P a) (P b) = PP (a L.* b)
  Mul_ (N a) (N b) = Mul_ (P a) (P b)
  Mul_ (P a) (N b) = NN (a L.* b)
  Mul_ (N a) (P b) = Mul_ (P a) (N b)

infixr 8 %^
-- | Singleton version of '^'.
(%^) :: SInteger a -> SInteger b -> SInteger (a ^ b)
(%^) sa sb = UnsafeSInteger (fromSing sa ^ fromSing sb)

-- | Exponentiation of type-level 'Integer's.
--
-- * @t'Z' '^' t'Z'@ doesn't type-check.
--
-- * @_ '^' t'N' _@ doesn't type-check.
type (a :: Integer) ^ (b :: Integer) = Pow_ (Normalize a) (Normalize b) :: Integer
type family Pow_ (a :: Integer) (b :: Integer) :: Integer where
  Pow_ _     (N _) = L.TypeError ('L.Text "KindInteger.(^): Negative exponent")
  Pow_ Z      Z    = L.TypeError ('L.Text "KindInteger.(^): (Z ^ Z) is undefined")
  Pow_ Z      _    = Z
  Pow_ _      Z    = P 1
  Pow_ (P a) (P b) = P (a L.^ b)
  Pow_ (N a) (P b) = N (a L.^ b)

infixl 6 %-
-- | Singleton version of '-'.
(%-) :: SInteger a -> SInteger b -> SInteger (a - b)
(%-) sa sb = UnsafeSInteger (fromSing sa - fromSing sb)

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
  Div__ 'RoundDown (N a) b = NN (If (b L.* L.Div a b P.== a)
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

-- | Singleton version of 'Log2'.
sLog2 :: SInteger i -> L.SNat (Log2 i)
sLog2 si = withSomeSing
             (fromIntegral (integerLog2 (fromSing si)) :: Natural)
             unsafeCoerce

-- | Log base 2 ('floor'ed) of type-level 'Integer's.
--
-- * Logarithm of /zero/ doesn't type-check.
--
-- * Logarithm of negative number doesn't type-check.
type Log2 (a :: Integer) = Log2_ (Normalize a) :: Natural
type family Log2_ (a :: Integer) :: Natural where
  Log2_ Z = L.TypeError ('L.Text "KindInteger.Log2: Logarithm of zero")
  Log2_ (P a) = L.Log2 a
  Log2_ (N a) = L.TypeError
    ('L.Text "KindInteger.Log2: Logarithm of negative number")


-- | Singleton version of 'GCD'.
sGCD :: SInteger a -> SInteger b -> L.SNat (GCD a b)
sGCD sa sb = withSomeSing
               (fromInteger (gcd (fromSing sa) (fromSing sb)) :: Natural)
               unsafeCoerce

-- | Greatest Common Divisor of type-level 'Integer' numbers @a@ and @b@.
--
-- Returns a 'Natural', since the Greatest Common Divisor is always positive.
type GCD (a :: Integer) (b :: Integer) = NatGCD (Abs a) (Abs b) :: Natural

-- | Greatest Common Divisor of type-level 'Natural's @a@ and @b@.
type family NatGCD (a :: Natural) (b :: Natural) :: Natural where
  NatGCD a 0 = a
  NatGCD a b = NatGCD b (L.Mod a b)

-- | Singleton version of 'LCM'.
sLCM :: SInteger a -> SInteger b -> L.SNat (LCM a b)
sLCM sa sb = withSomeSing
               (fromInteger (lcm (fromSing sa) (fromSing sb)) :: Natural)
               unsafeCoerce

-- | Least Common Multiple of type-level 'Integer' numbers @a@ and @b@.
--
-- Returns a 'Natural', since the Least Common Multiple is always positive.
type LCM (a :: Integer) (b :: Integer) = NatLCM (Abs a) (Abs b) :: Natural

-- | Least Common Multiple of type-level 'Natural's @a@ and @b@.
type NatLCM (a :: Natural) (b :: Natural) =
  L.Div a (NatGCD a b) L.* b :: Natural

instance P.PNum Integer where
  type a + b = a + b
  type a - b = a - b
  type a * b = a * b
  type Negate a = Negate_ (Normalize a)
  type Abs a = Normalize (P (Abs a))
  type Signum a = Signum_ (Normalize a)
  type FromInteger a = Normalize (P a)

instance P.SNum Integer where
  (%+) = (%+)
  (%-) = (%-)
  (%*) = (%*)
  sNegate = sNegate
  sAbs = sFromNatural . sAbs
  sSignum = sSignum
  sFromInteger = sFromNatural

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

instance OrdS.POrd Integer where
  type Compare a b = CmpInteger a b

instance OrdS.SOrd Integer where
  sCompare sa sb = case compare (fromSing sa) (fromSing sb) of
    LT -> unsafeCoerce OrdS.SLT
    EQ -> unsafeCoerce OrdS.SEQ
    GT -> unsafeCoerce OrdS.SGT

instance EqS.PEq Integer where
  type a == b = OrdCond (Compare a b) 'False 'True 'False

instance EqS.SEq Integer where
  sa %== sb
    | fromSing sa P.== fromSing sb = unsafeCoerce STrue
    | otherwise                    = unsafeCoerce SFalse

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
  EQ | Refl <- (unsafeCoerce Refl :: a :~: b)
     , Refl <- (unsafeCoerce Refl :: CmpInteger a b :~: 'EQ) -> EQI
  LT | Refl <- (unsafeCoerce Refl :: CmpInteger a b :~: 'LT) -> LTI
  GT | Refl <- (unsafeCoerce Refl :: CmpInteger a b :~: 'GT) -> GTI

--------------------------------------------------------------------------------

-- | Singleton type for a type-level 'Integer' @i@.
newtype SInteger (i :: Integer)
  = UnsafeSInteger P.Integer
    -- ^ Note that we could use SZ, SN and SP as constructor to make 'SInteger'
    -- internals type safe. We don't do it because this is more performant.
    -- The 'KnownRational' constraint in 'integerSing' makes everything safe.
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
-- f si\@'SInteger' = /... both (si :: 'SInteger' i) and ('KnownInteger' i) in scope .../
-- @
pattern SInteger :: forall i. () => KnownInteger i => SInteger i
pattern SInteger <- (knownIntegerInstance -> KnownIntegerInstance)
  where SInteger = integerSing
{-# COMPLETE SInteger #-}

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

instance P.Show (SInteger i) where
  showsPrec p i =
    showParen (p >= appPrec1) $
      showString "SInteger @" .
      showsPrecLit appPrec1 (fromSing i)

instance forall i. KnownInteger i => Read (SInteger i) where
  readPrec = ReadPrec.lift $ do
    let si = integerSing @i
    _ <- ReadP.string "SInteger" >> pSkipSpaces1
    _ <- ReadP.char '@'
    _ <- ReadP.string (showsPrecLit appPrec1 (fromSing si) "")
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
  | Refl <- sNormalizeRefl si
  , L.SomeNat @a _ <- L.someNatVal (fromSing (sAbs si))
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
  l %~ r =
    -- This is safe because this library doesn't expose any tool to construct
    -- non-normalized SInteger . Otherwise, very unsafe.
    case fromSing l P.== fromSing r of
      True  -> Proved (unsafeCoerce Refl)
      False -> Disproved (\Refl -> error "KindInteger.Integer: SDecide")

--------------------------------------------------------------------------------

-- | Demoted version of 'Div'.
--
-- Throws 'Ex.DivdeByZero' where 'Div' would fail to type-check.
div :: Round
    -> P.Integer  -- ^ Dividend @a@.
    -> P.Integer  -- ^ Divisor @b@.
    -> P.Integer  -- ^ Quotient @q@.
div r a b = fst (divRem r a b)

-- | Singleton version of 'Div'.
sDiv :: SRound r
     -> SInteger a  -- ^ Dividend.
     -> SInteger b  -- ^ Divisor.
     -> SInteger (Div r a b)
sDiv sr sa sb = fst (sDivRem sr sa sb)

-- | Demoted version of 'Rem'.
--
-- Throws 'Ex.DivdeByZero' where 'Div' would fail to type-check.
rem :: Round
    -> P.Integer  -- ^ Dividend @a@.
    -> P.Integer  -- ^ Divisor @b@.
    -> P.Integer  -- ^ Remainder @m@.
rem r a b = snd (divRem r a b)

-- | Singleton version of 'Rem'.
sRem :: SRound r
     -> SInteger a  -- ^ Dividend.
     -> SInteger b  -- ^ Divisor.
     -> SInteger (Rem r a b)
sRem sr sa sb = snd (sDivRem sr sa sb)

-- | Demoted version of 'DivRem'.
--
-- Throws 'Ex.DivdeByZero' where 'Div' would fail to type-check.
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
  -- ^ Negative -> divRem RoundDown -> divRem RoundUp -> Result
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

-- | Singleton version of 'DivRem'.
sDivRem :: SRound r
        -> SInteger a   -- ^ Dividend.
        -> SInteger b   -- ^ Divisor.
        -> (SInteger (Div r a b), SInteger (Rem r a b))
sDivRem sr sa sb =
  let (q, m) = divRem (fromSing sr) (fromSing sa) (fromSing sb)
  in withSomeSing q $ \sq ->
     withSomeSInteger m $ \sm ->
     (unsafeCoerce sq, unsafeCoerce sm)

--------------------------------------------------------------------------------
-- Rational tools necessary to support 'HalfLT'

data Rat = Rat Integer Natural

type family R (n :: Integer) (d :: Natural) :: Rat where
  R Z     d = RatNormalize ('Rat Z      d)
  R (P n) d = RatNormalize ('Rat (PP n) d)
  R (N n) d = RatNormalize ('Rat (NN n) d)

type family RatNormalize (r :: Rat) :: Rat where
  RatNormalize ('Rat n d) = RatNormalize_ ('Rat (Normalize n) d)
type family RatNormalize_ (r :: Rat) :: Rat where
  RatNormalize_ ('Rat _ 0) = L.TypeError ('L.Text "Denominator is 0")
  RatNormalize_ ('Rat Z _) = 'Rat Z 1
  RatNormalize_ ('Rat (P n) d) = 'Rat (PP (L.Div n (NatGCD n d)))
                                      (L.Div d (NatGCD n d))
  RatNormalize_ ('Rat (N n) d) = 'Rat (NN (L.Div n (NatGCD n d)))
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

--------------------------------------------------------------------------------

errDiv0 :: P.Integer -> P.Integer
errDiv0 0 = Ex.throw Ex.DivideByZero
errDiv0 i = i

pSkipSpaces1 :: ReadP.ReadP ()
pSkipSpaces1 = void $ ReadP.munch1 Char.isSpace

--------------------------------------------------------------------------------

$(genDefunSymbols
   [ ''Z, ''N, ''P, ''KnownInteger, ''Normalize, ''FromNatural, ''(^)
   , ''Odd , ''Even , ''Abs, ''GCD , ''LCM , ''Log2 , ''Div, ''Rem, ''DivRem
   ])
