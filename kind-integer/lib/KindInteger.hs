{-# LANGUAGE StrictData #-}
{-# LANGUAGE TypeFamilyDependencies #-}
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
  ( -- * Integer
    Integer
  , type Z, pattern SZ
  , type N, pattern SN
  , type P, pattern SP
  , FromNatural, sFromNatural
  , Fold, sFold
    -- * SInteger
  , KnownInteger
  , Normalized
  , integerSing
  , integerVal
  , withKnownInteger
  , SomeInteger(..)
  , someIntegerVal
  , SInteger
  , pattern SInteger
  , fromSInteger
  , withSomeSInteger
    -- * Proofs
  , sNegateRefl
  , sZigZagRefl
  , sZagZigRefl

    -- * Show
    --
    -- | Besides the following /\*Lit/ tools, 'P.PShow' and 'P.SShow' can
    -- be used to display as "Prelude".'P.Integer' does.
  , ShowLit, showLit, sShowLit
  , ShowsLit, showsLit, sShowsLit
  , ShowsPrecLit, showsPrecLit, sShowsPrecLit
  , readPrecLit

    -- * Operations
    --
    -- | Additional arithmetic operations are provided through the 'P.PNum'
    -- and 'P.SNum' instances. Notably 'P.Abs', 'P.sAbs', 'P.Negate',
    -- 'P.sNegate', 'P.Signum', 'P.sSignum', 'P.+', 'P.-', 'P.*', 'P.%+',
    -- 'P.%-', 'P.%*'.
  , type (^), (%^)
  , Odd, sOdd
  , Even, sEven
  , Abs, sAbs
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
  , cmpInteger
  , sameInteger

    -- * Misc
  , ZigZag, sZigZag
  , ZagZig, sZagZig

    -- * Defunctionalization
  , ZSym0
  , NSym0, NSym1
  , PSym0, PSym1
  , FromNaturalSym0, FromNaturalSym1
  , KnownIntegerSym0, KnownIntegerSym1
  , NormalizedSym0, NormalizedSym1
  , FoldSym0, FoldSym1, FoldSym2, FoldSym3, FoldSym4
  , type (^@#@$), type (^@#@$$), type (^@#@$$$)
  , OddSym0, OddSym1
  , EvenSym0, EvenSym1
  , GCDSym0, GCDSym1
  , LCMSym0, LCMSym1
  , Log2Sym0, Log2Sym1
  , DivSym0, DivSym1, DivSym2, DivSym3
  , RemSym0, RemSym1, RemSym2, RemSym3
  , DivRemSym0, DivRemSym1, DivRemSym2, DivRemSym3
  , ShowLitSym0, ShowLitSym1
  , ShowsLitSym0, ShowsLitSym1, ShowsLitSym2
  , ShowsPrecLitSym0, ShowsPrecLitSym1, ShowsPrecLitSym2, ShowsPrecLitSym3
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
import Data.String
import Data.Type.Bool (If)
import Data.Type.Coercion
import Data.Type.Equality (TestEquality(..))
import Data.Type.Ord
import GHC.Base (WithDict(..))
import GHC.Exts (TYPE, Constraint, coerce)
import GHC.Num.Integer (integerLog2)
import GHC.Real qualified as P
import GHC.Show (appPrec1)
import GHC.TypeLits qualified as L
import GHC.TypeLits.Singletons qualified as L hiding (natVal)
import Numeric.Natural (Natural)
import Prelude hiding (Show, Integer, (==), (/=), div, rem)
import Prelude qualified as P
import Prelude.Singletons qualified as P
import Text.ParserCombinators.ReadP qualified as ReadP
import Text.ParserCombinators.ReadPrec qualified as ReadPrec
import Text.Read qualified as Read
import Text.Show.Singletons (AppPrec1)
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

-- __Note__: This representation is depressing. We hate it. It's not “total”,
-- because things such as @'N' 0@ and @'P' 0@ are not “valid”, and it's not
-- efficient, because we have to pay attention to three different constructors
-- rather than just one. The reason why we use this horrible representation
-- anyway is that type errors are easier to understand, since literals like
-- @'N' 2@ or 'Z' will show up in them. @'N' 0@ and @'P' 0@ will type-check
-- just fine on their own. However, tools in this module will reject them. For
-- example, there is no 'KnownInteger' instance for them, and 'Normalized'
-- will fail to type-check. Moreover, it is not possible to construct a
-- 'SInteger' for @'N' 0@ or @'P' 0@. If we could customize the way this
-- 'Integer' gets rendered in type-errors, we'd switch to a better internal
-- representation such as ZigZag. Also note that rather than exporting the
-- constructors directly, we export type-synonyms with the same name. This is
-- mostly so that users don't need to type the annoying preceeding tick.
data Integer
  = Z         -- ^ 0.
  | N Natural -- ^ Guaranteed to be greater than 0 only after going through
              -- 'KnownInteger', 'Normalized' or 'SInteger'.
  | P Natural -- ^ Guaranteed to be greater than 0 only after going through
              -- 'KnownInteger', 'Normalized' or 'SInteger'.

--------------------------------------------------------------------------------

-- | Zero is represented as t'Z'.
type Z = 'Z :: Integer

type ZSym0 :: Integer

type ZSym0 = Z

-- | A negative number /-x/ is represented as @t'N' x@.
--
-- While a standalone @t'N' 0@ type-checks, it is not considered valid,
-- so tools like 'KnownInteger' or 'Normalized' will reject it.
-- @t'N' 0@ itself is not rejected so that it can be used to pattern-match
-- against 'Integer's at the type-level if necessary.
type N (x :: Natural) = 'N x :: Integer

data NSym0 :: Natural ~> Integer
type NSym1 :: Natural -> Integer

type instance Apply NSym0 x = NSym1 x
type NSym1 x = N x

-- | A positive number /+x/ is represented as @t'P' x@.
--
-- While a standalone @t'P' 0@ type-checks, it is not considered valid,
-- so tools like 'KnownInteger' or 'Normalized' will reject it.
-- @t'P' 0@ itself is not rejected so that it can be used to pattern-match
-- against 'Integer's at the type-level if necessary.
type P (x :: Natural) = 'P x :: Integer

data PSym0 :: Natural ~> Integer
type PSym1 :: Natural -> Integer

type instance Apply PSym0 x = PSym1 x
type PSym1 x = P x

--------------------------------------------------------------------------------

-- | @'SZ' == 'sing' \@Z@
pattern SZ :: SInteger Z
pattern SZ <- UnsafeSInteger _
  where SZ = UnsafeSInteger 0
{-# COMPLETE SZ #-}

-- | @'SP' ('sing' \@1) == 'sing' \@(P 1)@
pattern SP :: (0 < x)
           => (KnownInteger (P x), L.KnownNat x)
           => Sing (x :: Natural)
           -> SInteger (P x)
pattern SP x <- (patternSP -> PatternSI x)
  where SP = UnsafeSInteger . toInteger . fromSing
{-# COMPLETE SP #-}

-- | @'SN' ('sing' \@1) == 'sing' \@(N 1)@
pattern SN :: (0 < x)
           => (KnownInteger (N x), L.KnownNat x)
           => Sing (x :: Natural)
           -> SInteger (N x)
pattern SN x <- (patternSN -> PatternSI x)
  where SN = UnsafeSInteger . negate . toInteger . fromSing
{-# COMPLETE SN #-}

-- | Internal. Used to implement 'SP' and 'SN'.
data PatternSI i n = (KnownInteger i, L.KnownNat n) => PatternSI (Sing n)

patternSI :: SInteger i -> PatternSI i (Abs i)
patternSI si@SInteger | sa <- sAbs si = L.withKnownNat sa (PatternSI sa)

patternSN :: SInteger (N x) -> PatternSI (N x) x
patternSN = unsafeCoerce . patternSI

patternSP :: SInteger (P x) -> PatternSI (P x) x
patternSP = unsafeCoerce . patternSI

--------------------------------------------------------------------------------

-- | @'Fold' z n p i@ evaluates to @z@ if @i@ is zero, otherwise applies @n@ to
-- the absolute value of @i@ if negative, or @p@ to the absolute value of @i@ if
-- positive. @t'N' 0@ and @t'P' 0@ fail to type-check.
type family Fold
     (z :: k)
     (n :: Natural ~> k)
     (p :: Natural ~> k)
     (i :: Integer)
     :: k where
  Fold _ _ _ (N 0) = L.TypeError ('L.Text "Use ‘Z’ instead of ‘N 0’.")
  Fold _ _ _ (P 0) = L.TypeError ('L.Text "Use ‘Z’ instead of ‘P 0’.")
  Fold z _ _  Z    = z
  Fold _ n _ (N x) = n @@ x
  Fold _ _ p (P x) = p @@ x

data FoldSym0 :: k ~> (Natural ~> k) ~> (Natural ~> k) ~> Integer ~> k
data FoldSym1 :: k -> (Natural ~> k) ~> (Natural ~> k) ~> Integer ~> k
data FoldSym2 :: k -> (Natural ~> k) -> (Natural ~> k) ~> Integer ~> k
data FoldSym3 :: k -> (Natural ~> k) -> (Natural ~> k) -> Integer ~> k
type FoldSym4 :: k -> (Natural ~> k) -> (Natural ~> k) -> Integer -> k

type instance Apply FoldSym0 z = FoldSym1 z
type instance Apply (FoldSym1 z) n = FoldSym2 z n
type instance Apply (FoldSym2 z n) p = FoldSym3 z n p
type instance Apply (FoldSym3 z n p) i = FoldSym4 z n p i
type FoldSym4 z n p i = Fold z n p i

-- | Singleton version of 'Fold'.
sFold
  :: SingKind k
  => Sing (z :: k)
  -> Sing (n :: Natural ~> k)
  -> Sing (p :: Natural ~> k)
  -> SInteger i
  -> Sing (Fold z n p i)
sFold sz sn sp si =
  case compare (fromSing si) 0 of
    EQ -> unsafeCoerce sz
    LT -> unsafeCoerce (sn @@ sAbs si)
    GT -> unsafeCoerce (sp @@ sAbs si)

--------------------------------------------------------------------------------

-- | Displays @i@ as it would show literally as a term.
--
-- @
-- 'P.Show_' ( t'N' 1) ~ \"-1\"
-- 'P.Show_'   t'Z'    ~  \"0\"
-- 'P.Show_' ( t'P' 1) ~  \"1\"
-- @
--
-- @t'N' 0@ and @t'P' 0@ fail to type-check.
instance P.PShow Integer where
  type ShowsPrec p i s = ShowsPrec_ p (Normalized i) s

type ShowsPrec_ :: Natural -> Integer -> L.Symbol -> L.Symbol
type family ShowsPrec_ p i s where
  ShowsPrec_ _  Z    s = P.Shows 0 s
  ShowsPrec_ p (N x) s =
    P.ShowParen (p P.>= AppPrec1) (P.ShowCharSym1 '-' P..@#@$$$ P.ShowsSym1 x) s
  ShowsPrec_ _ (P x) s = P.Shows x s

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
-- 'P.ShowLit' ( t'N' 1) ~ \"P 1\"
-- 'P.ShowLit'   t'Z'    ~ \"Z\"
-- 'P.ShowLit' ( t'P' 1) ~ \"P 1\"
-- @
--
-- @t'N' 0@ and @t'P' 0@ fail to type-check.
type ShowLit (i :: Integer) = ShowsLit i "" :: L.Symbol

data ShowLitSym0 :: Integer ~> L.Symbol
type ShowLitSym1 :: Integer -> L.Symbol

type instance Apply ShowLitSym0 i = ShowLitSym1 i
type ShowLitSym1 i = ShowLit i

-- | Displays @i@ as it would show literally as a type. See 'ShowLit.
-- Behaves like 'P.Shows'.
type ShowsLit (i :: Integer) (s :: L.Symbol) = ShowsPrecLit 0 i s :: L.Symbol

data ShowsLitSym0 :: Integer ~> L.Symbol ~> L.Symbol
data ShowsLitSym1 :: Integer -> L.Symbol ~> L.Symbol
type ShowsLitSym2 :: Integer -> L.Symbol -> L.Symbol

type instance Apply ShowsLitSym0 i = ShowsLitSym1 i
type instance Apply (ShowsLitSym1 i) s = ShowsLitSym2 i s
type ShowsLitSym2 i s = ShowsLit i s

-- | Displays @i@ as it would show literally as a type. See 'ShowLit.
-- Behaves like 'P.ShowsPrec'.
type ShowsPrecLit :: Natural -> Integer -> L.Symbol -> L.Symbol
type ShowsPrecLit p i s = ShowsPrecLit_ p (Normalized i) s

type ShowsPrecLit_ :: Natural -> Integer -> L.Symbol -> L.Symbol
type family ShowsPrecLit_ p i s where
  ShowsPrecLit_ _ Z s = P.ShowString "Z" s
  ShowsPrecLit_ p (N n) s =
    P.ShowParen (p P.>= 11) (P.ShowStringSym1 "N " P..@#@$$$ P.ShowsSym1 n) s
  ShowsPrecLit_ p (P n) s =
    P.ShowParen (p P.>= 11) (P.ShowStringSym1 "P " P..@#@$$$ P.ShowsSym1 n) s

data ShowsPrecLitSym0 :: Natural ~> Integer ~> L.Symbol ~> L.Symbol
data ShowsPrecLitSym1 :: Natural -> Integer ~> L.Symbol ~> L.Symbol
data ShowsPrecLitSym2 :: Natural -> Integer -> L.Symbol ~> L.Symbol
type ShowsPrecLitSym3 :: Natural -> Integer -> L.Symbol -> L.Symbol

type instance Apply ShowsPrecLitSym0 p = ShowsPrecLitSym1 p
type instance Apply (ShowsPrecLitSym1 p) i = ShowsPrecLitSym2 p i
type instance Apply (ShowsPrecLitSym2 p i) s = ShowsPrecLitSym3 p i s
type ShowsPrecLitSym3 p i s = ShowsPrecLit p i s

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
  :: Sing (p :: Natural) -> SInteger i -> Sing (s :: P.Symbol)
  -> Sing (ShowsPrecLit p i s)
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
class KnownInteger_ (i :: Integer) where
  integerSing_ :: SInteger i

-- | Type-level 'Integer's satisfying 'KnownInteger' can be converted to
-- 'SInteger's using 'integerSing'. Every 'Integer' other than @'N' 0@ and
-- @'P' 0@ are 'KnownInteger's.

-- Note: Ideally, the constraints mentioned here would be superclasses to
-- 'KnownInteger_'. However, 'withDict' doesn't allow having a superclass
-- there, so we treat 'KnownInteger_' as internal an export 'KnownInteger' only.
-- The 'KnownNat (Abs i)' constraint is listed here as a convenience, seeing
-- as it is necessary to implement 'KnownInteger_' anyway.
type KnownInteger (i :: Integer) =
  (KnownInteger_ i, Normalized i ~ i, L.KnownNat (Abs i))

data KnownIntegerSym0 :: Integer ~> Constraint
type KnownIntegerSym1 :: Integer -> Constraint

type instance Apply KnownIntegerSym0 i = KnownIntegerSym1 i
type KnownIntegerSym1 i = KnownInteger i

-- | Convert an implicit 'KnownInteger' to an explicit 'SInteger'.
integerSing :: KnownInteger i => SInteger i
integerSing = integerSing_ -- The difference is in the constraint.
{-# INLINE integerSing #-}

instance KnownInteger_ Z where
  integerSing_ = UnsafeSInteger 0
  {-# INLINE integerSing_ #-}

instance (KnownInteger (N n), n ~ Abs (N n)) => KnownInteger_ (N n) where
  integerSing_ = UnsafeSInteger $! P.negate (L.natVal (Proxy @n))
  {-# INLINE integerSing_ #-}

instance (KnownInteger (P n), n ~ Abs (P n)) => KnownInteger_ (P n) where
  integerSing_ = UnsafeSInteger $! L.natVal (Proxy @n)
  {-# INLINE integerSing_ #-}

-- | @'Normalized' i@ is an identity function that fails to type-check
-- if @i@ is not in normalized form. That is, /zero/ is represented with t'Z',
-- not with @t'P' 0@ or @ t'N' 0@.
type family Normalized (i :: Integer) :: Integer where
  Normalized (N 0) = L.TypeError ('L.Text "Use ‘Z’ instead of ‘N 0’.")
  Normalized (P 0) = L.TypeError ('L.Text "Use ‘Z’ instead of ‘P 0’.")
  Normalized i     = i

data NormalizedSym0 :: Integer ~> Integer
type NormalizedSym1 :: Integer -> Integer

type instance Apply NormalizedSym0 i = Normalized i
type NormalizedSym1 i = Normalized i

-- | 'SInteger' contains only 'Normalized' integers.
sNormalizedRefl :: SInteger i -> (i :~: Normalized i)
sNormalizedRefl !_ = unsafeCoerce Refl

-- | Term-level "Prelude".'P.Integer' representation of the type-level
-- 'Integer'.
integerVal :: forall i proxy. KnownInteger i => proxy i -> P.Integer
integerVal _ = case integerSing_ :: SInteger i of UnsafeSInteger x -> x
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
  showsPrec p (SomeInteger i) =
    showsPrec p (integerVal i)

-- | As for "Prelude".'P.Integer'.
instance Read SomeInteger where
  readPrec = fmap someIntegerVal Read.readPrec

--------------------------------------------------------------------------------

-- | Construct a type-level 'Integer' from a type-level 'Natural'.
type family FromNatural (x :: Natural) :: Integer where
  FromNatural 0 = Z
  FromNatural x = P x

data FromNaturalSym0 :: Natural ~> Integer
type FromNaturalSym1 :: Natural -> Integer

type instance Apply FromNaturalSym0 i = FromNatural i
type FromNaturalSym1 i = FromNatural i

-- | Singleton version of 'FromNatural'.
sFromNatural :: Sing (x :: Natural) -> SInteger (FromNatural x)
sFromNatural = UnsafeSInteger . toInteger . fromSing
{-# INLINE sFromNatural #-}

--------------------------------------------------------------------------------

-- | Demoted version of 'ZigZag'.
zigZag :: P.Integer -> Natural
zigZag 0 = 0
zigZag i = fromInteger (if i < 0 then abs i * 2 - 1 else i * 2)

-- | Singleton version of 'ZigZag'.
sZigZag :: SInteger i -> Sing (ZigZag i :: Natural)
sZigZag si = withSomeSing @Natural (zigZag (fromSing si)) unsafeCoerce

-- | Identity.
sZigZagRefl :: SInteger i -> (i :~: ZagZig (ZigZag i))
sZigZagRefl !_ = unsafeCoerce Refl

-- | Demoted version of 'ZagZig'.
zagZig :: Natural -> P.Integer
zagZig 0 = 0
zagZig n = if odd n then P.negate (P.toInteger (P.div (n + 1) 2))
                    else P.toInteger (P.div n 2)

-- | Singleton version of 'ZagZig'.
sZagZig :: Sing (n :: Natural) -> SInteger (ZagZig n)
sZagZig = UnsafeSInteger . zagZig . fromSing

-- | Identity.
sZagZigRefl :: Sing (n :: Natural) -> (n :~: ZigZag (ZagZig n))
sZagZigRefl !_ = unsafeCoerce Refl

-- | ZigZag encoding of an 'Integer'.
--
-- * /0/ is /0/
--
-- * /-x/ is /abs(x) * 2 - 1/
--
-- * /+x/ is /x * 2/
type ZigZag :: Integer -> Natural
type ZigZag (i :: Integer) = ZigZag_ (Normalized i) :: Natural
type family ZigZag_ (i :: Integer) :: Natural where
  ZigZag_  Z    = 0
  ZigZag_ (P x) = x P.* 2
  ZigZag_ (N x) = x P.* 2 P.- 1

data ZigZagSym0 :: Integer ~> Natural
type ZigZagSym1 :: Integer -> Natural

type instance Apply ZigZagSym0 i = ZigZagSym1 i
type ZigZagSym1 i = ZigZag i

-- | Inverse of 'ZigZag'.
type ZagZig :: Natural -> Integer
type ZagZig n = If (L.Mod n 2 P.== 1)
                   (P.Negate (FromNatural (L.Div (n L.+ 1) 2)))
                   (FromNatural (L.Div n 2))

data ZagZigSym0 :: Natural ~> Integer
type ZagZigSym1 :: Natural -> Integer

type instance Apply ZagZigSym0 n = ZagZigSym1 n
type ZagZigSym1 n = ZagZig n

--------------------------------------------------------------------------------


-- | Whether a type-level 'Integer' is odd. /Zero/ is not considered odd.
type Odd (i :: Integer) = L.Mod (Abs i) 2 P.== 1 :: Bool

data OddSym0 :: Integer ~> Bool
type OddSym1 :: Integer -> Bool

type instance Apply OddSym0 i = Odd i
type OddSym1 i = Odd i

-- | Singleton version of 'Odd'.
sOdd :: SInteger i -> Sing (Odd i :: Bool)
sOdd si | odd (fromSing si) = unsafeCoerce STrue
        | otherwise         = unsafeCoerce SFalse

-- | Whether a type-level 'Integer' is even. /Zero/ is considered even.
type Even (i :: Integer) = L.Mod (Abs i) 2 P.== 0 :: Bool

data EvenSym0 :: Integer ~> Bool
type EvenSym1 :: Integer -> Bool

type instance Apply EvenSym0 i = Even i
type EvenSym1 i = Even i

-- | Singleton version of 'Even'.
sEven :: SInteger i -> Sing (Even i :: Bool)
sEven si | even (fromSing si) = unsafeCoerce STrue
         | otherwise          = unsafeCoerce SFalse

--------------------------------------------------------------------------------

-- | Double negation is identity.
sNegateRefl :: SInteger i -> (i :~: P.Negate (P.Negate i))
sNegateRefl !_ = unsafeCoerce Refl

--------------------------------------------------------------------------------

-- | Absolute value of a type-level 'Integer', as a type-level 'Natural'.
type Abs (i :: Integer) = Fold 0 P.IdSym0 P.IdSym0 i :: Natural

data AbsSym0 :: Integer ~> Natural
type AbsSym1 :: Integer -> Natural

type instance Apply AbsSym0 x = AbsSym1 x
type AbsSym1 x = Abs x

-- | Singleton version of 'Abs'.
sAbs :: SInteger i -> Sing (Abs i :: Natural)
sAbs si = withSomeSing @Natural (fromInteger (abs (fromSing si))) unsafeCoerce

--------------------------------------------------------------------------------

infixr 8 %^, ^, ^@#@$$$

-- | Singleton version of '^'.
(%^) :: Sing (b :: Integer) -> Sing (p :: Natural) -> Sing (b ^ p :: Integer)
sb %^ sp = UnsafeSInteger (fromSing sb ^ fromSing sp)
{-# INLINE (%^) #-}

-- | Exponentiation of type-level 'Integer's.
type (b :: Integer) ^ (p :: Natural) = Pow (Normalized b) p :: Integer

data (^@#@$) :: Integer ~> Natural ~> Integer
data (^@#@$$) :: Integer -> Natural ~> Integer
type (^@#@$$$) :: Integer -> Natural -> Integer

type instance Apply ((^@#@$$) b) p = (^@#@$$$) b p
type instance Apply (^@#@$) b = (^@#@$$) b
type (^@#@$$$) b p = b ^ p

type family Pow (b :: Integer) (p :: Natural) :: Integer where
  Pow _ 0 = P 1
  Pow Z _ = Z
  Pow (P b) p = FromNatural (b L.^ p)
  Pow (N b) p = FromNatural (b L.^ p) P.* If (Even (P p)) (P 1) (N 1)

--------------------------------------------------------------------------------

infixl 7 `Div`, `Rem`, `DivRem`

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

data DivRemSym0 :: Round ~> Integer ~> Integer ~> (Integer, Integer)
data DivRemSym1 :: Round -> Integer ~> Integer ~> (Integer, Integer)
data DivRemSym2 :: Round -> Integer -> Integer ~> (Integer, Integer)
type DivRemSym3 :: Round -> Integer -> Integer -> (Integer, Integer)

type instance Apply DivRemSym0 r = DivRemSym1 r
type instance Apply (DivRemSym1 r) a = DivRemSym2 r a
type instance Apply (DivRemSym2 r a) b = DivRemSym3 r a b
type DivRemSym3 r a b = DivRem r a b

--------------------------------------------------------------------------------

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
  Rem_ r (Normalized a) (Normalized b) :: Integer
type Rem_ (r :: Round) (a :: Integer) (b :: Integer) =
  a P.- b P.* Div r a b :: Integer

data RemSym0 :: Round ~> Integer ~> Integer ~> Integer
data RemSym1 :: Round -> Integer ~> Integer ~> Integer
data RemSym2 :: Round -> Integer -> Integer ~> Integer
type RemSym3 :: Round -> Integer -> Integer -> Integer

type instance Apply RemSym0 r = RemSym1 r
type instance Apply (RemSym1 r) a = RemSym2 r a
type instance Apply (RemSym2 r a) b = RemSym3 r a b
type RemSym3 r a b = Rem r a b

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

--------------------------------------------------------------------------------

-- | Divide the type-level 'Integer' @a@ by @b@,
-- using the specified 'Round'ing @r@.
--
-- * Division by /zero/ doesn't type-check.
type Div (r :: Round) (a :: Integer) (b :: Integer) =
  Div_ r (Normalized a) (Normalized b) :: Integer
type family Div_ (r :: Round) (a :: Integer) (b :: Integer) :: Integer where
  Div_ r  Z     Z    = Div__ r Z 0
  Div_ r  Z    (P b) = Div__ r Z b
  Div_ r  Z    (N b) = Div__ r Z b
  Div_ r (P a) (P b) = Div__ r (P a) b
  Div_ r (N a) (N b) = Div__ r (P a) b
  Div_ r (P a) (N b) = Div__ r (N a) b
  Div_ r (N a) (P b) = Div__ r (N a) b

data DivSym0 :: Round ~> Integer ~> Integer ~> Integer
data DivSym1 :: Round -> Integer ~> Integer ~> Integer
data DivSym2 :: Round -> Integer -> Integer ~> Integer
type DivSym3 :: Round -> Integer -> Integer -> Integer

type instance Apply DivSym0 r = DivSym1 r
type instance Apply (DivSym1 r) a = DivSym2 r a
type instance Apply (DivSym2 r a) b = DivSym3 r a b
type DivSym3 r a b = Div r a b

type family Div__ (r :: Round) (a :: Integer) (b :: Natural) :: Integer where
  Div__ _ _   0 = L.TypeError ('L.Text "Division by zero")
  Div__ _ Z _ = Z

  Div__ 'RoundDown (P a) b = FromNatural (L.Div a b)
  Div__ 'RoundDown (N a) b = P.Negate (FromNatural (If (b L.* L.Div a b P.== a)
                                                       (L.Div a b)
                                                       (L.Div a b L.+ 1)))

  Div__ 'RoundUp a b = P.Negate (Div__ 'RoundDown (P.Negate a) b)

  Div__ 'RoundZero (P a) b = Div__ 'RoundDown (P a) b
  Div__ 'RoundZero (N a) b = P.Negate (Div__ 'RoundDown (P a) b)

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

--------------------------------------------------------------------------------

-- | Singleton version of 'Log2'.
sLog2 :: SInteger i -> Sing (Log2 i :: Natural)
sLog2 si = withSomeSing
             (fromIntegral (integerLog2 (fromSing si)) :: Natural)
             unsafeCoerce


-- | Log base 2 ('floor'ed) of type-level 'Integer's.
--
-- * Logarithm of /zero/ doesn't type-check.
--
-- * Logarithm of negative number doesn't type-check.
type Log2 (a :: Integer) =
  Fold (L.TypeError ('L.Text "Logarithm of zero."))
       (P.ConstSym1 (L.TypeError ('L.Text "Logarithm of negative number.")))
       (L.Log2Sym0)
       a
    :: Natural

data Log2Sym0 :: Integer ~> Natural
type Log2Sym1 :: Integer -> Natural

type instance Apply Log2Sym0 a = Log2Sym1 a
type Log2Sym1 a = Log2 a

--------------------------------------------------------------------------------

-- | Singleton version of 'GCD'.
sGCD :: SInteger a -> SInteger b -> Sing (GCD a b :: Natural)
sGCD sa sb = withSomeSing @Natural
               (fromInteger (gcd (fromSing sa) (fromSing sb)))
               unsafeCoerce

-- | Greatest Common Divisor of type-level 'Integer' numbers @a@ and @b@.
--
-- Returns a 'Natural', since the Greatest Common Divisor is always positive.
type GCD (a :: Integer) (b :: Integer) = NatGCD (Abs a) (Abs b) :: Natural

data GCDSym0 :: Integer ~> Integer ~> Natural
data GCDSym1 :: Integer -> Integer ~> Natural
type GCDSym2 :: Integer -> Integer -> Natural

type instance Apply GCDSym0 a = GCDSym1 a
type instance Apply (GCDSym1 a) b = GCDSym2 a b
type GCDSym2 a b = GCD a b

-- | Greatest Common Divisor of type-level 'Natural's @a@ and @b@.
type family NatGCD (a :: Natural) (b :: Natural) :: Natural where
  NatGCD a 0 = a
  NatGCD a b = NatGCD b (L.Mod a b)

--------------------------------------------------------------------------------

-- | Singleton version of 'LCM'.
sLCM :: SInteger a -> SInteger b -> Sing (LCM a b :: Natural)
sLCM sa sb = withSomeSing @Natural
               (fromInteger (lcm (fromSing sa) (fromSing sb)))
               unsafeCoerce

-- | Least Common Multiple of type-level 'Integer' numbers @a@ and @b@.
--
-- Returns a 'Natural', since the Least Common Multiple is always positive.
type LCM (a :: Integer) (b :: Integer) = NatLCM (Abs a) (Abs b) :: Natural

data LCMSym0 :: Integer ~> Integer ~> Natural
data LCMSym1 :: Integer -> Integer ~> Natural
type LCMSym2 :: Integer -> Integer -> Natural

type instance Apply LCMSym0 a = LCMSym1 a
type instance Apply (LCMSym1 a) b = LCMSym2 a b
type LCMSym2 a b = LCM a b

-- | Least Common Multiple of type-level 'Natural's @a@ and @b@.
type NatLCM (a :: Natural) (b :: Natural) =
  L.Div a (NatGCD a b) L.* b :: Natural

--------------------------------------------------------------------------------

type Add_ :: Ordering -> Ordering ->  Natural -> Natural -> Integer
type family Add_ lc rc la ra where
  Add_ 'EQ 'EQ _ _ = Z
  Add_ 'EQ 'LT _ r = N r
  Add_ 'EQ 'GT _ r = P r
  Add_ 'LT 'EQ l _ = N l
  Add_ 'GT 'EQ l _ = P l
  Add_ 'LT 'LT l r = N (l L.+ r)
  Add_ 'LT 'GT l r = OrdCond (Compare l r) (P (r P.- l)) Z (N (l P.- r))
  Add_ 'GT 'GT l r = P (l L.+ r)
  Add_ 'GT 'LT l r = OrdCond (Compare l r) (N (r P.- l)) Z (P (l P.- r))

--------------------------------------------------------------------------------

type Mul_ :: Ordering -> Ordering ->  Natural -> Natural -> Integer
type family Mul_ lc rc la ra where
  Mul_ _   'EQ _ _ = Z
  Mul_ 'EQ _   _ _ = Z
  Mul_ x   x   l r = P (l L.* r)
  Mul_ _   _   l r = N (l L.* r)

--------------------------------------------------------------------------------

instance P.PNum Integer where
  type l + r = Add_ (Compare l Z) (Compare r Z) (Abs l) (Abs r)
  type l - r = l P.+ P.Negate r
  type l * r = Mul_ (Compare l Z) (Compare r Z) (Abs l) (Abs r)
  type Negate i = Fold Z PSym0 NSym0 i
  type Abs i = FromNatural (Abs i)
  type Signum i = Fold Z (P.ConstSym1 (N 1)) (P.ConstSym1 (P 1)) i
  type FromInteger i = FromNatural i

instance P.SNum Integer where
  l %+ r = UnsafeSInteger (fromSing l + fromSing r)
  l %- r = UnsafeSInteger (fromSing l - fromSing r)
  l %* r = UnsafeSInteger (fromSing l * fromSing r)
  sNegate = UnsafeSInteger . negate . fromSing
  sAbs = sFromNatural . sAbs
  sSignum = UnsafeSInteger . signum . fromSing
  sFromInteger = sFromNatural

--------------------------------------------------------------------------------

type instance Compare (l :: Integer) (r :: Integer) =
  CmpInteger_ (Fold 'EQ (P.ConstSym1 'LT) (P.ConstSym1 'GT) l)
              (Fold 'EQ (P.ConstSym1 'LT) (P.ConstSym1 'GT) r)
              (Abs l)
              (Abs r)

type CmpInteger_ :: Ordering -> Ordering -> Natural -> Natural -> Ordering
type family CmpInteger_ lc rc la ra where
  CmpInteger_ 'LT 'LT l r = Compare r l
  CmpInteger_ 'LT 'EQ _ _ = 'LT
  CmpInteger_ 'LT 'GT _ _ = 'LT
  CmpInteger_ 'EQ 'LT _ _ = 'GT
  CmpInteger_ 'EQ 'EQ _ _ = 'EQ
  CmpInteger_ 'EQ 'GT _ _ = 'LT
  CmpInteger_ 'GT 'LT _ _ = 'GT
  CmpInteger_ 'GT 'EQ _ _ = 'GT
  CmpInteger_ 'GT 'GT l r = Compare l r

instance OrdS.POrd Integer where
  type Compare l r = Data.Type.Ord.Compare l r

instance OrdS.SOrd Integer where
  sCompare sa sb =
    case compare (fromSing sa) (fromSing sb) of
      LT -> unsafeCoerce OrdS.SLT
      EQ -> unsafeCoerce OrdS.SEQ
      GT -> unsafeCoerce OrdS.SGT

--------------------------------------------------------------------------------

instance EqS.PEq Integer where
  type a == b = OrdCond (Compare a b) 'False 'True 'False

instance EqS.SEq Integer where
  sa %== sb = case fromSing sa P.== fromSing sb of
    True  -> unsafeCoerce STrue
    False -> unsafeCoerce SFalse

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
  EQ | Refl <- (unsafeCoerce Refl :: a :~: b)
     , Refl <- (unsafeCoerce Refl :: Compare a b :~: 'EQ) -> EQI
  LT | Refl <- (unsafeCoerce Refl :: Compare a b :~: 'LT) -> LTI
  GT | Refl <- (unsafeCoerce Refl :: Compare a b :~: 'GT) -> GTI

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
-- f si\@'SInteger' = /... both (si :: 'SInteger' i) and ('KnownInteger' i) in scope .../
-- @
pattern SInteger :: forall i. () => KnownInteger i => SInteger i
pattern SInteger <- (patternSInteger -> PatternSInteger)
  where SInteger = integerSing
{-# COMPLETE SInteger #-}

-- | Only used for defining the 'SInteger' pattern synonym.
data PatternSInteger (i :: Integer) where
  PatternSInteger :: KnownInteger i => PatternSInteger i

-- | Only used for defining the 'SInteger' pattern synonym.
patternSInteger :: SInteger i -> PatternSInteger i
patternSInteger si = withKnownInteger si PatternSInteger

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
fromSInteger = coerce
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
  | sa :: Sing a <- sAbs si
  , Refl <- sNormalizedRefl si
  = L.withKnownNat sa (withKnownInteger_ si x)

-- | Convert a "Prelude".'P.Integer' number into an @'SInteger' n@ value,
-- where @n@ is a fresh type-level 'Integer'.
withSomeSInteger
  :: forall rep (x :: TYPE rep)
  .  P.Integer
  -> (forall (i :: Integer). Sing i -> x)
  -> x
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
  l %~ r = case fromSing l P.== fromSing r of
    True  -> Proved (unsafeCoerce Refl)
    False -> Disproved (\Refl -> error "KindInteger.Integer: SDecide")

--------------------------------------------------------------------------------

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

data Rat = MkRat Integer Natural
  -- ^ Invariant: 'Natural' is never 0. This is enforced by the 'R' type-family.

type R :: Integer -> Natural -> Rat
type family R n d where
  R _ 0 = L.TypeError ('L.Text "Denominator is 0.")
  R n d = Fold ('MkRat Z 1) (RAuxSym2 NSym0 d) (RAuxSym2 PSym0 d) n

data RAuxSym2 :: (Natural ~> Integer) -> Natural -> Natural ~> Rat
type instance Apply (RAuxSym2 fn d) n = 'MkRat (fn @@ n) d

type RatReduce :: Rat -> Rat
type family RatReduce r where
  RatReduce ('MkRat n d) = 'MkRat
    (P.Signum n P.* FromNatural (L.Div (Abs n) (NatGCD (Abs n) d)))
    (L.Div d (NatGCD (Abs n) d))

type RatAbs :: Rat -> Rat
type family RatAbs r where
  RatAbs ('MkRat n d) = R (P.Abs n) d

type RatAdd :: Rat -> Rat -> Rat
type family RatAdd l r where
  RatAdd ('MkRat ln ld) ('MkRat rn rd) =
    RatReduce (R (ln P.* P rd P.+ rn P.* P ld) (ld P.* rd))

type RatNegate :: Rat -> Rat
type family RatNegate r where
  RatNegate ('MkRat n d) = R (P.Negate n) d

type RatMinus :: Rat -> Rat -> Rat
type RatMinus l r = RatAdd l (RatNegate r)

type RatCmp :: Rat -> Rat -> Ordering
type RatCmp l r = RatCmp_ (RatReduce l) (RatReduce r)
type family RatCmp_ (a :: Rat) (b :: Rat) :: Ordering where
  RatCmp_ a a = 'EQ
  RatCmp_ ('MkRat an ad) ('MkRat bn bd) = Compare (an P.* P bd) (bn P.* P ad)

-- | ''True' if the distance between @a@ and @b@ is less than /0.5/.
type HalfLT :: Rat -> Integer -> Bool
type HalfLT a b =
  'LT P.== RatCmp (RatAbs (RatMinus a ('MkRat b 1))) ('MkRat (P 1) 2)

--------------------------------------------------------------------------------

errDiv0 :: P.Integer -> P.Integer
errDiv0 0 = Ex.throw Ex.DivideByZero
errDiv0 i = i

pSkipSpaces1 :: ReadP.ReadP ()
pSkipSpaces1 = void $ ReadP.munch1 Char.isSpace

