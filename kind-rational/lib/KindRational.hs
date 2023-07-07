{-# LANGUAGE StrictData #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE UndecidableSuperClasses #-}

-- | This module provides a type-level representation for term-level
-- 'P.Rational's. This type-level representation is also named 'P.Rational',
-- So import this module qualified to avoid name conflicts.
--
-- @
-- import "KindRational" qualified as KR
-- @
--
-- The implementation details are similar to the ones for type-level 'Natural's
-- as of @base-4.18@ and @singletons-base-3.1.1@, and they will continue to
-- evolve together with @base@ and @singletons-base@, trying to more or less
-- follow their API.
module KindRational {--}
  ( -- * Rational
    Rational
  , type (:%)
  , FromNatural, fromNatural, sFromNatural
  , FromInteger, fromInteger, sFromInteger
  , Num, sNum
  , Den, sDen
  , ToRational(..)
  , sMkRational
  , (%%)
  , (%)

    -- * SRational
  , KnownRational
  , Reduced
  , rationalSing
  , rationalVal
  , withKnownRational
  , SomeRational(..)
  , someRationalVal
  , SRational
  , pattern SRational
  , fromSRational
  , withSomeSRational

    -- * Proofs
  , sNegateRefl

    -- * Show
    --
    -- | Besides the following /\*Lit/ tools, 'P.PShow' and 'P.SShow' can
    -- be used to display as "Prelude".'P.Rational' does.
  , ShowLit, showLit, sShowLit
  , ShowsLit, showsLit, sShowsLit
  , ShowsPrecLit, showsPrecLit, sShowsPrecLit
  , readPrecLit

    -- * Arithmethic
  , Signum, sSignum, sSignumRefl
  , Recip, sRecip, sRecip'
  , Div, sDiv, div
  , Rem, rem, sRem
  , DivRem, divRem, sDivRem
  , I.Round(..), I.SRound(..)

    -- * Decimals
  , IsTerminating
  , isTerminating
  , termination
  , Terminating
  , NonTerminating
  , pattern SRationalTerminating
  , pattern SRationalNonTerminating

    -- * Comparisons
    --
    -- | Additional comparison tools are available at 'SDdecide',
    -- 'TestEquality', 'TestCoercion', 'P.PEq', 'P.SEq', 'P.POrd', 'P.SOrd'
    -- and 'Compare'.
  , cmpRational
  , sameRational


    -- * Defunctionalization
  , type (%@#@$), type (%@#@$$), type (%@#@$$$)
  , type (:%@#@$), type (:%@#@$$), type (:%@#@$$$)
  , FromNaturalSym0, FromNaturalSym1
  , FromIntegerSym0, FromIntegerSym1
  , NumSym0, NumSym1
  , DenSym0, DenSym1
  , ToRationalSym0, ToRationalSym1, ToRationalSym2
  , ReducedSym0, ReducedSym1
  , ShowLitSym0, ShowLitSym1
  , ShowsLitSym0, ShowsLitSym1, ShowsLitSym2
  , ShowsPrecLitSym0, ShowsPrecLitSym1, ShowsPrecLitSym2, ShowsPrecLitSym3
  , IsTerminatingSym0, IsTerminatingSym1
  , TerminatingSym0, TerminatingSym1
  , NonTerminatingSym0, NonTerminatingSym1
  , SignumSym0, SignumSym1
  , RecipSym0, RecipSym1
  , DivSym0, DivSym1, DivSym2
  , RemSym0, RemSym1, RemSym2
  , DivRemSym0, DivRemSym1, DivRemSym2
  , I.RoundUpSym0
  , I.RoundDownSym0
  , I.RoundZeroSym0
  , I.RoundAwaySym0
  , I.RoundHalfUpSym0
  , I.RoundHalfDownSym0
  , I.RoundHalfZeroSym0
  , I.RoundHalfAwaySym0
  , I.RoundHalfEvenSym0
  , I.RoundHalfOddSym0
  ) --}
  where

import Control.Monad
import Data.Bits
import Data.Bool.Singletons (SBool(..))
import Data.Eq.Singletons qualified as EqS
import Data.Maybe
import Data.Ord.Singletons qualified as OrdS
import Data.Proxy
import Data.Singletons
import Data.Singletons.Decide
import Data.String
import Data.Type.Coercion
import Data.Type.Equality (TestEquality(..))
import Data.Type.Ord
import GHC.Base (WithDict(..))
import GHC.Exts (TYPE, Constraint)
import GHC.Read qualified as Read
import GHC.Real qualified as P (Ratio(..), (%))
import GHC.Show (appPrec, appPrec1)
import GHC.Stack (HasCallStack)
import GHC.TypeLits qualified as L hiding (someNatVal)
import GHC.TypeNats qualified as L (someNatVal)
import KindInteger (SInteger, Integer)
import KindInteger qualified as I
import Numeric.Natural (Natural)
import Prelude hiding (Rational, Integer, Num, div, rem, fromInteger)
import Prelude qualified as P
import Prelude.Singletons qualified as P
import Text.ParserCombinators.ReadP as ReadP
import Text.ParserCombinators.ReadPrec as ReadPrec
import Text.Read.Lex qualified as Read
import Text.Show.Singletons (AppPrec1)
import Unsafe.Coerce(unsafeCoerce)

--------------------------------------------------------------------------------

-- | Type-level version of "Prelude".'P.Rational', used only as a kind.
--
-- Use t'%' to construct one, use t':%' to pattern-match on it.
data Rational = Integer :% Natural
  -- ^ Note that @n ':%' d@ doesn't guarantee that @n@ be 'I.Normalized', nor
  -- that @d@ be non-zero, nor that the rational number is in 'Reduced' form.
  -- To make sure that's the case, use t'%' to construct type-level rationals,
  -- and use t':%' only for pattern-matching purposes. Alternatively, consider
  -- using the 'KnownRational' constraint or the 'Reduced' type-family. Both
  -- of them will reject any 'Rational' that fails to satisfy any of these
  -- constraints.

  -- Note: We export type-synonym ':%' rather than type-constructor ':%' so
  -- as to avoid having to use the leading tick, and to prevent term-level
  -- uses of the constructor.


-- | Shows just as a term-level "Prelude".'P.Rational'.
instance P.SShow Rational where
  sShowsPrec _ si ss = withSomeSing
                         (fromString (show (fromSing si)) <> fromSing ss)
                         unsafeCoerce

-- | Shows just as a term-level "Prelude".'P.Rational'.
--
-- 'ShowsPrec' type-checks only if the type-level 'Rational' is 'Reduced'.
instance P.PShow Rational where
  type ShowsPrec p r s = P.ShowParen (p P.>= AppPrec1)
    (P.ShowsPrecSym2 AppPrec1 (Num r) P..@#@$$$
     P.ShowStringSym1 " % " P..@#@$$$
     P.ShowsSym1 (Den r)) s

-- | Shows as a type-level "KindRational".'Rational' apears literally at the
-- type-level. Type-checks only if the type-level 'Rational' is 'Reduced'.
type ShowLit (r :: Rational) = ShowsLit r "" :: L.Symbol

data ShowLitSym0 :: Rational ~> L.Symbol
type ShowLitSym1 :: Rational -> L.Symbol

type instance Apply ShowLitSym0 i = ShowLitSym1 i
type ShowLitSym1 i = ShowLit i

-- | Shows as a type-level "KindRational".'Rational' apears literally at the
-- type-level. Type-checks only if the type-level 'Rational' is 'Reduced'.
type ShowsLit (r :: Rational) (s :: L.Symbol) = ShowsPrecLit 0 r s :: L.Symbol

data ShowsLitSym0 :: Rational ~> L.Symbol ~> L.Symbol
data ShowsLitSym1 :: Rational -> L.Symbol ~> L.Symbol
type ShowsLitSym2 :: Rational -> L.Symbol -> L.Symbol

type instance Apply ShowsLitSym0 i = ShowsLitSym1 i
type instance Apply (ShowsLitSym1 i) s = ShowsLitSym2 i s
type ShowsLitSym2 i s = ShowsLit i s

-- | Shows as a type-level "KindRational".'Rational' apears literally at the
-- type-level. Type-checks only if the type-level 'Rational' is 'Reduced'.
type ShowsPrecLit (p :: Natural) (r :: Rational) (s :: L.Symbol) =
  P.ShowParen (p P.>= AppPrec1)
    (I.ShowsLitSym1 (Num r) P..@#@$$$
     P.ShowStringSym1 " :% " P..@#@$$$
     P.ShowsSym1 (Den r)) s :: L.Symbol

data ShowsPrecLitSym0 :: Natural ~> Rational ~> L.Symbol ~> L.Symbol
data ShowsPrecLitSym1 :: Natural -> Rational ~> L.Symbol ~> L.Symbol
data ShowsPrecLitSym2 :: Natural -> Rational -> L.Symbol ~> L.Symbol
type ShowsPrecLitSym3 :: Natural -> Rational -> L.Symbol -> L.Symbol

type instance Apply ShowsPrecLitSym0 p = ShowsPrecLitSym1 p
type instance Apply (ShowsPrecLitSym1 p) i = ShowsPrecLitSym2 p i
type instance Apply (ShowsPrecLitSym2 p i) s = ShowsPrecLitSym3 p i s
type ShowsPrecLitSym3 p i s = ShowsPrecLit p i s

-- | Singleton version of 'ShowLit'.
sShowLit :: SRational r -> Sing (ShowLit r)
sShowLit sr = sShowsLit sr (sing @"")

-- | Demoted version of 'ShowLit'.
showLit :: P.Rational -> String
showLit r = showsLit r ""

-- | Singleton version of 'ShowsLit'.
sShowsLit :: SRational r -> Sing (s :: P.Symbol) -> Sing (ShowsLit r s)
sShowsLit = sShowsPrecLit (sing @0)

-- | Demoted version of 'ShowsLit'.
showsLit :: P.Rational -> ShowS
showsLit = showsPrecLit 0

-- | Singleton version of 'ShowsPrecLit'.
sShowsPrecLit
  :: Sing (p :: Natural)
  -> SRational r
  -> Sing (s :: P.Symbol)
  -> Sing (ShowsPrecLit p r s)
sShowsPrecLit sp si ss =
  let p = fromMaybe (error "sShowsPrecLit: invalid precedence")
                    (toIntegralSized (fromSing sp))
      t = fromString (showsPrecLit p (fromSing si) "")
  in withSomeSing (t <> fromSing ss) unsafeCoerce

-- | Demoted version of 'ShowsPrecLit'.
showsPrecLit :: Int -> P.Rational -> ShowS
showsPrecLit p _ | p < 0 = error "showsPrecLit: negative precedence"
showsPrecLit p (unsafeReduced -> n P.:% d) =
  showParen (p >= appPrec1) $
    I.showsPrecLit appPrec n .
    showString " :% " .
    shows d

-- | Inverse of 'showsPrecLit'.
readPrecLit :: ReadPrec.ReadPrec P.Rational
readPrecLit = Read.parens $ do
    n :: P.Integer <- Read.parens $ ReadPrec.step I.readPrecLit
    Read.expectP (Read.Symbol ":%")
    d :: Natural <- Read.parens $ ReadPrec.step $ ReadPrec.lift pNatural
    either fail pure $ rationalLit n (toInteger d)
  where
    pNatural :: ReadP.ReadP Natural
    pNatural = read <$> ReadP.munch1 (\c -> c >= '0' && c <= '9')

-- | Creates a "Prelude".'P.Rational' using the given literal numerator
-- and denominator, if in reduced form with a non-zero denominator.
rationalLit :: P.Integer -> P.Integer -> Either String P.Rational
rationalLit = \n d -> do
  when (d == 0) $ Left "Denominator is zero."
  when (d < 0) $ Left "Rational is not reduced."
  let r@(n' P.:% d') = n P.% d
  when (n /= n' || d /= d') $ Left "Rational is not reduced."
  Right r

-- | Reduces the given rational. 'error's if it can't be reduced.
unsafeReduce :: HasCallStack => P.Rational -> P.Rational
unsafeReduce = \case
  n P.:% d | d /= 0 -> n P.% d
           | otherwise -> error "Denominator is zero."

-- | 'error's if the given rational is not reduced.  Otherwise, returns it.
unsafeReduced :: HasCallStack => P.Rational -> P.Rational
unsafeReduced = \r0 -> case unsafeReduce r0 of
  r1 | r0 == r1 -> r0
     | otherwise -> error $ concat
        [ "Expected reduced rational ", showsPrec appPrec1 r1 ""
        , ", got " , showsPrec appPrec1 r0 "." ]

--------------------------------------------------------------------------------

-- | Singleton version of 'Num'.
sNum :: SRational r -> SInteger (Num r)
sNum sr | n P.:% _ <- fromSing sr =
  withSomeSing n unsafeCoerce

-- | Literal /'Num'erator/ of a type-level 'Rational'.
-- It fails to type-check if the 'Rational' is not 'Reduced'.
type Num (r :: Rational) = Num_ (Reduced r) :: Integer
type family Num_ (r :: Rational) :: Integer where Num_ (n :% _) = n

data NumSym0 :: Rational ~> Integer
type NumSym1 :: Rational -> Integer

type instance Apply NumSym0 i = Num i
type NumSym1 i = Num i

-- | Singleton version of 'Den'.
sDen :: SRational r -> Sing (Den r :: Natural)
sDen sr | _ P.:% d <- fromSing sr =
  withSomeSing @Natural (P.fromInteger d) unsafeCoerce

-- | Literal /'Den'ominator/ of a type-level 'Rational'.
-- It fails to type-check if the 'Rational' is not 'Reduced'.
type Den (r :: Rational) = Den_ (Reduced r) :: Natural
type family Den_ (r :: Rational) :: Natural where Den_ (_ :% d) = d

data DenSym0 :: Rational ~> Natural
type DenSym1 :: Rational -> Natural

type instance Apply DenSym0 i = Den i
type DenSym1 i = Den i

-- | Pattern-match on a type-level 'Rational'.
--
-- Note that t':%' doesn't check that the 'Rational' be 'Reduced' nor
-- have a non-zero denominator.
--
-- When constructing 'Rational's, use t'%' instead, which not only accepts
-- more polymorphic inputs, but also returns a 'Reduced' result with
-- denominator known to be non-zero.
type (n :: I.Integer) :% (d :: Natural) = n ':% d :: Rational

data (:%@#@$) :: Integer ~> Natural ~> Rational
data (:%@#@$$) :: Integer -> Natural ~> Rational
type (:%@#@$$$) :: Integer -> Natural -> Rational

type instance Apply ((:%@#@$$) b) p = (:%@#@$$$) b p
type instance Apply (:%@#@$) b = (:%@#@$$) b
type (:%@#@$$$) b p = b :% p

--------------------------------------------------------------------------------

-- | A 'Reduce'd rational number is one in which the numerator and denominator
-- have no common denominators. 'Reduced' fails to type-check if the given
-- type-level 'Rational' is not reduced, otherwise it returns the given
-- 'Rational', unmodified.  It also fails to type-check if the 'I.Integer'
-- numerator isn't 'I.Normalized', or if the denominator is zero.
--
-- Only reduced 'Rational's can be reliably constrained for equality
-- using '~'. Only reduced 'Rational's are 'KnownRational's.
--
-- The type-level functions in the "KindRational" module
-- always produce reduced 'Rational's.
type Reduced (r :: Rational) = Reduced_ r (Reduce r) :: Rational
type family Reduced_ (r :: Rational) (x :: Rational) :: Rational where
  Reduced_ r r = r
  Reduced_ (na :% da) (nb :% db) = L.TypeError
    ('L.Text "Expected reduced rational (" 'L.:<>:
     'L.Text (I.ShowsLit nb " :% ") 'L.:<>:
     'L.Text (P.Shows db "), got (") 'L.:<>:
     'L.Text (I.ShowsLit na " :% ") 'L.:<>:
     'L.Text (P.Shows da ")."))

data ReducedSym0 :: Rational ~> Rational
type ReducedSym1 :: Rational -> Rational

type instance Apply ReducedSym0 i = Reduced i
type ReducedSym1 i = Reduced i

-- | 'SRational's always contain a 'Reduced' 'Rational'.
sReducedRefl :: SRational r -> (r :~: Reduced r)
sReducedRefl !_ = unsafeCoerce Refl

-- | Reduce a type-level 'Rational' so that the numerator and denominator
-- have no common factors. It fails to type-check if the 'I.Integer'
-- numerator isn't 'I.Normalized'.
--
-- Only reduced 'Rational's can be reliably constrained for equality
-- using '~'. Only reduced 'Rational's are 'KnownRational's.
type family Reduce (r :: Rational) :: Rational where
  Reduce (n :% 0) = L.TypeError
    ('L.Text "Denominator is zero in (" 'L.:<>: 'L.Text (I.ShowsLit n " :% 0)"))
  Reduce (n :% d) =
    I.Fold (FromNatural 0) (ReduceNegSym1 d) (ReducePosSym1 d) n

data ReduceNegSym1 :: Natural -> Natural ~> Rational
type instance Apply (ReduceNegSym1 d) n = ReduceNeg d n
type ReduceNeg (d :: Natural) (n :: Natural)
  = ReduceNeg_ (ReducePos d n) :: Rational
type ReduceNeg_ (rpos :: Rational) = P.Negate (Num_ rpos) :% Den_ rpos

data ReducePosSym1 :: Natural -> Natural ~> Rational
type instance Apply (ReducePosSym1 d) n = ReducePos d n
type family ReducePos (d :: Natural) (n :: Natural) :: Rational where
  ReducePos d n =
    I.FromNatural (L.Div n (I.GCD (I.FromNatural n) (I.FromNatural d)))
               :% (L.Div d (I.GCD (I.FromNatural n) (I.FromNatural d)))

--------------------------------------------------------------------------------

-- | Construct a type-level 'Rational' from a type-level 'Natural'.
type FromNatural (n :: Natural) = I.FromNatural n :% 1 :: Rational

data FromNaturalSym0 :: Natural ~> Rational
type FromNaturalSym1 :: Natural -> Rational

type instance Apply FromNaturalSym0 i = FromNatural i
type FromNaturalSym1 i = FromNatural i

-- | Singleton version of 'FromNatural'.
sFromNatural :: Sing (n :: Natural) -> SRational (FromNatural n)
sFromNatural = UnsafeSRational . fromIntegral . fromSing
{-# INLINE sFromNatural #-}

-- | Demoted version of 'FromNatural'.
fromNatural :: Natural -> P.Rational
fromNatural = P.fromIntegral
{-# INLINE fromNatural #-}

--------------------------------------------------------------------------------

-- | Construct a type-level 'Rational' from a type-level 'Integer'.
-- It fails to type-check if the 'I.Integer' isn't 'I.Normalized'.
type FromInteger (i :: Integer) = I.Normalized i :% 1 :: Rational

data FromIntegerSym0 :: Integer ~> Rational
type FromIntegerSym1 :: Integer -> Rational

type instance Apply FromIntegerSym0 i = FromInteger i
type FromIntegerSym1 i = FromInteger i

-- | Singleton version of 'FromInteger'.
sFromInteger :: SInteger n -> SRational (FromInteger n)
sFromInteger = UnsafeSRational . fromInteger . fromSing
{-# INLINE sFromInteger #-}

-- | Demoted version of 'FromInteger'.
fromInteger :: P.Integer -> P.Rational
fromInteger = P.fromInteger
{-# INLINE fromInteger #-}

--------------------------------------------------------------------------------

infixl 7 %, %%, :%, :%@#@$$$, %@#@$$$

-- | @'ToRational' kn kd@ enables constructing type-level 'Rational's
-- from a numerator of kind @kn@ and a denominator of kind @kd@.
class (SingKind kn, SingKind kd) => ToRational kn kd where
  -- | @n t'%' d@ constructs and reduces a type-level 'Rational'
  -- with numerator @n@ and denominator @d@.
  --
  -- This type-family accepts any combination of 'Natural', 'Integer' and
  -- 'Rational' as input.
  --
  -- @
  -- ( t'%') :: 'Natural'  -> 'Natural'  -> 'Rational'
  -- ( t'%') :: 'Natural'  -> 'Integer'  -> 'Rational'
  -- ( t'%') :: 'Natural'  -> 'Rational' -> 'Rational'
  --
  -- ( t'%') :: 'Integer'  -> 'Natural'  -> 'Rational'
  -- ( t'%') :: 'Integer'  -> 'Integer'  -> 'Rational'
  -- ( t'%') :: 'Integer'  -> 'Rational' -> 'Rational'
  --
  -- ( t'%') :: 'Rational' -> 'Natural'  -> 'Rational'
  -- ( t'%') :: 'Rational' -> 'Integer'  -> 'Rational'
  -- ( t'%') :: 'Rational' -> 'Rational' -> 'Rational'
  -- @
  --
  -- It's not possible to pattern-match on @n t'%' d@.  Instead, you must
  -- pattern match on @x t':%' y@, where @x t':%' y ~ n t'%' d@.
  type (n :: kn) % (d :: kd) :: Rational
  -- | Demoted version of t'%'. Returns 'Nothing' where t'%' would fail
  -- to type-check (that is, denominator is 0).
  -- See '%' for an unsafe version of this.
  mkRational :: Demote kn -> Demote kd -> Maybe P.Rational

-- | Like 'mkRational', but fails with 'error' where 'mkRational' would
-- fail with 'Nothing'.
(%) :: forall kn kd
    .  (ToRational kn kd, HasCallStack)
    => Demote kn
    -> Demote kd
    -> P.Rational
n % d = fromMaybe (error "Denominator is zero.") (mkRational n d)
{-# INLINE (%) #-}

-- | Like 'sMkRational', but never fails, thanks to a
-- 'KnownRational' constraint.
(%%) :: forall kn kd n d
     .  (ToRational kn kd, KnownRational (n % d))
     => Sing (n :: kn)
     -> Sing (d :: kd)
     -> SRational (n % d)
(%%) sn sd = fromJust (sMkRational sn sd)
{-# INLINE (%%) #-}

-- | Singleton version of 'mkRational'.
sMkRational
  :: forall kn kd n d
  .  (ToRational kn kd)
  => Sing (n :: kn)
  -> Sing (d :: kd)
  -> Maybe (SRational (n % d))
sMkRational sn sd =
  UnsafeSRational <$> mkRational (fromSing sn) (fromSing sd)
{-# INLINE sMkRational #-}

data ToRationalSym0 :: kkn ~> kkd ~> Constraint
data ToRationalSym1 :: kkn -> kkd ~> Constraint
-- type ToRationalSym2 :: kkn -> kkd -> Constraint  --  (inferred)

type instance Apply ToRationalSym0 kn = ToRationalSym1 kn
type instance Apply (ToRationalSym1 kn) kd = ToRationalSym2 kn kd
type ToRationalSym2 kn kd = ToRational kn kd

data (%@#@$) :: kn ~> kd ~> Rational
data (%@#@$$) :: kn -> kd ~> Rational
type (%@#@$$$) :: kn -> kd -> Rational

type instance Apply ((%@#@$$) b) p = (%@#@$$$) b p
type instance Apply (%@#@$) b = (%@#@$$) b
type (%@#@$$$) b p = b % p

instance ToRational Natural Natural where
  type n % d = I.FromNatural n % d
  mkRational _ 0 = Nothing
  mkRational n d = Just $! toInteger n P.% toInteger d

instance ToRational Natural Integer where
  type n % d = (I.FromNatural n P.* P.Signum d) % I.Abs d
  mkRational _ 0 = Nothing
  mkRational n d = Just $! toInteger n P.% d

instance ToRational Natural Rational where
  type n % d = FromNatural n P.* Recip d
  mkRational _ 0 = Nothing
  mkRational n d = Just $! fromIntegral n P./ d

instance ToRational Integer Natural where
  type n % d = Reduce (n :% d)
  mkRational _ 0 = Nothing
  mkRational n d = Just $! n P.% toInteger d

instance ToRational Integer Integer where
  type n % d = (n P.* P.Signum d) % I.Abs d
  mkRational _ 0 = Nothing
  mkRational n d = Just $! (n P.* P.signum d) P.% P.abs d

instance ToRational Integer Rational where
  type n % d = FromInteger n P.* Recip d
  mkRational _ 0 = Nothing
  mkRational n d = Just $! fromInteger n P.* recip d

instance ToRational Rational Natural where
  type n % d = n P.* Recip (FromNatural d)
  mkRational _ 0 = Nothing
  mkRational n d = Just $! n P.* P.recip (fromNatural d)

instance ToRational Rational Integer where
  type n % d = n P.* Recip (FromInteger d)
  mkRational _ 0 = Nothing
  mkRational n d = Just $! n P.* P.recip (fromInteger d)

instance ToRational Rational Rational where
  type n % d = n P.* Recip d
  mkRational _ 0 = Nothing
  mkRational n d = Just $! n P.* P.recip d

--------------------------------------------------------------------------------

instance P.PNum Rational where
  type l + r = Add l r
  type l - r = Add l (P.Negate r)
  type l * r = Mul l r
  type Negate r = Reduce (P.Negate (Num r) :% Den r)
  type Abs r = Reduce (P.Abs (Num r) :% Den r)
  type Signum r = P.Signum (Num r) :% 1
  type FromInteger n = FromNatural n

instance P.SNum Rational where
  l %+ r = UnsafeSRational (fromSing l + fromSing r)
  l %- r = UnsafeSRational (fromSing l - fromSing r)
  l %* r = UnsafeSRational (fromSing l * fromSing r)
  sNegate = UnsafeSRational . negate . fromSing
  sAbs = UnsafeSRational . abs . fromSing
  sSignum = UnsafeSRational . signum . fromSing
  sFromInteger = sFromNatural

sNegateRefl :: Sing (r :: Rational) -> (r :~: P.Negate (P.Negate r))
sNegateRefl !_ = unsafeCoerce Refl

-- | Sign of type-level 'Rational's, as a type-level 'Integer'.
--
-- * 'I.Z' if zero.
--
-- * @'I.P' 1@ if positive.
--
-- * @'I.N' 1@ if negative.
type Signum (r :: Rational) = P.Signum (Num r) :: Integer

data SignumSym0 :: Rational ~> Integer
type SignumSym1 :: Rational -> Integer

type instance Apply SignumSym0 i = Signum i
type SignumSym1 i = Signum i

-- | Singleton version of 'Signum'.
sSignum :: Sing (r :: Rational) -> SInteger (Signum r)
sSignum sr = P.sSignum (sNum sr)

-- | The 'Signum' of a 'Rational' equals the 'PNum' 'P.Signum' of its
-- 'Num'erator, as well as the 'Num'erator of its 'PNum' 'P.Signum'.
sSignumRefl
  :: SRational r
  -> (Signum r :~: P.Signum (Num r),
      Signum r :~: Num (P.Signum r))
sSignumRefl !_ = unsafeCoerce (Refl, Refl)

type Mul (a :: Rational) (b :: Rational) =
  Mul_ (Reduce a) (Reduce b) :: Rational
type family Mul_ (a :: Rational) (b :: Rational) where
  Mul_ (n1 :% d1) (n2 :% d2) = Reduce ((n1 P.* n2) :% (d1 L.* d2))

type Add (a :: Rational) (b :: Rational) =
  Add_ (Reduce a) (Reduce b) :: Rational
type family Add_ (a :: Rational) (r :: Rational) :: Rational where
  Add_ (an :% ad) (bn :% bd) =
    (an P.* I.FromNatural bd P.+ bn P.* I.FromNatural ad) % (ad L.* bd)

--------------------------------------------------------------------------------

-- | Singleton version of 'Recip'. Type-checks only with a non-zero @r@.
sRecip :: I.Z :% 1 < P.Abs r => SRational r -> SRational (Recip r)
sRecip = UnsafeSRational . recip . fromSing

-- | Like 'sRecip', except it fails with 'Nothing' when @r@ is zero, rather
-- than requiring to satisfy this as a type-level constraint.
sRecip' :: forall r. SRational r -> Maybe (SRational (Recip r))
sRecip' sa = case fromSing sa of
               0 -> Nothing
               a -> Just (UnsafeSRational (recip a))

-- | /'Recip'rocal/ of the type-level 'Rational'.
-- Also known as /multiplicative inverse/.
type Recip (a :: Rational) = Den a % Num a :: Rational

data RecipSym0 :: Rational ~> Rational
type RecipSym1 :: Rational -> Rational

type instance Apply RecipSym0 i = Recip i
type RecipSym1 i = Recip i

--------------------------------------------------------------------------------

-- | Singleton version of 'Div'.
sDiv :: I.SRound r -> SRational a -> SInteger (Div r a)
sDiv sr sa = withSomeSing (div (fromSing sr) (fromSing sa)) unsafeCoerce

-- | Quotient of the 'Div'ision of the 'Num'erator of type-level 'Rational' @a@
-- by its 'Den'ominator, using the specified 'I.Round'ing @r@.
--
-- @
-- forall (r :: 'I.Round') (a :: 'Rational').
--   ('KnownRational' a) =>
--     'Rem' r a  '=='  a '-' 'Div' r a t'%' 1
-- @
--
-- Use this to approximate a type-level 'Rational' to an 'Integer'.
type Div (r :: I.Round) (a :: Rational) = Div_ r (Reduce a) :: Integer
type Div_ (r :: I.Round) (a :: Rational) =
  I.Div r (Num a) (I.FromNatural (Den a)) :: Integer

data DivSym0 :: I.Round ~> Rational ~> Integer
data DivSym1 :: I.Round -> Rational ~> Integer
type DivSym2 :: I.Round -> Rational -> Integer

type instance Apply DivSym0 a = DivSym1 a
type instance Apply (DivSym1 a) b = DivSym2 a b
type DivSym2 a b = Div a b

-- | Singleton version of 'Rem'.
sRem :: I.SRound r -> SRational a -> SRational (Rem r a)
sRem sr sa = snd (sDivRem sr sa)

-- | 'Rem'ainder from 'Div'iding the 'Num'erator of the type-level 'Rational'
-- @a@ by its 'Den'ominator, using the specified 'I.Round'ing @r@.
--
-- @
-- forall (r :: 'I.Round') (a :: 'Rational').
--   ('KnownRational' a) =>
--     'Rem' r a  '=='  a '-' 'Div' r a t'%' 1
-- @
type Rem (r :: I.Round) (a :: Rational) = P.Snd (DivRem r a) :: Rational

data RemSym0 :: I.Round ~> Rational ~> Rational
data RemSym1 :: I.Round -> Rational ~> Rational
type RemSym2 :: I.Round -> Rational -> Rational

type instance Apply RemSym0 a = RemSym1 a
type instance Apply (RemSym1 a) b = RemSym2 a b
type RemSym2 a b = Rem a b

-- | Singleton version of 'DivRem'.
sDivRem :: I.SRound r -> SRational a
        -> (SInteger (Div r a), SRational (Rem r a))
sDivRem sr sa =
  let (d1, r1) = divRem (fromSing sr) (fromSing sa)
  in (withSomeSing d1 unsafeCoerce, UnsafeSRational r1)

-- | Get both the quotient and the 'Rem'ainder of the 'Div'ision of the
-- 'Num'erator of type-level 'Rational' @a@ by its 'Den'ominator,
-- using the specified 'I.Round'ing @r@.
--
-- @
-- forall (r :: 'I.Round') (a :: 'Rational').
--   ('KnownRational' a) =>
--     'DivRem' r a  '=='  '('Div' r a, 'Rem' r a)
-- @
type DivRem (r :: I.Round) (a :: Rational) =
  DivRem_ r (Reduce a) :: (Integer, Rational)
type DivRem_ (r :: I.Round) (a :: Rational) =
  DivRem__ (Den a) (I.DivRem r (Num a) (I.FromNatural (Den a)))
    :: (Integer, Rational)
type DivRem__ (d :: Natural) (qm :: (Integer, Integer)) =
  '(P.Fst qm, Reduce (P.Snd qm :% d)) :: (Integer, Rational)

data DivRemSym0 :: I.Round ~> Rational ~> (Integer, Rational)
data DivRemSym1 :: I.Round -> Rational ~> (Integer, Rational)
type DivRemSym2 :: I.Round -> Rational -> (Integer, Rational)

type instance Apply DivRemSym0 a = DivRemSym1 a
type instance Apply (DivRemSym1 a) b = DivRemSym2 a b
type DivRemSym2 a b = DivRem a b

-- | Term-level version of 'Div'.
--
-- Takes a "KindRational".'Rational' as input, returns a "Prelude"
-- 'P.Integer'.
--
-- NB: 'error's if the 'P.Rational' denominator is 0.
div :: I.Round -> P.Rational -> P.Integer
div rnd = \(unsafeReduce -> n P.:% d) -> f n d
  where f = I.div rnd

-- | Term-level version of 'Rem'.
--
-- Takes a "Prelude".'P.Rational' as input, returns a "Prelude".'P.Rational'.
--
-- NB: 'error's if the 'P.Rational' denominator is 0.
rem :: I.Round -> P.Rational -> P.Rational
rem rnd = snd . divRem rnd

-- | Term-level version of 'DivRem'.
--
-- Takes a "Prelude".'P.Rational' as input, returns a pair of
-- /(quotient, reminder)/.
--
-- @
-- forall ('r' :: 'I.Round') (a :: 'P.Rational').
--   ('P.denominator' a 'P./=' 0) =>
--     'divRem' r a  'P.=='  ('div' r a, 'rem' r a)
-- @
--
-- NB: 'error's if the 'P.Rational' denominator is 0.
divRem :: I.Round -> P.Rational -> (P.Integer, P.Rational)
divRem rnd = \(unsafeReduce -> n P.:% d) ->
                 let (q, m) = f n d
                 in  (q, m P.:% d) -- (m :% d) == ((n :% d) - q)
  where f = I.divRem rnd

--------------------------------------------------------------------------------

-- | Determine whether @r@ is 'Terminating' or 'NonTerminating' at the
-- term-level, and create the corresponding type-level proof.
termination
  :: forall r a
  .  (NonTerminating r => a)
  -> (Terminating r => a)
  -> SRational r
  -> a
termination f t sr =
  withKnownRational sr $ case isTerminating (fromSRational sr) of
    False | Refl <- (unsafeCoerce Refl :: IsTerminating r :~: 'False) -> f
    True  | Refl <- (unsafeCoerce Refl :: IsTerminating r :~: 'True)  -> t

-- | This is essentially the same as @('KnownRational' r, 'IsTerminating' r ~ 'True')@,
-- except with a nicer error message when @'IsTerminating' r ~ 'False'@.
type Terminating (r :: Rational) = Terminating_ r (IsTerminating r) :: Constraint
type family Terminating_ r (b :: Bool):: Constraint where
  Terminating_ r 'True = (IsTerminating r ~ 'True, KnownRational r)
  Terminating_ r 'False = L.TypeError
    ('L.Text "Unexpected: IsTerminating ("
     'L.:<>: 'L.ShowType r 'L.:<>: 'L.Text ") ~ 'False")

data TerminatingSym0 :: Rational ~> Constraint
type TerminatingSym1 :: Rational -> Constraint

type instance Apply TerminatingSym0 i = Terminating i
type TerminatingSym1 i = Terminating i

-- | This is essentially the same as @('KnownRational' r, 'IsTerminating' r ~ 'False')@,
-- except with a nicer error message when @'IsTerminating' r ~ 'False'@.
type NonTerminating (r :: Rational) = NonTerminating_ r (IsTerminating r) :: Constraint
type family NonTerminating_ r (b :: Bool):: Constraint where
  NonTerminating_ r 'False = (IsTerminating r ~ 'False, KnownRational r)
  NonTerminating_ r 'True  = L.TypeError
    ('L.Text "Unexpected: IsTerminating ("
     'L.:<>: 'L.ShowType r 'L.:<>: 'L.Text ") ~ 'True")

data NonTerminatingSym0 :: Rational ~> Constraint
type NonTerminatingSym1 :: Rational -> Constraint

type instance Apply NonTerminatingSym0 i = NonTerminating i
type NonTerminatingSym1 i = NonTerminating i


-- | Whether the type-level 'Rational' is terminating. That is, whether
-- it can be fully represented as a finite decimal number.
type IsTerminating (r :: Rational) = IsTerminating_ (Den r) :: Bool
type family IsTerminating_ (n :: Natural) :: Bool where
  IsTerminating_ 5 = 'True
  IsTerminating_ 2 = 'True
  IsTerminating_ 1 = 'True
  IsTerminating_ d = IsTerminating_5 d (L.Mod d 5)

-- @IsTerminating_5@ is here to prevent @IsTerminating_@ from recursing into
-- @IsTerminating_ (Div d 5)@ if it would diverge.
type family IsTerminating_5 (d :: Natural) (md5 :: Natural) :: Bool where
  IsTerminating_5 d 0 = IsTerminating_ (L.Div d 5)
  IsTerminating_5 d _ = IsTerminating_2 d (L.Mod d 2)

-- @IsTerminating_2@ is here to prevent @IsTerminating_5@ from recursing into
-- @IsTerminating_ (Div d 2)@ if it would diverge, and also to prevent calculating
-- @Mod d 2@ unless necessary.
type family IsTerminating_2 (d :: Natural) (md2 :: Natural) :: Bool where
  IsTerminating_2 d 0 = IsTerminating_ (L.Div d 2)
  IsTerminating_2 _ _ = 'False

data IsTerminatingSym0 :: Rational ~> Bool
type IsTerminatingSym1 :: Rational -> Bool

type instance Apply IsTerminatingSym0 i = IsTerminating i
type IsTerminatingSym1 i = IsTerminating i

-- | Term-level version of the "IsTerminating" function.
--
-- NB: 'error's if the 'P.Rational' denominator is 0.
isTerminating :: P.Rational -> Bool
isTerminating = \(unsafeReduce -> _ P.:% d) -> go d
  where
    go = \case
      5 -> True
      2 -> True
      1 -> True
      n | (q, 0) <- P.divMod n 5 -> go q
        | (q, 0) <- P.divMod n 2 -> go q
      _ -> False

{-# COMPLETE SRationalTerminating, SRationalNonTerminating #-}

-- | Matches a 'SRational' that is 'Terminating'.
pattern SRationalTerminating
  :: forall r. () => (Terminating r) => SRational r
pattern SRationalTerminating <-
  (termination Nothing (Just Dict) -> Just (Dict :: Dict (Terminating r)))

-- | Matches a 'SRational' that is 'NonTerminating'.
pattern SRationalNonTerminating
  :: forall r. () => (NonTerminating r) => SRational r
pattern SRationalNonTerminating <-
  (termination (Just Dict) Nothing -> Just (Dict :: Dict (NonTerminating r)))

--------------------------------------------------------------------------------

-- | Comparison of type-level 'Rational's, as a function.
type CmpRational (a :: Rational) (b :: Rational) =
  CmpRational_ (Reduce a) (Reduce b) :: Ordering
type family CmpRational_ (a :: Rational) (b :: Rational) :: Ordering where
  CmpRational_ a a = 'EQ
  CmpRational_ (an :% ad) (bn :% bd) =
    P.Compare (an P.* I.FromNatural bd) (bn P.* I.FromNatural ad)

-- | "Data.Type.Ord" support for type-level 'Rational's.
type instance Compare (a :: Rational) (b :: Rational) = CmpRational a b

instance OrdS.POrd Rational where
  type Compare a b = CmpRational a b

instance OrdS.SOrd Rational where
  sCompare sa sb = case compare (fromSing sa) (fromSing sb) of
    LT -> unsafeCoerce OrdS.SLT
    EQ -> unsafeCoerce OrdS.SEQ
    GT -> unsafeCoerce OrdS.SGT

instance EqS.PEq Rational where
  type a == b = CmpRational a b P.== 'EQ

instance EqS.SEq Rational where
  sa %== sb
    | fromSing sa P.== fromSing sb = unsafeCoerce STrue
    | otherwise                    = unsafeCoerce SFalse

--------------------------------------------------------------------------------

-- | This class gives the 'SRational' associated with a type-level 'Rational'.
--
-- There are instances for every 'Reduced' 'Rational'.

-- Note: Ideally, 'KnownRational' wouldn' exist and the 'Constraint's metioned
-- there would be superclasses to 'KnownRational_'. However, 'withDict' doesn't
-- allow superclasses, so we treat 'KnownRational_' as internal an export
-- 'KnownRational' only.
class KnownRational_ (r :: Rational) where
  rationalSing_ :: SRational r

-- | Type-level 'Rational's satisfying 'KnownRational' can be converted to
-- 'SRational's using 'rationalSing'. Moreover, 'KnownRational' implies that
-- the numerator is a 'I.KnownInteger', and that the denominator is a
-- 'L.KnownNat'.
type KnownRational (r :: Rational) =
  ( KnownRational_ r
  , Reduced r ~ r
  , I.KnownInteger (Num r)
  , L.KnownNat (Den r)
  )

-- | Convert an implicit 'KnownRational' to an explicit 'SRational'.
rationalSing :: KnownRational r => SRational r
rationalSing = rationalSing_ -- The difference is in the constraint.
{-# INLINE rationalSing #-}

instance (KnownRational r) => KnownRational_ r where
  rationalSing_ =
    UnsafeSRational (demote @(Num r) P.:% L.natVal (Proxy @(Den r)))

-- | Normalized term-level "Prelude" 'P.Rational' representation of the
-- type-level 'Rational' @r@.
rationalVal :: forall r proxy. KnownRational r => proxy r -> P.Rational
rationalVal _ = case rationalSing :: SRational r of UnsafeSRational x -> x
{-# INLINE rationalVal #-}

-- | This type represents unknown type-level 'Rational'.
data SomeRational = forall n. KnownRational n => SomeRational (Proxy n)


-- | Convert a term-level "Prelude".'P.Rational' into an
-- extistentialized 'KnownRational' wrapped in 'SomeRational'.
--
-- NB: 'error's if a non-'Reduced' 'P.Rational' is given.
someRationalVal :: P.Rational -> SomeRational
someRationalVal = \r ->
  withSomeSRational r $ \(sr :: SRational r) ->
  withKnownRational sr $ SomeRational (Proxy @r)

instance Eq SomeRational where
  SomeRational x == SomeRational y =
    rationalVal x P.== rationalVal y
  {-# INLINE (==) #-}

instance Ord SomeRational where
  compare (SomeRational x) (SomeRational y) =
    compare (rationalVal x) (rationalVal y)
  {-# INLINE compare #-}

-- | As for "Prelude".'P.Rational'.
instance Show SomeRational where
  showsPrec p (SomeRational i) = showsPrec p (rationalVal i)

-- | As for "Prelude".'P.Rational'.
instance Read SomeRational where
  readPrec = fmap someRationalVal Read.readPrec

--------------------------------------------------------------------------------

-- | We either get evidence that this function was instantiated with the
-- same type-level 'Rational's, or 'Nothing'.
sameRational
  :: forall a b proxy1 proxy2
  .  (KnownRational a, KnownRational b)
  => proxy1 a
  -> proxy2 b
  -> Maybe (a :~: b)
sameRational _ _ = testEquality (rationalSing @a) (rationalSing @b)

-- | Like 'sameRational', but if the type-level 'Rational's aren't equal, this
-- additionally provides proof of 'LT' or 'GT'.
cmpRational
  :: forall a b proxy1 proxy2
  .  (KnownRational a, KnownRational b)
  => proxy1 a
  -> proxy2 b
  -> OrderingI a b
cmpRational x y = case compare (rationalVal x) (rationalVal y) of
    EQ -> case unsafeCoerce Refl :: CmpRational a b :~: 'EQ of
      Refl -> case unsafeCoerce Refl :: a :~: b of
        Refl -> EQI
    LT -> case unsafeCoerce Refl :: (CmpRational a b :~: 'LT) of
      Refl -> LTI
    GT -> case unsafeCoerce Refl :: (CmpRational a b :~: 'GT) of
      Refl -> GTI

--------------------------------------------------------------------------------

-- | Singleton type for a type-level 'Rational' @r@.
newtype SRational (r :: Rational) = UnsafeSRational P.Rational
type role SRational nominal

-- | A explicitly bidirectional pattern synonym relating an 'SRational' to a
-- 'KnownRational' constraint.
--
-- As an __expression__: Constructs an explicit @'SRational' r@ value from an
-- implicit @'KnownRational' r@ constraint:
--
-- @
-- 'SRational' @r :: 'KnownRational' r => 'SRational' r
-- @
--
-- As a __pattern__: Matches on an explicit @'SRational' r@ value bringing
-- an implicit @'KnownRational' r@ constraint into scope:
--
-- @
-- f :: 'SRational' r -> ..
-- f SRational = {- SRational r in scope -}
-- @
pattern SRational :: forall r. () => KnownRational r => SRational r
pattern SRational <- (knownRationalInstance -> KnownRationalInstance)
  where SRational = rationalSing_

-- | An internal data type that is only used for defining the 'SRational' pattern
-- synonym.
data KnownRationalInstance (r :: Rational) where
  KnownRationalInstance :: KnownRational r => KnownRationalInstance r

-- | An internal function that is only used for defining the 'SRational' pattern
-- synonym.
knownRationalInstance :: SRational r -> KnownRationalInstance r
knownRationalInstance si = withKnownRational si KnownRationalInstance

instance Show (SRational r) where
  showsPrec p (UnsafeSRational r) =
    showParen (p >= appPrec1) $
      showString "SRational @" .
      showsPrecLit appPrec1 r

instance Eq (SRational r) where
  _ == _ = True
  {-# INLINE (==) #-}

instance Ord (SRational r) where
  compare _ _ = EQ
  {-# INLINE compare #-}

instance TestEquality SRational where
  testEquality = decideEquality
  {-# INLINE testEquality #-}

instance TestCoercion SRational where
  testCoercion = decideCoercion
  {-# INLINE testCoercion #-}

-- | Return the term-level "Prelude".'P.Rational' number corresponding to @r@.
fromSRational :: SRational r -> P.Rational
fromSRational (UnsafeSRational r) = r

-- | Convert an explicit @'SRational' r@ value into an implicit
-- @'KnownRational_' r@ constraint.
withKnownRational_
  :: forall r rep (x :: TYPE rep)
  .  SRational r
  -> (KnownRational_ r => x)
  -> x
withKnownRational_ = withDict @(KnownRational_ r)

-- | Convert an explicit @'SRational' r@ value into an implicit
-- @'KnownRational' r@ constraint.
withKnownRational
  :: forall r rep (x :: TYPE rep)
  .  SRational r
  -> (KnownRational r => x)
  -> x
withKnownRational sr x
  | n P.:% d <- fromSRational sr
  , I.SomeInteger @n _ <- I.someIntegerVal n
  , L.SomeNat @d _ <- L.someNatVal (P.fromInteger d)
  , Refl <- sReducedRefl sr
  , -- These unsafeCoreces are safe because this module doesn't offer any tool
    -- for constructing non-reduced SRationals. Very unsafe otherwise.
    Refl <- unsafeCoerce Refl :: d :~: Den r
  , Refl <- unsafeCoerce Refl :: n :~: Num r
  = withKnownRational_ sr x

-- | Convert a "Prelude".'P.Rational' number into an @'SRational' n@ value,
-- where @n@ is a fresh type-level 'Rational'.
withSomeSRational
  :: forall rep (x :: TYPE rep). P.Rational -> (forall r. SRational r -> x) -> x
withSomeSRational (unsafeReduced -> !r) k = k (UnsafeSRational r)
-- It's very important to keep this NOINLINE! See the docs at "GHC.TypeNats"
{-# NOINLINE withSomeSRational #-}

--------------------------------------------------------------------------------

type instance Sing = SRational

instance (KnownRational r) => SingI (r :: Rational) where
  sing = rationalSing
  {-# INLINE sing #-}

instance SingKind Rational where
  type Demote Rational = P.Rational
  fromSing = fromSRational
  {-# INLINE fromSing #-}
  toSing r = withSomeSRational r SomeSing
  {-# INLINE toSing #-}

instance SDecide Rational where
  UnsafeSRational l %~ UnsafeSRational r =
    -- This is safe because this library doesn't expose any tool to construct
    -- non-normalized SRationals. Otherwise, very unsafe.
    case l P.== r of
      True  -> Proved (unsafeCoerce Refl)
      False -> Disproved (\Refl -> error "Rational: SDecide")

--------------------------------------------------------------------------------
-- Extra stuff that doesn't belong here.

data Dict (c :: Constraint) where
  Dict :: c => Dict c

