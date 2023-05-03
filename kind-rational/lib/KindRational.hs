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
-- The implementation details are the same as the ones for type-level 'Natural's
-- in "GHC.TypeNats" as of @base-4.18@, and it will continue to evolve together
-- with @base@, trying to follow its core API as much as possible until the day
-- @base@ provides its own type-level rationals, making this module redundant.
module KindRational {--}
  ( -- * Rational kind
    Rational
  , rational
  , type (%)
  , type (/)
  , Num
  , Den
  , showsPrecTypeLit
  , readPrecTypeLit
  , Normalize
  , normalize

    -- * Types ⇔ Terms
  , KnownRational
  , rationalSing
  , rationalVal
  , SomeRational(..)
  , someRationalVal
  , sameRational

    -- * Singletons
  , SRational
  , pattern SRational
  , fromSRational
  , withSomeSRational
  , withKnownRational

    -- * Arithmethic
  , type (+)
  , type (*)
  , type (-)
  , Negate
  , Sign
  , Abs
  , Recip
  , Div
  , div
  , Rem
  , rem
  , DivRem
  , divRem
  , I.Round(..)

    -- * Decimals
  , Terminates
  , terminates
  , termination
  , Terminating
  , pattern SRationalTerminates
  , pattern SRationalTerminatesNot

    -- * Comparisons
  , CmpRational
  , cmpRational

    -- * Extra
    --
    -- | This stuff should be exported by the "Data.Type.Ord" module.
  , type (==?), type (==), type (/=?), type (/=)
  ) --}
  where

import Control.Monad
import Data.Proxy
import Data.Singletons
import Data.Singletons.Decide
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
import KindInteger (Integer, N, P, Z)
import KindInteger (type (==?), type (==), type (/=?), type (/=))
import KindInteger qualified as I
import Numeric.Natural (Natural)
import Prelude hiding (Rational, Integer, Num, div, rem)
import Prelude qualified as P
import Text.ParserCombinators.ReadP as ReadP
import Text.ParserCombinators.ReadPrec as ReadPrec
import Text.Read.Lex qualified as Read
import Unsafe.Coerce(unsafeCoerce)

--------------------------------------------------------------------------------

-- | Type-level version of 'P.Rational', used only as a kind.
--
-- Use '/' to construct one, use '%' to pattern-match on it.
data Rational = Integer :% Natural -- ^ See the docs for '%'.

{-# COMPLETE UnsafeNormalizedRational #-}
pattern UnsafeNormalizedRational
  :: HasCallStack => P.Integer -> Natural -> P.Rational
pattern UnsafeNormalizedRational n d <-
  (pattern_UnsafeNormalizedRational -> n P.:% (fromInteger -> d))

pattern_UnsafeNormalizedRational :: HasCallStack => P.Rational -> P.Rational
pattern_UnsafeNormalizedRational = \(n P.:% d) ->
  case rational n d of
    Left e  -> error ("KindRational: " <> show e)
    Right r -> r

-- | Shows the "Prelude".'P.Rational' as it would appear literally at
-- the type-level.
--
-- @
-- 'show' ('demote' \@('N' 0 '%' 4)) == \"N 0 % 4\"
-- @
--
-- NB: 'error's if a non-'Normalized' 'P.Rational' is given.
showsPrecTypeLit :: Int -> P.Rational -> ShowS
showsPrecTypeLit p (UnsafeNormalizedRational n d) =
  showParen (p >= appPrec1) $
    I.showsPrecTypeLit appPrec n .
    showString " % " .
    shows d

-- | Inverse of 'showsPrecTypeLit'.
readPrecTypeLit :: ReadPrec.ReadPrec P.Rational
readPrecTypeLit = Read.parens $ do
    n :: P.Integer <- Read.parens $ ReadPrec.step I.readPrecTypeLit
    Read.expectP (Read.Symbol "%")
    d :: Natural <- Read.parens $ ReadPrec.step $ ReadPrec.lift pNatural
    either fail pure $ rational n (toInteger d)
  where
    pNatural :: ReadP.ReadP Natural
    pNatural = read <$> ReadP.munch1 (\c -> c >= '0' && c <= '9')

-- | Creates a "Prelude".'P.Rational' using the given literal numerator
-- and denominator, if they are already normalized.
rational :: P.Integer -> P.Integer -> Either String P.Rational
rational = \n d -> do
    when (d == 0) $ Left "Denominator is zero"
    when (d < 0) $ Left notNormMsg
    let r@(n' P.:% d') = n P.% d
    when (n /= n' || d /= d') $ Left notNormMsg
    Right r
  where
    notNormMsg = "Rational is not normalized. Use Data.Ratio.% or \
                 \KindRational.normalize to normalize it."

-- | Normalizes a "Prelude".'P.Rational'. 'Nothing' if denominator is zero.
normalize :: P.Rational -> Maybe P.Rational
normalize = \(n P.:% d) -> (n P.% d) <$ guard (d /= 0)

--------------------------------------------------------------------------------

-- | 'Normalize'd /'Num'erator/ of the type-level 'Rational'.
type Num (r :: Rational) = Num_ (Normalize r) :: Integer
type family Num_ (r :: Rational) :: Integer where
  Num_ (n % _) = n

-- | 'Normalize'd /'Den'ominator/ of the type-level 'Rational'.
type Den (r :: Rational) = Den_ (Normalize r) :: Natural
type family Den_ (r :: Rational) :: Natural where
  Den_ (_ % d) = d

-- | Pattern-match on a type-level 'Rational'.
--
-- When /constructing/ 'Rational's, use '/' instead, which not only accepts
-- more polymorphic inputs, but also 'Normalize's the result.
--
-- Also note that while @n '%' 0@ is a valid type or non-'Normalize'd types
-- like @2 '%' 4@ type-check on their own, all tools in the "KindRational"
-- module will reject such non-'Normalize'd input.
type (n :: I.Integer) % (d :: Natural) = n :% d :: Rational

-- | Normalize a type-level 'Rational' so that a /0/ denominator fails to
-- type-check, and that the 'Num'erator and denominator have no common factors.
--
-- Only 'Normalize'd 'Rational's can be reliably constrained for equality
-- using '~'. Only normalized 'Rational's are 'KnownRational's.
--
-- All of the type-level functions in the "KindRational" module accept both
-- 'Normalize'd and non-'Normalize'd inputs, but they always produce
-- 'Normalize'd output.
type family Normalize (r :: Rational) :: Rational where
  Normalize (_ % 0) = L.TypeError ('L.Text "KindRational: Denominator is zero")
  Normalize (Z   % _) = Z % 1
  Normalize (P 0 % _) = Z % 1
  Normalize (N 0 % _) = Z % 1
  Normalize (P n % d) = PP (L.Div n (GCD n d)) % L.Div d (GCD n d)
  Normalize (N n % d) = NN (L.Div n (GCD n d)) % L.Div d (GCD n d)

-- | Construct a 'Normalize'd 'N'egative type-level 'I.Integer'.
--
-- | To be used for producing all negative outputs in this module.
type NN (a :: Natural) = I.Normalize (N a) :: I.Integer

-- | Construct a 'Normalize'd 'P'ositive type-level 'I.Integer'.
--
-- To be used for producing all positive outputs in this module.
type PP (a :: Natural) = I.Normalize (P a) :: I.Integer

--------------------------------------------------------------------------------

infixl 6 +, -
infixl 7 *, /


type (/) :: kn -> kd -> Rational
-- | @n'/'d@ constructs and 'Normalize's a type-level 'Rational'
-- with numerator @n@ and denominator @d@.
--
-- This type-family accepts any combination of 'Natural', 'Integer' and
-- 'Rational' as input.
--
-- @
-- ('/') :: 'Natural'  -> 'Natural'  -> 'Rational'
-- ('/') :: 'Natural'  -> 'Integer'  -> 'Rational'
-- ('/') :: 'Natural'  -> 'Rational' -> 'Rational'
--
-- ('/') :: 'Integer'  -> 'Natural'  -> 'Rational'
-- ('/') :: 'Integer'  -> 'Integer'  -> 'Rational'
-- ('/') :: 'Integer'  -> 'Rational' -> 'Rational'
--
-- ('/') :: 'Rational' -> 'Natural'  -> 'Rational'
-- ('/') :: 'Rational' -> 'Integer'  -> 'Rational'
-- ('/') :: 'Rational' -> 'Rational' -> 'Rational'
-- @
--
-- It's not possible to pattern-match on @n'/'d@.  Instead, you must
-- pattern match on @x'%'y@, where @x'%'y ~ n'/'d@.
type family n / d :: Rational where
  -- Natural/Natural
  (n :: Natural) / (d :: Natural) = Normalize (P n % d)
  -- Natural/Integer
  (n :: Natural) / (Z   :: Integer) = Normalize (P n % 0)
  (n :: Natural) / (P d :: Integer) = Normalize (P n % d)
  (n :: Natural) / (N d :: Integer) = Normalize (N n % d)
  -- Natural/Rational
  (n :: Natural) / (d :: Rational) = (P n % 1) * Recip d
  -- Integer/Natural
  (i :: Integer) / (d :: Natural) = Normalize (i % d)
  -- Integer/Integer
  (n   :: Integer) / (Z   :: Integer) = Normalize (n % 0)
  (Z   :: Integer) / (P d :: Integer) = Normalize (Z % d)
  (Z   :: Integer) / (N d :: Integer) = Normalize (Z % d)
  (P n :: Integer) / (P d :: Integer) = Normalize (P n % d)
  (N n :: Integer) / (N d :: Integer) = Normalize (P n % d)
  (P n :: Integer) / (N d :: Integer) = Normalize (N n % d)
  (N n :: Integer) / (P d :: Integer) = Normalize (N n % d)
  -- Integer/Rational
  (n :: Integer) / (d :: Rational) = (n % 1) * Recip d
  -- Rational/Natural
  (n :: Rational) / (d :: Natural) = n * Recip (P d % 1)
  -- Rational/Integer
  (n :: Rational) / (d :: Integer) = n * Recip (d % 1)
  -- Rational/Rational
  (n :: Rational) / (d :: Rational) = n * Recip d

--------------------------------------------------------------------------------

-- | /'Negate'/ a type-level 'Rational'. Also known as /additive inverse/.
type family Negate (r :: Rational) :: Rational where
  Negate (Z   % d) = Normalize (Z   % d)
  Negate (P n % d) = Normalize (N n % d)
  Negate (N n % d) = Normalize (P n % d)

-- | Sign of type-level 'Rational's, as a type-level 'Integer'.
--
-- * @'P' 0@ if zero.
--
-- * @'P' 1@ if positive.
--
-- * @'N' 1@ if negative.
type Sign (r :: Rational) = I.Sign (Num r) :: Integer

-- | /'Abs'olute/ value of a type-level 'Rational'.
type Abs (r :: Rational) = P (I.Abs (Num_ r)) / Den_ r :: Rational

--------------------------------------------------------------------------------

-- | @a t'*' b@ multiplies type-level 'Rational's @a@ and @b@.
type (a :: Rational) * (b :: Rational) =
  Mul_ (Normalize a) (Normalize b) :: Rational
type family Mul_ (a :: Rational) (b :: Rational) where
  Mul_ (n1 % d1) (n2 % d2) = Normalize ((n1 I.* n2) % (d1 L.* d2))

-- | /'Recip'rocal/ of the type-level 'Rational'.
-- Also known as /multiplicative inverse/.
type Recip (a :: Rational) = Recip_ (Normalize a) :: Rational
type family Recip_ (a :: Rational) :: Rational where
  Recip_ (Z   % d) = Normalize (P d % 0)
  Recip_ (P n % d) = Normalize (P d % n)
  Recip_ (N n % d) = Normalize (N d % n)

-- | @a t'+' b@ adds type-level 'Rational's @a@ and @b@.
type (a :: Rational) + (b :: Rational) =
  Add_ (Normalize a) (Normalize b) :: Rational
type family Add_ (a :: Rational) (r :: Rational) :: Rational where
  Add_ (an % ad) (bn % bd) =
    Normalize ((an I.* P bd I.+ bn I.* P ad) % (ad L.* bd))

-- | @a t'-' b@ subtracts the type-level 'Rational' @b@ from
-- the type-level 'Rational' @a@.
type (a :: Rational) - (b :: Rational) = a + Negate b :: Rational


--------------------------------------------------------------------------------

-- | Quotient of the 'Div'ision of the 'Num'erator of type-level 'Rational' @a@
-- by its 'Den'ominator, using the specified 'I.Round'ing @r@.
--
-- @
-- forall (r :: 'I.Round') (a :: 'Rational').
--   ('Den' a '/=' 0) =>
--     'Rem' r a  '=='  a '-' 'Div' r a '%' 1
-- @
--
-- Use this to approximate a type-level 'Rational' to an 'Integer'.
type Div (r :: I.Round) (a :: Rational) =
  Div_ r (Normalize a) :: Integer
type Div_ (r :: I.Round) (a :: Rational) =
  I.Div r (Num_ a) (P (Den_ a)) :: Integer

-- | 'Rem'ainder from 'Div'iding the 'Num'erator of the type-level 'Rational'
-- @a@ by its 'Den'ominator, using the specified 'I.Round'ing @r@.
--
-- @
-- forall (r :: 'I.Round') (a :: 'Rational').
--   ('Den' a '/=' 0) =>
--     'Rem' r a  '=='  a '-' 'Div' r a '%' 1
-- @
type Rem (r :: I.Round) (a :: Rational) = Snd (DivRem r a) :: Rational

-- | Get both the quotient and the 'Rem'ainder of the 'Div'ision of the
-- 'Num'erator of type-level 'Rational' @a@ by its 'Den'ominator,
-- using the specified 'I.Round'ing @r@.
--
-- @
-- forall (r :: 'I.Round') (a :: 'Rational').
--   ('Den' a '/=' 0) =>
--     'DivRem' r a  '=='  '('Div' r a, 'Rem' r a)
-- @
type DivRem (r :: I.Round) (a :: Rational) =
  DivRem_ r (Normalize a) :: (Integer, Rational)
type DivRem_ (r :: I.Round) (a :: Rational) =
  DivRem__ (Den_ a) (I.DivRem r (Num_ a) (P (Den_ a))) :: (Integer, Rational)
type DivRem__ (d :: Natural) (qm :: (Integer, Integer)) =
  '(Fst qm, Normalize (Snd qm % d)) :: (Integer, Rational)

-- | Term-level version of 'Div'.
--
-- Takes a "KindInteger" 'Rational' as input, returns a "Prelude"
-- 'P.Integer'.
--
-- NB: 'error's if a non-'Normalized' 'P.Rational' is given.
div :: I.Round -> P.Rational -> P.Integer
div rnd = let f = I.div rnd
          in \(UnsafeNormalizedRational n d) -> f n (toInteger d)

-- | Term-level version of 'Rem'.
--
-- Takes a "Prelude".'P.Rational' as input, returns a "Prelude".'P.Rational'.
--
-- NB: 'error's if a non-'Normalized' 'P.Rational' is given.
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
-- NB: 'error's if a non-'Normalized' 'P.Rational' is given.
divRem :: I.Round -> P.Rational -> (P.Integer, P.Rational)
divRem rnd = let f = I.divRem rnd
             in \(UnsafeNormalizedRational n d0) ->
                    let d = toInteger d0
                        (q, m) = f n d
                    in  (q, m P.:% d) -- (m % d) == ((n % d) - q)

--------------------------------------------------------------------------------

-- | Determine whether @r@ 'Terminates' or not at the term-level, and bring
-- create the corresponding type-level proof.
termination
  :: forall r a
  .  (Terminates r ~ 'False => a)
  -> (Terminates r ~ 'True  => a)
  -> SRational r
  -> a
termination f t sr
  | terminates (fromSRational sr)
  , Refl <- unsafeCoerce Refl :: Terminates r :~: 'True  = t
  | Refl <- unsafeCoerce Refl :: Terminates r :~: 'False = f

-- | This is essentially the same as @'Terminates' r ~ 'True'@, except with
-- a nicer error message when @'Terminates' r ~ 'False'@.
type Terminating (r :: Rational) = Terminating_ r (Terminates r) :: Constraint
type family Terminating_ r (b :: Bool):: Constraint where
  Terminating_ r 'True = Terminates r ~ 'True
  Terminating_ r 'False = L.TypeError
    ('L.Text "Unexpected: KindRational.Terminates ("
     'L.:<>: 'L.ShowType r 'L.:<>: 'L.Text ") ~ 'False")

-- | Whether the type-level 'Rational' terminates. That is, whether
-- it can be fully represented as a finite decimal number.
type Terminates (r :: Rational) = Terminates_ (Den r) :: Bool
type family Terminates_ (n :: Natural) :: Bool where
  Terminates_ 5 = 'True
  Terminates_ 2 = 'True
  Terminates_ 1 = 'True
  Terminates_ d = Terminates_5 d (L.Mod d 5)

-- @Terminates_5@ is here to prevent @Terminates_@ from recursing into
-- @Terminates_ (Div d 5)@ if it would diverge.
type family Terminates_5 (d :: Natural) (md5 :: Natural) :: Bool where
  Terminates_5 d 0 = Terminates_ (L.Div d 5)
  Terminates_5 d _ = Terminates_2 d (L.Mod d 2)

-- @Terminates_2@ is here to prevent @Terminates_5@ from recursing into
-- @Terminates_ (Div d 2)@ if it would diverge, and also to prevent calculating
-- @Mod d 2@ unless necessary.
type family Terminates_2 (d :: Natural) (md2 :: Natural) :: Bool where
  Terminates_2 d 0 = Terminates_ (L.Div d 2)
  Terminates_2 _ _ = 'False

-- | Term-level version of the "Terminates" function.
terminates :: P.Rational -> Bool
terminates = \(UnsafeNormalizedRational _ d) -> go (toInteger d)
  where
    go = \case
      5 -> True
      2 -> True
      1 -> True
      n | (q, 0) <- P.divMod n 5 -> go q
        | (q, 0) <- P.divMod n 2 -> go q
      _ -> False

{-# COMPLETE SRationalTerminates, SRationalTerminatesNot #-}

-- | Matches a 'SRational' that 'Terminates'.
pattern SRationalTerminates
  :: forall r. () => (Terminates r ~ 'True) => SRational r
pattern SRationalTerminates <-
  (termination Nothing (Just Dict) -> Just (Dict :: Dict (Terminates r ~ 'True)))

-- | Matches a 'SRational' that does not 'Terminates'.
pattern SRationalTerminatesNot
  :: forall r. () => (Terminates r ~ 'False) => SRational r
pattern SRationalTerminatesNot <-
  (termination (Just Dict) Nothing -> Just (Dict :: Dict (Terminates r ~ 'False)))

--------------------------------------------------------------------------------

-- | Comparison of type-level 'Rational's, as a function.
type CmpRational (a :: Rational) (b :: Rational) =
  CmpRational_ (Normalize a) (Normalize b) :: Ordering
type family CmpRational_ (a :: Rational) (b :: Rational) :: Ordering where
  CmpRational_ a a = 'EQ
  CmpRational_ (an % ad) (bn % bd) = I.CmpInteger (an I.* P bd) (bn I.* P ad)

-- | "Data.Type.Ord" support for type-level 'Rational's.
type instance Compare (a :: Rational) (b :: Rational) = CmpRational a b

--------------------------------------------------------------------------------

-- | This class gives the 'SRational' associated with a type-level 'Rational'.
--
-- There are instances for every 'Normalize'd 'Rational'.

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
--
-- The 'Normalize' constraint means that types such as @'P' 2 '%' 4@ are not
-- 'KnownRational's. This restriction makes the rest of the "KindRational"
-- module easier to use, since we can assume that 'Rational's and 'SRational's
-- are always normalized everywhere.
type KnownRational (r :: Rational) =
  ( KnownRational_ r
  , Normalize r ~ r
  , I.KnownInteger (Num_ r)
  , L.KnownNat (Den_ r)
  )

-- | Convert an implicit 'KnownRational' to an explicit 'SRational'.
rationalSing :: KnownRational r => SRational r
rationalSing = rationalSing_ -- The difference is in the constraint.
{-# INLINE rationalSing #-}

instance (KnownRational (n % d)) => KnownRational_ (n % d) where
  rationalSing_ = UnsafeSRational (demote @n P.:% L.natVal (Proxy @d))

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
-- NB: 'error's if a non-'Normalized' 'P.Rational' is given.
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
      showsPrecTypeLit appPrec1 r

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
  , L.SomeNat @d _ <- L.someNatVal (fromInteger d)
  , -- These unsafeCoreces are safe because this module doesn't offer any tool
    -- for constructing non-normalized SRationals. Very unsafe otherwise.
    Refl <- unsafeCoerce Refl :: Normalize r :~: r
  , Refl <- unsafeCoerce Refl :: Num_ r :~: n
  , Refl <- unsafeCoerce Refl :: Den_ r :~: d
  = withKnownRational_ sr x

-- | Convert a "Prelude".'P.Rational' number into an @'SRational' n@ value,
-- where @n@ is a fresh type-level 'Rational'.
withSomeSRational
  :: forall rep (x :: TYPE rep). P.Rational -> (forall r. SRational r -> x) -> x
withSomeSRational r@UnsafeNormalizedRational{} k = k (UnsafeSRational r)
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
      False -> Disproved (\Refl -> error "KindRational.Rational: SDecide")

--------------------------------------------------------------------------------
-- Extra stuff that doesn't belong here.

-- | /Greatest Common Divisor/ of 'Natural' numbers @a@ and @b@.
type GCD (a :: Natural) (b :: Natural) = I.GCD (PP a) (PP b) :: Natural

type family Fst (ab :: (a, b)) :: a where Fst '(a, b) = a
type family Snd (ab :: (a, b)) :: b where Snd '(a, b) = b

data Dict (c :: Constraint) where
  Dict :: c => Dict c
