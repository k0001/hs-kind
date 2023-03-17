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
  , type (%)
  , type (/)
  , Normalize
  , Num
  , Den

    -- Prelude support
  , rational
  , fromPrelude
  , toPrelude

    -- * Types ⇔ Terms
  , KnownRational(rationalSing)
  , rationalVal
  , SomeRational(..)
  , someRationalVal
  , sameRational
  , showsPrecTypeLit

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
  , Mod
  , mod
  , Dif
  , dif
  , DivMod
  , divMod
  , DivDif
  , divDif
  , I.Round(..)

    -- * Decimals
  , Terminating
  , withTerminating
  , Terminates
  , terminates

    -- * Comparisons
  , CmpRational
  , cmpRational

    -- * Extra
    --
    -- | This stuff should be exported by the "Data.Type.Ord" module.
  , type (==?), type (==), type (/=?), type (/=)
  ) --}
  where

import qualified Control.Exception as Ex
import Control.Monad
import Data.Proxy
import Data.Type.Bool (If)
import Data.Type.Coercion
import Data.Type.Equality (TestEquality(..), (:~:)(..))
import Data.Type.Ord
import GHC.Base (WithDict(..))
import GHC.Read qualified as Read
import GHC.Real qualified as P (Ratio(..), (%))
import GHC.Show (appPrec, appPrec1)
import GHC.TypeLits qualified as L
import GHC.TypeNats qualified as N
import GHC.Types (TYPE, Constraint)
import KindInteger (Integer, N, P)
import KindInteger (type (==?), type (==), type (/=?), type (/=))
import KindInteger qualified as I
import Numeric.Natural (Natural)
import Prelude hiding (Rational, Integer, Num, div, mod, divMod)
import Prelude qualified as P
import Text.ParserCombinators.ReadPrec as Read
import Text.Read.Lex qualified as Read
import Unsafe.Coerce(unsafeCoerce)

--------------------------------------------------------------------------------

-- | Type-level version of 'P.Rational'. Use '/' to construct one, use '%' to
-- pattern-match on it.
--
-- 'Rational' is mostly used as a kind, with its types constructed
-- using '/'.  However, it might also be used as type, with its terms
-- constructed using 'rational' or 'fromPrelude'. One reason why you may want a
-- 'Rational' at the term-level is so that you embed it in larger data-types
-- (for example, this 'Rational' embeds the 'I.Integer' similarly offered by
-- the "KindInteger" module). But perhaps more importantly, this 'Rational'
-- offers better safety than the 'P.Rational' from "Prelude", since it's not
-- possible to construct one with a zero denominator, or so large that
-- operating with it would exhaust system resources. Notwithstanding this, for
-- ergonomic reasons, all of the functions exported by this module take
-- "Prelude" 'Rational's as input and produce "Prelude" 'Rational's as outputs.
-- Internally, however, the beforementioned checks are always performed, and
-- fail with 'Ex.throw' if necessary. If you want to be sure those 'error's
-- never happen, just filter your "Prelude" 'Rational's with 'fromPrelude'. In
-- practice, it's very unlikely that you will be affected by this unless if
-- you are unsafelly constructing "Prelude" 'Rational's.
data Rational
  = -- | This constructor is /unsafe/ because it doesn't check for the things
    -- that 'rational' checks for.
    --
    -- * At the term-level, safely construct a 'Rational' using 'rational'
    -- or 'fromPrelude' instead.
    --
    -- * At the type-level, safely construct a 'Rational' using '/'.
    I.Integer :% Natural

num :: Rational -> I.Integer
num (n :% _) = n

den :: Rational -> Natural
den (_ :% d) = d

instance Eq Rational where
  a == b = toPrelude a == toPrelude b

instance Ord Rational where
  compare a b = compare (toPrelude a) (toPrelude b)
  a <= b = toPrelude a <= toPrelude b

-- | Same as "Prelude" 'P.Rational'.
instance Show Rational where
  showsPrec p = showsPrec p . toPrelude

-- | Same as "Prelude" 'P.Rational'.
instance Read Rational where
  readPrec = Read.parens $ Read.prec 7 $ do  -- 7 is GHC.Real.ratioPrec
    n :: P.Integer <- Read.step Read.readPrec
    Read.expectP (Read.Symbol "%")
    d :: P.Integer <- Read.step Read.readPrec
    Just r <- pure (rational n d)
    pure r

-- | Shows the 'Rational' as it appears literally at the type-level.
--
-- This is different from normal 'show' for 'Rational', which shows
-- the term-level value.
--
-- @
-- 'shows'            0 ('rationalVal' ('Proxy' \@(1'/'2))) \"z\" == \"1 % 2z\"
-- 'showsPrecTypeLit' 0 ('rationalVal' ('Proxy' \@(1'/'2))) \"z\" == \"P 1 % 2z\"
-- @
showsPrecTypeLit :: Int -> Rational -> ShowS
showsPrecTypeLit p r = showParen (p > appPrec) $
  I.showsPrecTypeLit appPrec (num r) . showString " % " . shows (den r)

-- | Make a term-level "KindRational" 'Rational' number, provided that
-- the numerator is not @0@, and that its numerator and denominator are
-- not so large that they would exhaust system resources. The 'Rational'
-- is 'Normalize'd.
rational :: (Integral num, Integral den) => num -> den -> Maybe Rational
rational = \(toInteger -> n) (toInteger -> d) -> do
    guard (d /= 0 && abs n <= max_ && abs d <= max_)
    pure $ let n1 P.:% d1 = n P.% d
           in I.fromPrelude n1 :% fromInteger d1
  where
    max_ :: P.Integer -- Some big enough number. TODO: Pick good number.
    max_ = 10 ^ (1000 :: Int)

-- | Try to obtain a term-level "KindRational" 'Rational' from a term-level
-- "Prelude" 'P.Rational'. This can fail if the "Prelude" 'P.Rational' is
-- infinite, or if it is so big that it would exhaust system resources.
--
-- @
-- 'fromPrelude' . 'toPrelude'      == 'Just'
-- 'fmap' 'toPrelude' . 'fromPrelude' == 'Just'
-- @
fromPrelude :: P.Rational -> Maybe Rational
fromPrelude (n P.:% d) = rational n d

-- | Like 'fromPrelude', but 'Ex.throw's in situations where
-- 'fromPrelude' fails with 'Nothing'.
unsafeFromPrelude :: P.Rational -> Rational
unsafeFromPrelude = \case
    n P.:% d
     | d == 0 -> Ex.throw Ex.RatioZeroDenominator
     | abs n > max_ || abs d > max_ -> Ex.throw Ex.Overflow
     | otherwise ->
       let n1 P.:% d1 = n P.% d
       in I.fromPrelude n1 :% fromInteger d1
  where
    max_ :: P.Integer -- Some big enough number. TODO: Pick good number.
    max_ = 10 ^ (1000 :: Int)

-- | Convert a term-level "KindRational" 'Rational' into a term-level
-- "Prelude" 'P.Rational'.
--
-- @
-- 'fromPrelude' . 'toPrelude'      == 'Just'
-- 'fmap' 'toPrelude' . 'fromPrelude' == 'Just'
-- @
toPrelude :: Rational -> P.Rational
toPrelude r = I.toPrelude (num r) P.:% toInteger (den r)

--------------------------------------------------------------------------------

-- | /'Num'erator/ of the type-level 'Rational'.
type Num (r :: Rational) = Num_ (Normalize r) :: Integer
type family Num_ (r :: Rational) :: Integer where
  Num_ (n :% _) = n

-- | /'Den'ominator/ of the type-level 'Rational'.
type Den (r :: Rational) = Den_ (Normalize r) :: Natural
type family Den_ (r :: Rational) :: Natural where
  Den_ (_ :% d) = d

-- | Pattern-match on a type-level 'Rational'.
--
-- __NB:__ When /constructing/ a 'Rational' number, prefer to use '/',
-- which not only accepts more polymorphic inputs, but also 'Normalize's
-- the type-level 'Rational'. Also note that while @n '%' 0@ is a valid
-- type, all tools in the "KindRational" will reject such input.
type (n :: I.Integer) % (d :: Natural) = n ':% d :: Rational

-- | Normalize a type-level 'Rational' so that a /0/ denominator fails to
-- type-check, and that the 'Num'erator and denominator have no common factors.
--
-- Only 'Normalize'd 'Rational's can be reliably constrained for equality
-- using '~'.
--
-- All of the functions in the "KindRational" module accept both
-- 'Normalize'd and non-'Normalize'd inputs, but they always produce
-- 'Normalize'd output.
type family Normalize (r :: Rational) :: Rational where
  Normalize (_ % 0) = L.TypeError ('L.Text "KindRational: Denominator is zero")
  Normalize (P 0 % _) = P 0 % 1
  Normalize (N 0 % _) = P 0 % 1
  Normalize (P n % d) = P (L.Div n (GCD n d)) % L.Div d (GCD n d)
  Normalize (N n % d) = N (L.Div n (GCD n d)) % L.Div d (GCD n d)

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
  (n :: Natural) / (P d :: Integer) = Normalize (P n % d)
  (n :: Natural) / (N d :: Integer) = Normalize (N n % d)
  -- Natural/Rational
  (n :: Natural) / (d :: Rational) = (P n % 1) * Recip d
  -- Integer/Natural
  (i :: Integer) / (d :: Natural) = Normalize (i % d)
  -- Integer/Integer
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
type Abs (r :: Rational) = Normalize (P (I.Abs (Num_ r)) % Den_ r) :: Rational

--------------------------------------------------------------------------------

-- | @a t'*' b@ multiplies type-level 'Rational's @a@ and @b@.
type (a :: Rational) * (b :: Rational) =
  Mul_ (Normalize a) (Normalize b) :: Rational
type family Mul_ (a :: Rational) (b :: Rational) where
  Mul_ (n1 % d1) (n2 % d2) = Normalize ((n1 I.* n2) % (d1 L.* d2))

-- | /Reciprocal/ of the type-level 'Rational'.
-- Also known as /multiplicative inverse/.
type Recip (a :: Rational) = Recip_ (Normalize a) :: Rational
type family Recip_ (a :: Rational) :: Rational where
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

-- | This class gives the rational associated with a type-level rational.
-- There are instances of the class for every rational.
class KnownRational (r :: Rational) where
  rationalSing :: SRational r

instance forall r n d.
  ( Normalize r ~ n % d
  , I.KnownInteger n
  , L.KnownNat d
  ) => KnownRational r where
  rationalSing = UnsafeSRational
    (I.fromPrelude (I.integerVal (Proxy @n)) :% N.natVal (Proxy @d))

-- | Term-level "KindRational" 'Rational' representation of the type-level
-- 'Rational' @r@.
rationalVal' :: forall r proxy. KnownRational r => proxy r -> Rational
rationalVal' _ = case rationalSing :: SRational r of
                   UnsafeSRational x -> x

-- | Term-level "Prelude" 'P.Rational' representation of the type-level
-- 'Rational' @r@.
rationalVal :: forall r proxy. KnownRational r => proxy r -> P.Rational
rationalVal = toPrelude . rationalVal'

-- | This type represents unknown type-level 'Rational'.
data SomeRational = forall n. KnownRational n => SomeRational (Proxy n)

-- | Convert a term-level "Prelude" 'Rational' into an unknown
-- type-level 'Rational'.
someRationalVal :: P.Rational -> SomeRational
someRationalVal r =
  withSomeSRational (unsafeFromPrelude r) $ \(sr :: SRational r) ->
    withKnownRational sr (SomeRational @r Proxy)

instance Eq SomeRational where
  SomeRational x == SomeRational y = rationalVal x P.== rationalVal y

instance Ord SomeRational where
  SomeRational x <= SomeRational y =
    rationalVal x <= rationalVal y
  compare (SomeRational x) (SomeRational y) =
    compare (rationalVal x) (rationalVal y)

instance Show SomeRational where
  showsPrec p (SomeRational x) = showsPrec p (rationalVal x)

instance Read SomeRational where
  readsPrec p xs = do (a, ys) <- readsPrec p xs
                      [(someRationalVal a, ys)]

--------------------------------------------------------------------------------

-- | 'Constraint' version of @'Terminates' r@. Satisfied by all type-level
-- 'Rational's that can be represented as a finite decimal number.

-- Written as a class rather than as a type-synonym so that downstream doesn't
-- need to use UndecidableSuperClasses.
class (KnownRational r, Terminates r ~ True)
  => Terminating (r :: Rational)

-- Note: Even if @Terminates r ~ 'False@, GHC shows our @TypeError@ first.
instance
  ( KnownRational r
  , Terminates r ~ 'True
  , If (Terminates r)
       (() :: Constraint)
       (L.TypeError ('L.Text "‘" 'L.:<>: 'L.ShowType r 'L.:<>:
                     'L.Text "’ is not a terminating "
                     'L.:<>: 'L.ShowType Rational))
  ) => Terminating r

withTerminating
  :: forall r a
  .  KnownRational r
  => (Terminating r => a)
  -> Maybe a
withTerminating g = do
  guard (terminates' (rationalVal' (Proxy @r)))
  case unsafeCoerce (Dict @(Terminating (P 1 % 1))) of
    (Dict :: Dict (Terminating r)) -> pure g

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
-- Takes a "Prelude" 'P.Rational' as input.
terminates :: P.Rational -> Bool
terminates = terminates' . unsafeFromPrelude

-- | Term-level version of the "Terminates" function.
-- Takes a "KindRational" 'P.Rational' as input.
terminates' :: Rational -> Bool
terminates' = go . den
  where
    go = \case
      5 -> True
      2 -> True
      1 -> True
      n | (q, 0) <- P.divMod n 5 -> go q
        | (q, 0) <- P.divMod n 2 -> go q
      _ -> False

--------------------------------------------------------------------------------

-- | Quotient of the 'Div'ision of the 'Num'erator of type-level 'Rational' @a@
-- by its 'Den'ominator, using the specified 'I.Round'ing @r@.
--
-- @
-- forall (r :: 'I.Round') (a :: 'Rational').
--   ('Den' a '/=' 0) =>
--     'Mod' r a  '=='  'Num' a 'I.-' 'P' ('Den' a) 'I.*' 'Div' r a
-- @
type Div (r :: I.Round) (a :: Rational) =
  Div_ r (Normalize a) :: Integer
type Div_ (r :: I.Round) (a :: Rational) =
  I.Div r (Num_ a) (P (Den_ a)) :: Integer

-- | 'Mod'ulus of the division of the 'Num'erator of type-level 'Rational'
-- @a@ by its 'Den'ominator, using the specified 'I.Round'ing @r@.
--
-- @
-- forall (r :: 'I.Round') (a :: 'Rational').
--   ('Den' a '/=' 0) =>
--     'Mod' r a  '=='  'Num' a 'I.-' 'P' ('Den' a) 'I.*' 'Div' r a
-- @
type Mod (r :: I.Round) (a :: Rational) = Snd (DivMod r a) :: Integer

-- | Get both the quotient and the 'Mod'ulus of the 'Div'ision of the
-- 'Num'erator of type-level 'Rational' @a@ by its 'Den'ominator,
-- using the specified 'I.Round'ing @r@.
--
-- @
-- forall (r :: 'I.Round') (a :: 'Rational').
--   ('Den' a '/=' 0) =>
--     'DivMod' r a  '=='  '('Div' r a, 'Mod' r a)
-- @
type DivMod (r :: I.Round) (a :: Rational) =
  DivMod_ r (Normalize a) :: (Integer, Integer)
type DivMod_ (r :: I.Round) (a :: Rational) =
  I.DivMod r (Num_ a) (P (Den_ a)) :: (Integer, Integer)

-- | 'Dif'ference of the type type-level 'Rational' @a@ and the 'Div'ision of
-- its 'Num'erator by its 'Den'ominator, using the specified 'I.Round'ing @r@.
--
-- @
-- forall (r :: 'I.Round') (a :: 'Rational').
--   ('Den' a '/=' 0) =>
--     'Dif' r a  '=='  a '-' 'Div' r a '%' 1
-- @
--
-- Note: We use the word /difference/ because talking about /remainder/ in this
-- context can be confusing, considering "Prelude"'s `rem`ainder function.
-- However, strictly speaking, @`Dif` r a@ is the 'Rational' that /remiains/
-- after performing the 'I.Round'ed 'Div'ision. So, yes, 'Dif' could potentially
-- have been called @Rem@ instead.
type Dif (r :: I.Round) (a :: Rational) = Snd (DivDif r a) :: Rational

-- | Get both the quotient and the 'Dif'ference of the 'Div'ision of the
-- 'Num'erator of type-level 'Rational' @a@ by its 'Den'ominator,
-- using the specified 'I.Round'ing @r@.
--
-- @
-- forall (r :: 'I.Round') (a :: 'Rational').
--   ('Den' a '/=' 0) =>
--     'DivDif' r a  '=='  '('Div' r a, 'Dif' r a)
-- @
type DivDif (r :: I.Round) (a :: Rational) =
  DivDif_ r (Normalize a) :: (Integer, Rational)
type DivDif_ (r :: I.Round) (a :: Rational) =
  DivDif__ a (Div_ r a) :: (Integer, Rational)
type DivDif__ (a :: Rational) (q :: Integer) =
  '(q, a - q :% 1) :: (Integer, Rational)

-- | Term-level version of 'Div'.
--
-- Takes a "Prelude" 'P.Rational' as input, returns a "Prelude" 'P.Integer'.
div :: I.Round -> P.Rational -> P.Integer
div r = \(n P.:% d) -> f n d
  where f = I.div r

-- | Term-level version of 'Div'.
--
-- Takes a "Prelude" 'P.Rational' as input, returns a "Prelude" 'P.Integer'.
mod :: I.Round -> P.Rational -> P.Integer
mod r = \(n P.:% d) -> f n d
  where f = I.mod r

-- | Term-level version of 'DivMod'.
-- Takes a "Prelude" 'P.Rational' as input, returns a pair of "Prelude"
-- 'P.Integer's /(quotient, modulus)/.
--
-- @
-- forall ('r' :: 'I.Round') (a :: 'P.Rational').
--   ('P.denominator' a 'P./=' 0) =>
--     'divMod' r a  'P.=='  ('div' r a, 'mod' r a)
-- @
divMod :: I.Round -> P.Rational -> (P.Integer, P.Integer)
divMod r = \(n P.:% d) -> f n d
  where f = I.divMod r

-- | Term-level version of 'Dif'.
--
-- Takes a "Prelude" 'P.Rational' as input, returns a "Prelude" 'P.Rational'.
dif :: I.Round -> P.Rational -> P.Rational
dif r = \a -> a - toRational (f a)
  where f = div r

-- | Term-level version of 'DivDif'.
--
-- Takes a "Prelude" 'P.Rational' as input, returns a pair of "Prelude"
-- 'P.Rational's /(quotient, difference)/.
--
-- @
-- forall ('r' :: 'I.Round') (a :: 'P.Rational').
--   ('P.denominator' a 'P./=' 0) =>
--     'divDif' r a  'P.=='  ('div' r a, 'dif' r a)
-- @
divDif :: I.Round -> P.Rational -> (P.Integer, P.Rational)
divDif r = \a -> let q = f a in (q, a - toRational q)
  where f = div r

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
newtype SRational (r :: Rational) = UnsafeSRational Rational

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
pattern SRational <- (knownRationalInstance -> KnownRationalegerInstance)
  where SRational = rationalSing

-- | An internal data type that is only used for defining the 'SRational' pattern
-- synonym.
data KnownRationalegerInstance (r :: Rational) where
  KnownRationalegerInstance :: KnownRational r => KnownRationalegerInstance r

-- | An internal function that is only used for defining the 'SRational' pattern
-- synonym.
knownRationalInstance :: SRational r -> KnownRationalegerInstance r
knownRationalInstance si = withKnownRational si KnownRationalegerInstance

instance Show (SRational r) where
  showsPrec p (UnsafeSRational r) = showParen (p > appPrec) $
    showString "SRational @" . showsPrecTypeLit appPrec1 r

instance TestEquality SRational where
  testEquality (UnsafeSRational x) (UnsafeSRational y) = do
    guard (toPrelude x P.== toPrelude y)
    pure (unsafeCoerce Refl)

instance TestCoercion SRational where
  testCoercion x y = fmap (\Refl -> Coercion) (testEquality x y)

-- | Return the term-level "Prelude" 'P.Rational' number corresponding
-- to @r@ in a @'SRational' r@ value.
fromSRational :: SRational r -> P.Rational
fromSRational (UnsafeSRational r) = toPrelude r

-- | Convert an explicit @'SRational' r@ value into an implicit
-- @'KnownRational' r@ constraint.
withKnownRational
  :: forall r rep (a :: TYPE rep). SRational r -> (KnownRational r => a) -> a
withKnownRational = withDict @(KnownRational r)

-- | Convert a "Prelude" 'P.Rational' number into an @'SRational' n@ value,
-- where @n@ is a fresh type-level 'Rational'.
withSomeSRational
  :: forall rep (a :: TYPE rep). Rational -> (forall r. SRational r -> a) -> a
withSomeSRational r k = k (UnsafeSRational r)
-- It's very important to keep this NOINLINE! See the docs at "GHC.TypeNats"
{-# NOINLINE withSomeSRational #-}

--------------------------------------------------------------------------------
-- Extra stuff that doesn't belong here.

-- | /Greatest Common Divisor/ of 'Natural' numbers @a@ and @b@.
type GCD (a :: Natural) (b :: Natural) = I.GCD (P a) (P b) :: Natural

data Dict c where Dict :: c => Dict c

type family Snd (ab :: (a, b)) :: b where Snd '(a, b) = b

