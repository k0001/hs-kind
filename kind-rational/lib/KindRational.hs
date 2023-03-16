{-# LANGUAGE MagicHash #-}
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
  , rationalVal'
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
  , type (+), type (*), type (-)
  , Negate, Sign, Abs, Div, Mod, Quot, Rem, Recip
    -- ** Term-level
    --
    -- | As a convenience, we offer term-level versions of some of the type-level
    -- arithmetic functions.
  , div, mod, quot, rem

    -- * Rounding
  , RoundUp
  , RoundDown
  , RoundZero
  , RoundAway
  , RoundHalfUp
  , RoundHalfDown
  , RoundHalfZero
  , RoundHalfAway
  , RoundHalfEven
  , RoundHalfOdd

    -- ** Term-level
    --
    -- | As a convenience, we offer term-level versions of the type-level
    -- rounding functions.
  , roundUp
  , roundDown
  , roundZero
  , roundAway
  , roundHalfUp
  , roundHalfDown
  , roundHalfZero
  , roundHalfAway
  , roundHalfEven
  , roundHalfOdd

    -- * Decimals
  , Terminating
  , withTerminating
  , Terminates

    -- ** Term-level
    --
    -- | As a convenience, we offer term-level versions of the type-level
    -- rounding functions.
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

import Control.Monad
import Data.Proxy
import Data.Type.Bool (If)
import Data.Type.Coercion
import Data.Type.Equality (TestEquality(..), (:~:)(..))
import Data.Type.Ord
import GHC.Base (WithDict(..))
import GHC.Prim (proxy#, Proxy#)
import GHC.Read qualified as Read
import GHC.Real qualified as P (Ratio(..), reduce)
import GHC.Show (appPrec, appPrec1)
import GHC.TypeLits qualified as L
import GHC.TypeNats qualified as N
import GHC.Types (TYPE, Constraint)
import KindInteger (Integer, N, P)
import KindInteger (type (==?), type (==), type (/=?), type (/=))
import KindInteger qualified as I
import Numeric.Natural (Natural)
import Prelude hiding (Rational, Integer, Num, div, mod, quot, rem)
import Prelude qualified as P
import Text.ParserCombinators.ReadPrec as Read
import Text.Read.Lex qualified as Read
import Unsafe.Coerce(unsafeCoerce)

--------------------------------------------------------------------------------

-- | Type-level version of 'P.Rational'. Use '/' to construct one, use '%' to
-- pattern-match on it.
--
-- __NB__: 'Rational' is mostly used as a kind, with its types constructed
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
-- fail with 'error' if necessary. If you want to be sure those 'error's never
-- happen, just filter your "Prelude" 'Rational's with 'fromPrelude'. In
-- practice, it's very unlikely that you will be affected by this, unless if
-- you are unsafely constructing "Prelude" 'Rational's.
data Rational = I.Integer :% Natural

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
    n <- Read.step Read.readPrec
    Read.expectP (Read.Symbol "%")
    d <- Read.step Read.readPrec
    Just r <- pure (rational n d)
    pure r

-- | Shows the 'Rational' as it appears literally at the type-level.
--
-- This is different from normal 'show' for 'Rational', which shows
-- the term-level value.
--
-- @
-- 'shows' 0 ('intervalVal' @(1 '%' 2)) \"z\" == \"1 % 2z\"
--
-- 'showsPrecTypeLit' 0 ('intervalVal' @(1 '%' 2)) \"z\" == \"P 1 % 2z\"
-- @
showsPrecTypeLit :: Int -> Rational -> ShowS
showsPrecTypeLit p (n :% d) = showParen (p > appPrec) $
  showsPrec appPrec n . showString " % " . shows d

-- | Make a term-level "KindRational" 'Rational' number, provided that
-- the numerator is not @0@, and that its numerator and denominator are
-- not so large that they would exhaust system resources.
rational :: P.Integer  -- ^ Numerator.
         -> P.Integer  -- ^ Denominator.
         -> Maybe Rational
rational = \n d -> do
    guard (d /= 0 && abs n < max_ && abs d < max_)
    let (n' P.:% d') = P.reduce n d
    pure (I.fromPrelude n' :% fromInteger d')
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

-- | Like 'fromPrelude', but fails with 'error' in situations where
-- 'fromPrelude' fails with 'Nothing'.
unsafeFromPrelude :: String -> P.Rational -> Rational
unsafeFromPrelude prefix = \r0 -> case fromPrelude r0 of
  Just r1 -> r1
  Nothing -> error ("KindRational." <> prefix <>
                    ": bad Prelude Rational (" <> show r0 <> ")")

-- | Like 'unsafeFromPrelude', but returns a "Prelude" 'P.Rational'.
unsafeCheckPrelude :: String -> P.Rational -> P.Rational
unsafeCheckPrelude pre = toPrelude . unsafeFromPrelude pre


-- | Convert a term-level "KindRational" 'Rational' into a term-level
-- "Prelude" 'P.Rational'.
--
-- @
-- 'fromPrelude' . 'toPrelude'      == 'Just'
-- 'fmap' 'toPrelude' . 'fromPrelude' == 'Just'
-- @
toPrelude :: Rational -> P.Rational
toPrelude (n :% d) = I.toPrelude n P.:% toInteger d

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
-- the type-level 'Rational'.
type (n :: I.Integer) % (d :: Natural) = n ':% d :: Rational

-- | Normalize a type-level 'Rational' so that a /0/ denominator fails to
-- type-check, and that the 'Num'erator and denominator have no common factors.
--
-- __NB__: All of the functions in the "KindRational" module accept both
-- 'Normalize'd and non-'Normalize'd inputs, but they always produce
-- 'Normalize'd output.
type family Normalize (r :: Rational) :: Rational where
  Normalize (_ % 0) = L.TypeError ('L.Text "KindRational: Denominator is 0")
  Normalize (P 0 % _) = P 0 % 1
  Normalize (N 0 % _) = P 0 % 1
  Normalize (P n % d) = P (L.Div n (GCD n d)) % L.Div d (GCD n d)
  Normalize (N n % d) = N (L.Div n (GCD n d)) % L.Div d (GCD n d)

--------------------------------------------------------------------------------

infixl 6 +, -
infixl 7 *, /


type (/) :: kn -> kd -> Rational
-- | @n@ '/' @d@ constructs and 'Normalize's a type-level 'Rational'
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
-- __NB__: It's not possible to pattern-match on @n '/' d@.
-- Instead, you must pattern match on a 'Normalize'd @n' t'%' b'@ if
-- necessary.
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

-- | Round a type-level 'Rational' number towards /zero/.
-- Also known as /truncate/.
type RoundZero (r :: Rational) = Quot r :: Integer

-- | Term-level version of 'RoundZero'.
-- Takes a "Prelude" 'P.Rational' as input, returns a "Prelude" 'P.Integer'.
roundZero :: P.Rational -> P.Integer
roundZero (unsafeCheckPrelude "roundZero" -> r) = P.truncate r

-- | Round a type-level 'Rational' number towards /±infinity/,
-- that is, /away/ from zero.
type RoundAway (r :: Rational) = RoundAway_ r :: Integer
type family RoundAway_ (r :: Rational) :: Integer where
  RoundAway_ (P n % d) = RoundUp   (P n % d)
  RoundAway_ (N n % d) = RoundDown (N n % d)

-- | Term-level version of 'RoundAway'.
-- Takes a "Prelude" 'P.Rational' as input, returns a "Prelude" 'P.Integer'.
roundAway :: P.Rational -> P.Integer
roundAway (unsafeCheckPrelude "roundAway" -> r)
  | r >= 0    = ceiling r
  | otherwise = floor r

-- | Round a type-level 'Rational' number towards /negative infinity/.
-- Also known as /floor/.
type RoundDown (r :: Rational) = Div r :: Integer

-- | Term-level version of 'RoundDown'.
-- Takes a "Prelude" 'P.Rational' as input, returns a "Prelude" 'P.Integer'.
roundDown :: P.Rational -> P.Integer
roundDown (unsafeCheckPrelude "roundDown" -> r) = floor r

-- | Round a type-level 'Rational' number towards /positive infinity/.
-- Also known as /ceiling/.
type RoundUp (r :: Rational) = RoundUp_ (Normalize r) :: Integer
type family RoundUp_ (r :: Rational) :: Integer where
  RoundUp_ (n % d) = P 1 I.+ RoundDown ((n I.- P 1) / d)

-- | Term-level version of 'RoundUp'.
-- Takes a "Prelude" 'P.Rational' as input, returns a "Prelude" 'P.Integer'.
roundUp :: P.Rational -> P.Integer
roundUp (unsafeCheckPrelude "roundUp" -> r) = ceiling r

-- | Round a type-level 'Rational' towards the closest type-level 'Integer'.
-- If equidistant to the two closests 'Integer's , round towards the
-- 'I.Even' one.
type RoundHalfEven (r :: Rational) =
  RoundHalfEven_ (Normalize r) (RoundDown r) (RoundUp r) :: Integer
type RoundHalfEven_ :: Rational -> Integer -> Integer -> Integer
type family RoundHalfEven_ r f c where
  RoundHalfEven_ r f c = If (r - f % 1 <? P 1 % 2) f
                            (If (c % 1 - r <? P 1 % 2) c
                                (If (I.Even f) f c))

-- | Term-level version of 'RoundHalfEven'.
-- Takes a "Prelude" 'P.Rational' as input, returns a "Prelude" 'P.Integer'.
roundHalfEven :: P.Rational -> P.Integer
roundHalfEven (unsafeCheckPrelude "roundHalfEven" -> r)
  | r - fr < hr = f
  | cr - r < hr = c
  | even f      = f
  | otherwise   = c
  where
    hr = 1 P.:% 2
    f  = floor r
    fr = f P.:% 1
    c  = ceiling r
    cr = c P.:% 1


-- | Round a type-level 'Rational' towards the closest type-level 'Integer'.
-- If equidistant to the two closests 'Integer's , round towards the
-- 'I.Odd' one.
type RoundHalfOdd (r :: Rational) =
  RoundHalfOdd_ (Normalize r) (RoundDown r) (RoundUp r) :: Integer
type RoundHalfOdd_ :: Rational -> Integer -> Integer -> Integer
type family RoundHalfOdd_ r f c where
  RoundHalfOdd_ r f c = If (r - f % 1 <? P 1 % 2) f
                           (If (c % 1 - r <? P 1 % 2) c
                               (If (I.Odd f) f c))

-- | Term-level version of 'RoundHalfOdd'.
-- Takes a "Prelude" 'P.Rational' as input, returns a "Prelude" 'P.Integer'.
roundHalfOdd :: P.Rational -> P.Integer
roundHalfOdd (unsafeCheckPrelude "roundHalfOdd" -> r)
  | r - fr < hr = f
  | cr - r < hr = c
  | odd f       = f
  | otherwise   = c
  where
    hr = 1 P.:% 2
    f  = floor r
    fr = f P.:% 1
    c  = ceiling r
    cr = c P.:% 1

-- | Round a type-level 'Rational' towards the closest type-level 'Integer'.
-- If equidistant to the two closests 'Integer's , round towards
-- /positive infinity/.
type RoundHalfUp (r :: Rational) =
  RoundHalfUp_ (Normalize r) (RoundDown r) (RoundUp r) :: Integer
type RoundHalfUp_ :: Rational -> Integer -> Integer -> Integer
type family RoundHalfUp_ r f c where
  RoundHalfUp_ r f c = If (r - f % 1 <? P 1 % 2) f c

-- | Term-level version of 'RoundHalfUp'.
-- Takes a "Prelude" 'P.Rational' as input, returns a "Prelude" 'P.Integer'.
roundHalfUp :: P.Rational -> P.Integer
roundHalfUp (unsafeCheckPrelude "roundHalfUp" -> r)
  | r - fr < hr = f
  | cr - r < hr = c
  | otherwise   = c
  where
    hr = 1 P.:% 2
    f  = floor r
    fr = f P.:% 1
    c  = ceiling r
    cr = c P.:% 1

-- | Round a type-level 'Rational' towards the closest type-level 'Integer'.
-- If equidistant to the two closests 'Integer's , round towards
-- /negative infinity/.
type RoundHalfDown (r :: Rational) =
  RoundHalfDown_ (Normalize r) (RoundDown r) (RoundUp r) :: Integer
type RoundHalfDown_ :: Rational -> Integer -> Integer -> Integer
type family RoundHalfDown_ r f c where
  RoundHalfDown_ r f c = If (c % 1 - r <? P 1 % 2) c f

-- | Term-level version of 'RoundHalfDown'.
-- Takes a "Prelude" 'P.Rational' as input, returns a "Prelude" 'P.Integer'.
roundHalfDown :: P.Rational -> P.Integer
roundHalfDown (unsafeCheckPrelude "roundHalfDown" -> r)
  | r - fr < hr = f
  | cr - r < hr = c
  | otherwise   = f
  where
    hr = 1 P.:% 2
    f  = floor r
    fr = f P.:% 1
    c  = ceiling r
    cr = c P.:% 1

-- | Round a type-level 'Rational' towards the closest type-level 'Integer'.
-- If equidistant to the two closests 'Integer's , round towards /zero/.
type RoundHalfZero (r :: Rational) =
  RoundHalfZero_ (Normalize r) (RoundDown r) (RoundUp r) :: Integer
type RoundHalfZero_ :: Rational -> Integer -> Integer -> Integer
type family RoundHalfZero_ r f c where
  RoundHalfZero_ r f c = If (r - f % 1 <? P 1 % 2) f
                            (If (c % 1 - r <? P 1 % 2) c
                                (RoundZero r))

-- | Term-level version of 'RoundHalfZero'.
-- Takes a "Prelude" 'P.Rational' as input, returns a "Prelude" 'P.Integer'.
roundHalfZero :: P.Rational -> P.Integer
roundHalfZero (unsafeCheckPrelude "roundHalfZero" -> r)
  | r - fr < hr = f
  | cr - r < hr = c
  | r >= 0      = f
  | otherwise   = c
  where
    hr = 1 P.:% 2
    f  = floor r
    fr = f P.:% 1
    c  = ceiling r
    cr = c P.:% 1

-- | Round a type-level 'Rational' towards the closest type-level 'Integer'.
-- If equidistant to the two closests 'Integer's , round towards /±infinity/,
-- that is, /away/ from zero.
type RoundHalfAway (r :: Rational) =
  RoundHalfAway_ (Normalize r) (RoundDown r) (RoundUp r) :: Integer
type RoundHalfAway_ :: Rational -> Integer -> Integer -> Integer
type family RoundHalfAway_ r f c where
  RoundHalfAway_ (P n % d) f c = RoundHalfUp_   (P n % d) f c
  RoundHalfAway_ (N n % d) f c = RoundHalfDown_ (P n % d) f c

-- | Term-level version of 'RoundHalfAway'.
-- Takes a "Prelude" 'P.Rational' as input, returns a "Prelude" 'P.Integer'.
roundHalfAway :: P.Rational -> P.Integer
roundHalfAway (unsafeCheckPrelude "roundHalfAway" -> r)
  | r - fr < hr = f
  | cr - r < hr = c
  | r >= 0      = c
  | otherwise   = f
  where
    hr = 1 P.:% 2
    f  = floor r
    fr = f P.:% 1
    c  = ceiling r
    cr = c P.:% 1

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
rationalVal_ :: forall r proxy. KnownRational r => proxy r -> Rational
rationalVal_ _ = case rationalSing :: SRational r of
                   UnsafeSRational x -> x

-- | Term-level "Prelude" 'P.Rational' representation of the type-level
-- 'Rational' @r@.
rationalVal :: forall r proxy. KnownRational r => proxy r -> P.Rational
rationalVal _ = rationalVal' (proxy# @r)

-- | Term-level "Prelude" 'P.Rational' representation of the type-level
-- 'Rational' @r@.
rationalVal' :: forall r. KnownRational r => Proxy# r -> P.Rational
rationalVal' _ = case rationalSing :: SRational r of
                   UnsafeSRational x -> toPrelude x

-- | This type represents unknown type-level 'Rational'.
data SomeRational = forall n. KnownRational n => SomeRational (Proxy n)

-- | Convert a term-level "Prelude" 'Rational' into an unknown
-- type-level 'Rational'.
someRationalVal :: P.Rational -> SomeRational
someRationalVal (unsafeFromPrelude "someRationalVal" -> r) =
  withSomeSRational r (\(sr :: SRational r) ->
  withKnownRational sr (SomeRational @r Proxy))

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
  guard (terminates_ (rationalVal_ (Proxy @r)))
  case unsafeCoerce (Dict @(Terminating (P 1 % 1))) of
    (Dict :: Dict (Terminating r)) -> pure g

-- | Whether the type-level 'Rational' terminates. That is, whether
-- it can be fully represented as a finite decimal number.
type Terminates (r :: Rational) = Terminates_ (Den r) :: Bool
type family Terminates_ (n :: Natural) :: Bool where
  Terminates_ 1 = 'True
  Terminates_ 2 = 'True
  Terminates_ 5 = 'True
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
terminates (unsafeFromPrelude "terminates" -> r) = terminates_ r

-- | Whether the given 'Rational' terminates. That is, whether
-- it can be represented as a finite decimal number.
terminates_ :: Rational -> Bool
terminates_ = \(_ :% d) -> go (toInteger d)
  where
    go = \case
      0 -> error "KindRational.terminates_: impossible"
      1 -> True
      2 -> True
      5 -> True
      n | (q, 0) <- divMod n 5 -> go q
        | (q, 0) <- divMod n 2 -> go q
      _ -> False

--------------------------------------------------------------------------------

-- | Division ('floor'ed) of the 'Num'erator by the 'Den'ominator of a
-- type-level 'Rational' number.
--
-- @
-- forall (r :: 'Rational').
--   'Num' r  == 'Div' r '*' 'Den' r + 'Mod' r
-- @
type Div (r :: Rational) = Div_ (Normalize r) :: Integer
type family Div_ (r :: Rational) :: Integer where
  Div_ (n % d) = I.Div n (P d)

-- | Term-level version of 'Div'.
-- Takes a "Prelude" 'P.Rational' as input, returns a "Prelude" 'P.Integer'.
div :: P.Rational -> P.Integer
div (unsafeCheckPrelude "div" -> n P.:% d) = P.div n d

-- | Remainder of the division ('floor'ed) of the 'Num'erator by the
-- 'Den'ominator of a type-level 'Rational' number.
--
-- @
-- forall (r :: 'Rational').
--   'Mod' r  '=='  'Num' r 'I.-' 'Den' r 'I.*' 'Div' r
-- @
type Mod (r :: Rational) = Mod_ (Normalize r) :: Integer
type Mod_ (r :: Rational) = Num_ r I.- P (Den_ r) I.* (Div_ r) :: Integer

-- | Term-level version of 'Mod'.
-- Takes a "Prelude" 'P.Rational' as input, returns a "Prelude" 'P.Integer'.
mod :: P.Rational -> P.Integer
mod (unsafeCheckPrelude "mod" -> (n P.:% d)) = n P.- d * P.div n d

-- | Division ('truncate'd) of the 'Num'erator by the 'Den'ominator of a
-- type-level 'Rational' number.
--
-- @
-- forall (r :: 'Rational').
--   r  ==  'Quot' r + 'Rem' r
-- @
type Quot (r :: Rational) = Quot_ (Normalize r) :: Integer
type family Quot_ (r :: Rational) :: Integer where
  Quot_ (n % d) = I.Quot n (P d)

-- | Term-level version of 'Quot'.
-- Takes a "Prelude" 'P.Rational' as input, returns a "Prelude" 'P.Integer'.
quot :: P.Rational -> P.Integer
quot (unsafeCheckPrelude "quot" -> n P.:% d) = P.quot n d

-- | Remainder of the division ('truncate'd) of the 'Num'erator by the
-- 'Den'ominator of a type-level 'Rational' number.
--
-- @
-- forall (r :: 'Rational').
--   'Rem' r  '=='  'Num' r 'I.-' 'Den' r 'I.*' 'Quot' r
-- @
type Rem (r :: Rational) = Rem_ (Normalize r) :: Integer
type Rem_ (r :: Rational) = Num_ r I.- P (Den_ r) I.* (Quot_ r) :: Integer

-- | Term-level version of 'Rem'.
-- Takes a "Prelude" 'P.Rational' as input, returns a "Prelude" 'P.Integer'.
quotRem :: P.Rational -> (P.Integer, P.Integer)
quotRem (unsafeCheckPrelude "rem" -> (n P.:% d)) =
 (P.quot n d, n P.- d * P.quot n d)

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

data Dict c where
  Dict :: c => Dict c
