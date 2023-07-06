{-# OPTIONS_GHC -Wno-missing-signatures -Wno-incomplete-uni-patterns #-}
module Main {--}
  ( main
  ) --}
  where

import Control.Applicative
import Control.Monad
import Data.List qualified as List
import Data.Maybe
import Data.Singletons
import Data.Type.Equality (TestEquality(..))
import Data.Type.Ord (type (<=), type (<))
import GHC.Exts (Constraint)
import GHC.Show (appPrec, appPrec1)
import Numeric.Natural (Natural)
import System.Exit
import Text.Read (readMaybe, readPrec_to_S)
import GHC.Real qualified as P
import Prelude hiding (Rational, Integer)
import Prelude qualified as P
import Prelude.Singletons qualified as P

import KindInteger (P, N, Z)
import KindInteger qualified as KI
import KindRational (type (%), type (:%))
import KindRational qualified as K

--------------------------------------------------------------------------------

data Dict (c :: Constraint) where
  Dict :: c => Dict c

--------------------------------------------------------------------------------
_testNum =  Dict
_testNum :: Dict
  ( K.Num (Z :% 1) ~ Z
  , K.Num (P 1 :% 1) ~ P 1
  , K.Num (P 2 :% 1) ~ P 2
  , K.Num (N 1 :% 1) ~ N 1
  , K.Num (N 2 :% 1) ~ N 2
  )

_testDen =  Dict
_testDen :: Dict
  ( K.Den (Z :% 1) ~ 1
  , K.Den (P 1 :% 1) ~ 1
  , K.Den (P 2 :% 1) ~ 1
  , K.Den (N 1 :% 1) ~ 1
  , K.Den (N 2 :% 1) ~ 1
  , K.Den (P 1 :% 2) ~ 2
  , K.Den (N 1 :% 2) ~ 2
  )

_testShow =  Dict
_testShow :: Dict
  ( P.Show_ (Z :% 1) ~ "0 % 1"
  , P.Show_ (P 1 :% 1) ~ "1 % 1"
  , P.Show_ (N 2 :% 1) ~ "(-2) % 1"
  )

_testShowLit =  Dict
_testShowLit :: Dict
  ( K.ShowLit (Z :% 1) ~ "Z :% 1"
  , K.ShowLit (P 1 :% 1) ~ "P 1 :% 1"
  , K.ShowLit (N 2 :% 1) ~ "N 2 :% 1"
  )

_testFromNatural =  Dict
_testFromNatural :: Dict
  ( Z :% 1 ~ K.FromNatural 0
  , P 1 :% 1 ~ K.FromNatural 1
  , P 2 :% 1 ~ K.FromNatural 2
  )

_testFromInteger =  Dict
_testFromInteger :: Dict
  ( Z :% 1 ~ K.FromInteger Z
  , P 1 :% 1 ~ K.FromInteger (P 1)
  , P 2 :% 1 ~ K.FromInteger (P 2)
  , N 1 :% 1 ~ K.FromInteger (N 1)
  , N 2 :% 1 ~ K.FromInteger (N 2)
  )

_testReduced =  Dict
_testReduced :: Dict
  ( Z   :% 1 ~ K.Reduced (Z   :% 1)
  , P 1 :% 1 ~ K.Reduced (P 1 :% 1)
  , P 2 :% 1 ~ K.Reduced (P 2 :% 1)
  , P 3 :% 1 ~ K.Reduced (P 3 :% 1)
  , P 4 :% 1 ~ K.Reduced (P 4 :% 1)
  , P 3 :% 2 ~ K.Reduced (P 3 :% 2)
  , P 1 :% 3 ~ K.Reduced (P 1 :% 3)
  , P 2 :% 3 ~ K.Reduced (P 2 :% 3)
  , P 4 :% 3 ~ K.Reduced (P 4 :% 3)
  , P 1 :% 4 ~ K.Reduced (P 1 :% 4)
  , P 3 :% 4 ~ K.Reduced (P 3 :% 4)
  , N 1 :% 1 ~ K.Reduced (N 1 :% 1)
  , N 2 :% 1 ~ K.Reduced (N 2 :% 1)
  , N 3 :% 1 ~ K.Reduced (N 3 :% 1)
  , N 4 :% 1 ~ K.Reduced (N 4 :% 1)
  , N 3 :% 2 ~ K.Reduced (N 3 :% 2)
  , N 1 :% 3 ~ K.Reduced (N 1 :% 3)
  , N 2 :% 3 ~ K.Reduced (N 2 :% 3)
  , N 4 :% 3 ~ K.Reduced (N 4 :% 3)
  , N 1 :% 4 ~ K.Reduced (N 1 :% 4)
  , N 3 :% 4 ~ K.Reduced (N 3 :% 4)
  )

_testReduce =  Dict
_testReduce :: Dict
  ( Z :% 1 ~ Z % 1
  , P 1 :% 1 ~ P 1 % 1
  , P 2 :% 1 ~ P 2 % 1
  , P 3 :% 1 ~ P 3 % 1
  , P 4 :% 1 ~ P 4 % 1
  , Z :% 1 ~ Z % 2
  , P 1 :% 1 ~ P 2 % 2
  , P 3 :% 2 ~ P 3 % 2
  , P 2 :% 1 ~ P 4 % 2
  , Z :% 1 ~ Z % 3
  , P 1 :% 3 ~ P 1 % 3
  , P 2 :% 3 ~ P 2 % 3
  , P 1 :% 1 ~ P 3 % 3
  , P 4 :% 3 ~ P 4 % 3
  , Z :% 1 ~ Z % 4
  , P 1 :% 4 ~ P 1 % 4
  , P 1 :% 2 ~ P 2 % 4
  , P 3 :% 4 ~ P 3 % 4
  , P 1 :% 1 ~ P 4 % 4
  , Z :% 1 ~ Z % 1
  , N 1 :% 1 ~ N 1 % 1
  , N 2 :% 1 ~ N 2 % 1
  , N 3 :% 1 ~ N 3 % 1
  , N 4 :% 1 ~ N 4 % 1
  , Z :% 1 ~ Z % 2
  , N 1 :% 2 ~ N 1 % 2
  , N 1 :% 1 ~ N 2 % 2
  , N 3 :% 2 ~ N 3 % 2
  , N 2 :% 1 ~ N 4 % 2
  , Z :% 1 ~ Z % 3
  , N 1 :% 3 ~ N 1 % 3
  , N 2 :% 3 ~ N 2 % 3
  , N 1 :% 1 ~ N 3 % 3
  , N 4 :% 3 ~ N 4 % 3
  , Z :% 1 ~ Z % 4
  , N 1 :% 4 ~ N 1 % 4
  , N 1 :% 2 ~ N 2 % 4
  , N 3 :% 4 ~ N 3 % 4
  , N 1 :% 1 ~ N 4 % 4
  )

_testNegate  = Dict
_testNegate :: Dict
  ( Z :% 1 ~ P.Negate (Z % 1)
  , Z :% 1 ~ P.Negate (Z % 2)

  , P 1 :% 1 ~ P.Negate (N 1 % 1)
  , P 1 :% 2 ~ P.Negate (N 1 % 2)

  , N 1 :% 1 ~ P.Negate (P 1 % 1)
  , N 1 :% 2 ~ P.Negate (P 1 % 2)

  , P 1 :% 1 ~ P.Negate (N 1 % 1)
  , P 2 :% 1 ~ P.Negate (N 2 % 1)

  , N 1 :% 1 ~ P.Negate (P 1 % 1)
  , N 2 :% 1 ~ P.Negate (P 2 % 1)
  )

_testSignum  = Dict
_testSignum :: Dict
  ( Z ~ K.Signum (Z % 1)
  , Z ~ K.Signum (Z % 2)

  , N 1 ~ K.Signum (N 1 % 1)
  , N 1 ~ K.Signum (N 1 % 2)

  , P 1 ~ K.Signum (P 1 % 1)
  , P 1 ~ K.Signum (P 1 % 2)

  , N 1 ~ K.Signum (N 1 % 1)
  , N 1 ~ K.Signum (N 2 % 1)

  , P 1 ~ K.Signum (P 1 % 1)
  , P 1 ~ K.Signum (P 2 % 1)
  )

_testAbs  = Dict
_testAbs :: Dict
  ( Z :% 1 ~ P.Abs (Z % 1)
  , Z :% 1 ~ P.Abs (Z % 2)
  , Z :% 1 ~ P.Abs (Z % 1)
  , Z :% 1 ~ P.Abs (Z % 2)

  , P 1 :% 1 ~ P.Abs (N 1 % 1)
  , P 1 :% 2 ~ P.Abs (N 1 % 2)
  , P 1 :% 1 ~ P.Abs (N 1 % 1)
  , P 1 :% 2 ~ P.Abs (N 1 % 2)

  , P 1 :% 1 ~ P.Abs (P 1 % 1)
  , P 1 :% 2 ~ P.Abs (P 1 % 2)
  , P 1 :% 1 ~ P.Abs (P 1 % 1)
  , P 1 :% 2 ~ P.Abs (P 1 % 2)

  , P 1 :% 1 ~ P.Abs (N 1 % 1)
  , P 2 :% 1 ~ P.Abs (N 2 % 1)
  , P 1 :% 1 ~ P.Abs (N 1 % 1)
  , P 2 :% 1 ~ P.Abs (N 2 % 1)

  , P 1 :% 1 ~ P.Abs (P 1 % 1)
  , P 2 :% 1 ~ P.Abs (P 2 % 1)
  , P 1 :% 1 ~ P.Abs (P 1 % 1)
  , P 2 :% 1 ~ P.Abs (P 2 % 1)
  )


_testEq =  Dict
_testEq :: Dict
  ( (1%2 P.== 1%2) ~ 'True
  , (1%2 P.== 2%4) ~ 'True
  , (1%2 P.== 3%4) ~ 'False
  , (1%2 P./= 3%4) ~ 'True
  )

_testCmp =  Dict
_testCmp :: Dict
  ( 1%4 <= 1%4
  , 2%8 <= 1%4
  , 1%4 <= 1%2
  , 1%4 <= 2%4
  , 2%8 <= 1%2
  , 1%4 <  1%2
  , 1%4 <  2%4
  , 2%8 <  1%2
  , 2%8 <  2%4
  )

_testAdd =  Dict
_testAdd :: Dict
  ( (Z % 1) ~ (Z % 1) P.+ (Z % 1)
  , (Z % 1) ~ (N 5 % 1) P.+ (P 5 % 1)
  , (N 5 % 9) ~ (Z % 1) P.+ (N 5 % 9)
  , (N 9 % 2) ~ (N 3 % 2) P.+ (N 3 % 1)
  , (9 % 2) ~ (3 % 2) P.+ (3 % 1)
  , (N 11 % 3)~ (N 3 % 1) P.+ (N 2 % 3)
  )

_testMul =  Dict
_testMul :: Dict
  ( (0 % 1) ~ (Z % 1) P.* (Z % 1)
  , (N 25 % 1) ~ (N 5 % 1) P.* (5 % 1)
  , (N 1 % 1) ~ (N 5 % 1) P.* (1 % 5)
  , (5 % 9) ~ (N 1 % 1) P.* (N 5 % 9)
  , (9 % 1) ~ (N 3 % 1) P.* (N 3 % 1)
  , (2 % 1) ~ (N 3 % 1) P.* (N 2 % 3)
  , (1 % 1) ~ (P 3 % 2) P.* (P 2 % 3)
  )

_testRecip =  Dict
_testRecip :: Dict
  ( (1 % 1) ~ K.Recip (1 % 1)
  , (1 % 2) ~ K.Recip (2 % 1)
  , (4 % 3) ~ K.Recip (3 % 4)
  , (N 1 % 1) ~ K.Recip (N 1 % 1)
  , (N 1 % 2) ~ K.Recip (N 2 % 1)
  , (N 4 % 3) ~ K.Recip (N 3 % 4)
  )

_testDiv =  Dict
_testDiv :: Dict
  ( P 1 ~ K.Div 'K.RoundDown (3 % 2)
  , P 2 ~ K.Div 'K.RoundUp (3 % 2)
  , P 1 ~ K.Div 'K.RoundZero (3 % 2)
  , P 2 ~ K.Div 'K.RoundAway (3 % 2)
  , P 1 ~ K.Div 'K.RoundHalfDown (3 % 2)
  , P 2 ~ K.Div 'K.RoundHalfUp (3 % 2)
  , P 1 ~ K.Div 'K.RoundHalfZero (3 % 2)
  , P 2 ~ K.Div 'K.RoundHalfAway (3 % 2)
  , P 2 ~ K.Div 'K.RoundHalfEven (3 % 2)
  , P 1 ~ K.Div 'K.RoundHalfOdd (3 % 2)

  , N 2 ~ K.Div 'K.RoundDown (N 3 % 2)
  , N 1 ~ K.Div 'K.RoundUp (N 3 % 2)
  , N 1 ~ K.Div 'K.RoundZero (N 3 % 2)
  , N 2 ~ K.Div 'K.RoundAway (N 3 % 2)
  , N 2 ~ K.Div 'K.RoundHalfDown (N 3 % 2)
  , N 1 ~ K.Div 'K.RoundHalfUp (N 3 % 2)
  , N 1 ~ K.Div 'K.RoundHalfZero (N 3 % 2)
  , N 2 ~ K.Div 'K.RoundHalfAway (N 3 % 2)
  , N 2 ~ K.Div 'K.RoundHalfEven (N 3 % 2)
  , N 1 ~ K.Div 'K.RoundHalfOdd (N 3 % 2)

  , Z ~ K.Div 'K.RoundDown (3 % 4)
  , P 1 ~ K.Div 'K.RoundUp (3 % 4)
  , Z ~ K.Div 'K.RoundZero (3 % 4)
  , P 1 ~ K.Div 'K.RoundAway (3 % 4)
  , P 1 ~ K.Div 'K.RoundHalfDown (3 % 4)
  , P 1 ~ K.Div 'K.RoundHalfUp (3 % 4)
  , P 1 ~ K.Div 'K.RoundHalfZero (3 % 4)
  , P 1 ~ K.Div 'K.RoundHalfAway (3 % 4)
  , P 1 ~ K.Div 'K.RoundHalfEven (3 % 4)
  , P 1 ~ K.Div 'K.RoundHalfOdd (3 % 4)

  , N 1 ~ K.Div 'K.RoundDown (N 3 % 4)
  , Z ~ K.Div 'K.RoundUp (N 3 % 4)
  , Z ~ K.Div 'K.RoundZero (N 3 % 4)
  , N 1 ~ K.Div 'K.RoundAway (N 3 % 4)
  , N 1 ~ K.Div 'K.RoundHalfDown (N 3 % 4)
  , N 1 ~ K.Div 'K.RoundHalfUp (N 3 % 4)
  , N 1 ~ K.Div 'K.RoundHalfZero (N 3 % 4)
  , N 1 ~ K.Div 'K.RoundHalfAway (N 3 % 4)
  , N 1 ~ K.Div 'K.RoundHalfEven (N 3 % 4)
  , N 1 ~ K.Div 'K.RoundHalfOdd (N 3 % 4)
  )

_testRem =  Dict
_testRem :: Dict
  ( P 1 % 2 ~ K.Rem 'K.RoundDown (3 % 2)
  , N 1 % 2 ~ K.Rem 'K.RoundUp (3 % 2)
  , P 1 % 2 ~ K.Rem 'K.RoundZero (3 % 2)
  , N 1 % 2 ~ K.Rem 'K.RoundAway (3 % 2)
  , P 1 % 2 ~ K.Rem 'K.RoundHalfDown (3 % 2)
  , N 1 % 2 ~ K.Rem 'K.RoundHalfUp (3 % 2)
  , P 1 % 2 ~ K.Rem 'K.RoundHalfZero (3 % 2)
  , N 1 % 2 ~ K.Rem 'K.RoundHalfAway (3 % 2)
  , N 1 % 2 ~ K.Rem 'K.RoundHalfEven (3 % 2)
  , P 1 % 2 ~ K.Rem 'K.RoundHalfOdd (3 % 2)

  , P 1 % 2 ~ K.Rem 'K.RoundDown (N 3 % 2)
  , N 1 % 2 ~ K.Rem 'K.RoundUp (N 3 % 2)
  , N 1 % 2 ~ K.Rem 'K.RoundZero (N 3 % 2)
  , P 1 % 2 ~ K.Rem 'K.RoundAway (N 3 % 2)
  , P 1 % 2 ~ K.Rem 'K.RoundHalfDown (N 3 % 2)
  , N 1 % 2 ~ K.Rem 'K.RoundHalfUp (N 3 % 2)
  , N 1 % 2 ~ K.Rem 'K.RoundHalfZero (N 3 % 2)
  , P 1 % 2 ~ K.Rem 'K.RoundHalfAway (N 3 % 2)
  , P 1 % 2 ~ K.Rem 'K.RoundHalfEven (N 3 % 2)
  , N 1 % 2 ~ K.Rem 'K.RoundHalfOdd (N 3 % 2)

  , P 3 % 4 ~ K.Rem 'K.RoundDown (3 % 4)
  , N 1 % 4 ~ K.Rem 'K.RoundUp (3 % 4)
  , P 3 % 4 ~ K.Rem 'K.RoundZero (3 % 4)
  , N 1 % 4 ~ K.Rem 'K.RoundAway (3 % 4)
  , N 1 % 4 ~ K.Rem 'K.RoundHalfDown (3 % 4)
  , N 1 % 4 ~ K.Rem 'K.RoundHalfUp (3 % 4)
  , N 1 % 4 ~ K.Rem 'K.RoundHalfZero (3 % 4)
  , N 1 % 4 ~ K.Rem 'K.RoundHalfAway (3 % 4)
  , N 1 % 4 ~ K.Rem 'K.RoundHalfEven (3 % 4)
  , N 1 % 4 ~ K.Rem 'K.RoundHalfOdd (3 % 4)

  , P 1 % 4 ~ K.Rem 'K.RoundDown (N 3 % 4)
  , N 3 % 4 ~ K.Rem 'K.RoundUp (N 3 % 4)
  , N 3 % 4 ~ K.Rem 'K.RoundZero (N 3 % 4)
  , P 1 % 4 ~ K.Rem 'K.RoundAway (N 3 % 4)
  , P 1 % 4 ~ K.Rem 'K.RoundHalfDown (N 3 % 4)
  , P 1 % 4 ~ K.Rem 'K.RoundHalfUp (N 3 % 4)
  , P 1 % 4 ~ K.Rem 'K.RoundHalfZero (N 3 % 4)
  , P 1 % 4 ~ K.Rem 'K.RoundHalfAway (N 3 % 4)
  , P 1 % 4 ~ K.Rem 'K.RoundHalfEven (N 3 % 4)
  , P 1 % 4 ~ K.Rem 'K.RoundHalfOdd (N 3 % 4)
  )

_testDivRem =  Dict
_testDivRem :: Dict
  ( '(P 1, P 1 % 2) ~ K.DivRem 'K.RoundDown (3 % 2)
  , '(P 2, N 1 % 2) ~ K.DivRem 'K.RoundUp (3 % 2)
  , '(P 1, P 1 % 2) ~ K.DivRem 'K.RoundZero (3 % 2)
  , '(P 2, N 1 % 2) ~ K.DivRem 'K.RoundAway (3 % 2)
  , '(P 1, P 1 % 2) ~ K.DivRem 'K.RoundHalfDown (3 % 2)
  , '(P 2, N 1 % 2) ~ K.DivRem 'K.RoundHalfUp (3 % 2)
  , '(P 1, P 1 % 2) ~ K.DivRem 'K.RoundHalfZero (3 % 2)
  , '(P 2, N 1 % 2) ~ K.DivRem 'K.RoundHalfAway (3 % 2)
  , '(P 2, N 1 % 2) ~ K.DivRem 'K.RoundHalfEven (3 % 2)
  , '(P 1, P 1 % 2) ~ K.DivRem 'K.RoundHalfOdd (3 % 2)

  , '(N 2, P 1 % 2) ~ K.DivRem 'K.RoundDown (N 3 % 2)
  , '(N 1, N 1 % 2) ~ K.DivRem 'K.RoundUp (N 3 % 2)
  , '(N 1, N 1 % 2) ~ K.DivRem 'K.RoundZero (N 3 % 2)
  , '(N 2, P 1 % 2) ~ K.DivRem 'K.RoundAway (N 3 % 2)
  , '(N 2, P 1 % 2) ~ K.DivRem 'K.RoundHalfDown (N 3 % 2)
  , '(N 1, N 1 % 2) ~ K.DivRem 'K.RoundHalfUp (N 3 % 2)
  , '(N 1, N 1 % 2) ~ K.DivRem 'K.RoundHalfZero (N 3 % 2)
  , '(N 2, P 1 % 2) ~ K.DivRem 'K.RoundHalfAway (N 3 % 2)
  , '(N 2, P 1 % 2) ~ K.DivRem 'K.RoundHalfEven (N 3 % 2)
  , '(N 1, N 1 % 2) ~ K.DivRem 'K.RoundHalfOdd (N 3 % 2)

  , '(Z, P 3 % 4) ~ K.DivRem 'K.RoundDown (3 % 4)
  , '(P 1, N 1 % 4) ~ K.DivRem 'K.RoundUp (3 % 4)
  , '(Z, P 3 % 4) ~ K.DivRem 'K.RoundZero (3 % 4)
  , '(P 1, N 1 % 4) ~ K.DivRem 'K.RoundAway (3 % 4)
  , '(P 1, N 1 % 4) ~ K.DivRem 'K.RoundHalfDown (3 % 4)
  , '(P 1, N 1 % 4) ~ K.DivRem 'K.RoundHalfUp (3 % 4)
  , '(P 1, N 1 % 4) ~ K.DivRem 'K.RoundHalfZero (3 % 4)
  , '(P 1, N 1 % 4) ~ K.DivRem 'K.RoundHalfAway (3 % 4)
  , '(P 1, N 1 % 4) ~ K.DivRem 'K.RoundHalfEven (3 % 4)
  , '(P 1, N 1 % 4) ~ K.DivRem 'K.RoundHalfOdd (3 % 4)

  , '(N 1, P 1 % 4) ~ K.DivRem 'K.RoundDown (N 3 % 4)
  , '(Z, N 3 % 4) ~ K.DivRem 'K.RoundUp (N 3 % 4)
  , '(Z, N 3 % 4) ~ K.DivRem 'K.RoundZero (N 3 % 4)
  , '(N 1, P 1 % 4) ~ K.DivRem 'K.RoundAway (N 3 % 4)
  , '(N 1, P 1 % 4) ~ K.DivRem 'K.RoundHalfDown (N 3 % 4)
  , '(N 1, P 1 % 4) ~ K.DivRem 'K.RoundHalfUp (N 3 % 4)
  , '(N 1, P 1 % 4) ~ K.DivRem 'K.RoundHalfZero (N 3 % 4)
  , '(N 1, P 1 % 4) ~ K.DivRem 'K.RoundHalfAway (N 3 % 4)
  , '(N 1, P 1 % 4) ~ K.DivRem 'K.RoundHalfEven (N 3 % 4)
  , '(N 1, P 1 % 4) ~ K.DivRem 'K.RoundHalfOdd (N 3 % 4)
  )

_testTerminating =  Dict
_testTerminating :: Dict
  ( K.Terminating (Z%1)
  , K.Terminating (1%1)
  , K.Terminating (N 2%1)
  , K.Terminating (1%2)
  , K.Terminating (N 1%4)
  , K.Terminating (1%5)
  , K.Terminating (N 1%10)
  , K.Terminating (1%20)
  , K.Terminating (N 1%50)
  , K.Terminating (1%10000000)

  , K.Terminating (N 3%1)
  , K.Terminating (3%1)
  , K.Terminating (N 3%2)
  , K.Terminating (3%3)
  , K.Terminating (N 3%4)
  , K.Terminating (3%5)
  , K.Terminating (N 3%6)
  , K.Terminating (3%10)
  , K.Terminating (N 3%20)
  , K.Terminating (3%50)
  , K.Terminating (N 3%10000000)

  , K.NonTerminating (1%3)
  , K.NonTerminating (N 1%12)
  , K.NonTerminating (1%15)
  , K.NonTerminating (N 2%3)
  , K.NonTerminating (75%7)
  , K.NonTerminating (N 8%3)
  )

_testIsTerminating =  Dict
_testIsTerminating :: Dict
  ( 'True ~ K.IsTerminating (Z%1)
  , 'True ~ K.IsTerminating (1%1)
  , 'True ~ K.IsTerminating (N 2%1)
  , 'True ~ K.IsTerminating (1%2)
  , 'True ~ K.IsTerminating (N 1%4)
  , 'True ~ K.IsTerminating (1%5)
  , 'True ~ K.IsTerminating (N 1%10)
  , 'True ~ K.IsTerminating (1%20)
  , 'True ~ K.IsTerminating (N 1%50)
  , 'True ~ K.IsTerminating (1%10000000)

  , 'True ~ K.IsTerminating (N 3%1)
  , 'True ~ K.IsTerminating (3%1)
  , 'True ~ K.IsTerminating (N 3%2)
  , 'True ~ K.IsTerminating (3%3)
  , 'True ~ K.IsTerminating (N 3%4)
  , 'True ~ K.IsTerminating (3%5)
  , 'True ~ K.IsTerminating (N 3%6)
  , 'True ~ K.IsTerminating (3%10)
  , 'True ~ K.IsTerminating (N 3%20)
  , 'True ~ K.IsTerminating (3%50)
  , 'True ~ K.IsTerminating (N 3%10000000)

  , 'False ~ K.IsTerminating (1%3)
  , 'False ~ K.IsTerminating (N 1%12)
  , 'False ~ K.IsTerminating (1%15)
  , 'False ~ K.IsTerminating (N 2%3)
  , 'False ~ K.IsTerminating (75%7)
  , 'False ~ K.IsTerminating (N 8%3)
  )

--------------------------------------------------------------------------------

assert
  :: String  -- ^ Test name
  -> Bool    -- ^ Successful is true
  -> IO Bool -- ^ Return the same 'Bool' given as input.
assert n x = do
  putStrLn ((if x then "[OK] " else "[FAIL] ") <> n)
  pure x

testsMain :: [IO Bool] -> IO a
testsMain xs = do
  res <- sequence xs
  let (oks, bads) = List.partition id res
  putStrLn ("[TOTAL] OK: " <> show (length oks) <>
            ". FAIL: " <> show (length bads) <> ".")
  case bads of
    [] -> exitSuccess
    _  -> exitFailure

rats :: P.Integer -> [P.Rational]
rats i = do n <- [negate i .. i]
            d <- [negate i .. i]
            guard (d /= 0)
            pure (n P.% d)

main :: IO ()
main = testsMain $
  [ assert "sNum" $ do
    flip all (rats 4) $ \a@(n P.:% _) ->
      case K.someRationalVal a of
        K.SomeRational (_ :: Proxy a) ->
          n == fromSing (K.sNum (sing @a))

  , assert "sDen" $ do
    flip all (rats 4) $ \a@(_ P.:% d) ->
      case K.someRationalVal a of
        K.SomeRational (_ :: Proxy a) ->
          d == toInteger (fromSing (K.sDen (sing @a)))

  , assert "rationalVal . someRationalVal == id" $
    flip all (rats 4) $ \a ->
      case K.someRationalVal a of
        K.SomeRational pa ->
          a == K.rationalVal pa

  , assert "sameRationalVal a a" $
    flip all (rats 4) $ \a ->
      case K.someRationalVal a of
        K.SomeRational pa ->
          isJust (K.sameRational pa pa)

  , assert "sameRationalVal a a'" $
    flip all (rats 4) $ \a ->
      case (K.someRationalVal a, K.someRationalVal a) of
        (K.SomeRational pa1, K.SomeRational pa2) ->
          isJust (K.sameRational pa1 pa2)

  , assert "sameRationalVal a b" $
    flip all (liftA2 (,) (rats 4) (rats 4)) $ \(a, b) ->
      case (K.someRationalVal a, K.someRationalVal b) of
        (K.SomeRational pa, K.SomeRational pb)
          | a == b    -> isJust    (K.sameRational pa pb)
          | otherwise -> isNothing (K.sameRational pa pb)

  , assert "demote @(Z :% 1)" $ demote @(Z :% 1) == (0 :: P.Rational)
  , assert "demote @(P 1 :% 1)" $ demote @(P 1 :% 1) == (1 :: P.Rational)
  , assert "demote @(N 1 :% 1)" $ demote @(N 1 :% 1) == ((-1) :: P.Rational)

  , assert "show (SRational @(Z :% 1))" $
    show (K.SRational @(Z :% 1)) == "SRational @(Z :% 1)"
  , assert "show (SRational @(P 1 :% 1))" $
    show (K.SRational @(P 1 :% 1)) == "SRational @(P 1 :% 1)"
  , assert "show (SRational @(N 1 :% 1))" $
    show (K.SRational @(N 1 :% 1)) == "SRational @(N 1 :% 1)"

  , assert "Eq SomeRational" $
    and [ K.someRationalVal 0 == K.someRationalVal 0
        , K.someRationalVal 1 == K.someRationalVal 1
        , K.someRationalVal 2 == K.someRationalVal 2
        , K.someRationalVal (1 P.% 2) == K.someRationalVal (1 P.% 2)
        ]

  , assert "Ord SomeRational" $
    flip all (liftA2 (,) (rats 4) (rats 4)) $ \(a, b) ->
      compare (K.someRationalVal a) (K.someRationalVal b)
        == compare a b

  , assert "Show SomeRational" $
    flip all (rats 4) $ \a ->
      show (K.someRationalVal a) == show a

  , assert "Read SomeRational" $
    flip all (rats 4) $ \a ->
    fmap K.someRationalVal (readMaybe (show a))
       == Just (K.someRationalVal a)

  , assert "TestEquality 0%1 0%1" $
     isJust (testEquality (K.SRational @(Z :% 1)) (K.SRational @(Z :% 1)))
  , assert "TestEquality +1%2 +1%2" $
     isJust (testEquality (K.SRational @(P 1 :% 2)) (K.SRational @(P 1 :% 2)))
  , assert "TestEquality -1%2 -1%2" $
     isJust (testEquality (K.SRational @(N 1 :% 2)) (K.SRational @(N 1 :% 2)))
  , assert "TestEquality 0%1 +1%1" $
     isNothing (testEquality (K.SRational @(Z :% 1)) (K.SRational @(P 1 :% 1)))
  , assert "TestEquality 0%1 -1%1" $
     isNothing (testEquality (K.SRational @(Z :% 1)) (K.SRational @(N 1 :% 1)))
  , assert "TestEquality 0%1 +1%1" $
     isNothing (testEquality (K.SRational @(Z :% 1)) (K.SRational @(P 1 :% 1)))
  , assert "TestEquality +1%1 0%1" $
     isNothing (testEquality (K.SRational @(P 1 :% 1)) (K.SRational @(Z :% 1)))
  , assert "TestEquality -1%1 0%1" $
     isNothing (testEquality (K.SRational @(N 1 :% 1)) (K.SRational @(Z :% 1)))

  , assert "Show Rational 0" $
     "0 % 1" == show (K.fromSRational (K.SRational @(Z :% 1)))
  , assert "Show Rational +1" $
     "1 % 1" == show (K.fromSRational (K.SRational @(P 1 :% 1)))
  , assert "Show Rational -1" $
     "(-1) % 1" == show (K.fromSRational (K.SRational @(N 1 :% 1)))

  , assert "Show Rational 0" $
     isJust (testEquality (sing @"0 % 1") (P.sShow_ (K.SRational @(Z :% 1))))
  , assert "Show Rational 1" $
     isJust (testEquality (sing @"1 % 1") (P.sShow_ (K.SRational @(P 1 :% 1))))
  , assert "Show Rational -2" $
     isJust (testEquality (sing @"(-2) % 1") (P.sShow_ (K.SRational @(N 2 :% 1))))

  , assert "Show SRational 0" $
     "SRational @(Z :% 1)" == show (K.SRational @(Z :% 1))
  , assert "Show SRational +1" $
     "SRational @(P 1 :% 1)" == show (K.SRational @(P 1 :% 1))
  , assert "Show SRational -1" $
     "SRational @(N 1 :% 1)" == show (K.SRational @(N 1 :% 1))

  , assert "showLit Rational 0" $
     "Z :% 1" == K.showLit (K.fromSRational (K.SRational @(Z :% 1)))
  , assert "showLit Rational +1" $
     "P 1 :% 1" == K.showLit (K.fromSRational (K.SRational @(P 1 :% 1)))
  , assert "showLit Rational -1" $
     "N 1 :% 1" == K.showLit (K.fromSRational (K.SRational @(N 1 :% 1)))

  , assert "readPrecLit appPrec" $
    flip all (rats 4) $ \a ->
      [(a, "")] == readPrec_to_S K.readPrecLit appPrec (K.showsPrecLit appPrec a "")

  , assert "readPrecLit appPrec1" $
    flip all (rats 4) $ \a ->
      [(a, "")] == readPrec_to_S K.readPrecLit appPrec1 (K.showsPrecLit appPrec1 a "")

  , assert "sShowLit Rational 0" $
     isJust (testEquality (sing @"Z :% 1") (K.sShowLit (K.SRational @(Z :% 1))))
  , assert "sShowLit Rational 1" $
     isJust (testEquality (sing @"P 1 :% 1") (K.sShowLit (K.SRational @(P 1 :% 1))))
  , assert "sShowLit Rational -2" $
     isJust (testEquality (sing @"N 2 :% 1") (K.sShowLit (K.SRational @(N 2 :% 1))))

  , assert "sFromNatural 0" $
     isJust (testEquality (K.SRational @(Z :% 1)) (K.sFromNatural (sing @0)))
  , assert "sFromNatural 1" $
     isJust (testEquality (K.SRational @(P 1 :% 1)) (K.sFromNatural (sing @1)))
  , assert "sFromNatural 2" $
     isJust (testEquality (K.SRational @(P 2 :% 1)) (K.sFromNatural (sing @2)))

  , assert "fromNatural 0" $
      K.mkRational (0 :: P.Integer) (1 :: P.Integer) == Just (K.fromNatural 0)
  , assert "fromNatural 1" $
      K.mkRational (1 :: P.Integer) (1 :: P.Integer) == Just (K.fromNatural 1)
  , assert "fromNatural 2" $
      K.mkRational (2 :: P.Integer) (1 :: P.Integer) == Just (K.fromNatural 2)

  , assert "sFromInteger 0" $
     isJust (testEquality (K.SRational @(Z :% 1)) (K.sFromInteger (sing @Z)))
  , assert "sFromInteger 1" $
     isJust (testEquality (K.SRational @(P 1 :% 1)) (K.sFromInteger (sing @(P 1))))
  , assert "sFromInteger 2" $
     isJust (testEquality (K.SRational @(P 2 :% 1)) (K.sFromInteger (sing @(P 2))))
  , assert "sFromInteger -1" $
     isJust (testEquality (K.SRational @(N 1 :% 1)) (K.sFromInteger (sing @(N 1))))
  , assert "sFromInteger -2" $
     isJust (testEquality (K.SRational @(N 2 :% 1)) (K.sFromInteger (sing @(N 2))))

  , assert "fromInteger 0" $
      K.mkRational (0 :: P.Integer) (1 :: P.Integer) == Just (K.fromInteger 0)
  , assert "fromInteger 1" $
      K.mkRational (1 :: P.Integer) (1 :: P.Integer) == Just (K.fromInteger 1)
  , assert "fromInteger 2" $
      K.mkRational (2 :: P.Integer) (1 :: P.Integer) == Just (K.fromInteger 2)
  , assert "fromInteger -1" $
      K.mkRational ((-1) :: P.Integer) (1 :: P.Integer) == Just (K.fromInteger (-1))
  , assert "fromInteger -2" $
      K.mkRational ((-2) :: P.Integer) (1 :: P.Integer) == Just (K.fromInteger (-2))

  , assert "mkRational Natural Natural" $
    flip all ((,) <$> [0..2] <*> [1, 2]) $ \(n, d) ->
      K.mkRational @Natural @Natural n d == Just (toInteger n P.% toInteger d)

  , assert "mkRational Natural Integer" $
    flip all ((,) <$> [0..2] <*> [(-2), (-1), 1, 2]) $ \(n, d) ->
      K.mkRational @Natural @KI.Integer n d == Just (toInteger n P.% d)

  , assert "mkRational Natural Rational" $
      flip all ((,) <$> [0..2] <*> filter (/= 0) (rats 4)) $ \(n, d) ->
        K.mkRational @Natural @K.Rational n d == Just (P.fromIntegral n P./ d)

  , assert "mkRational Integer Natural" $
    flip all ((,) <$> [(-2)..2] <*> [1, 2]) $ \(n, d) ->
      K.mkRational @KI.Integer @Natural n d == Just (n P.% toInteger d)

  , assert "mkRational Integer Integer" $
    flip all ((,) <$> [(-2)..2] <*> [(-2), (-1), 1, 2]) $ \(n, d) ->
      K.mkRational @KI.Integer @KI.Integer n d == Just (n P.% d)

  , assert "mkRational Integer Rational" $
    flip all ((,) <$> [(-2)..2] <*> filter (/= 0) (rats 4)) $ \(n, d) ->
      K.mkRational @KI.Integer @K.Rational n d == Just (P.fromInteger n P./ d)

  , assert "mkRational Rational Natural" $
    flip all ((,) <$> rats 4 <*> [1, 2]) $ \(n, d) ->
      K.mkRational @K.Rational @Natural n d == Just (n P./ K.fromNatural d)

  , assert "mkRational Rational Integer" $
    flip all ((,) <$> rats 4 <*> [(-2), (-1), 1, 2]) $ \(n, d) ->
      K.mkRational @K.Rational @KI.Integer n d == Just (n P./ K.fromInteger d)

  , assert "mkRational Rational Rational" $
    flip all ((,) <$> rats 4 <*> filter (/= 0) (rats 4)) $ \(n, d) ->
      K.mkRational @K.Rational @K.Rational n d == Just (n P./ d)

  , assert "sMkRational Natural Natural" $
    flip all ((,) <$> [0..2] <*> [1, 2]) $ \(n, d) ->
      K.mkRational @Natural @Natural n d == Just (toInteger n P.% toInteger d)

  , assert "mkRational Natural Natural 0" $
    flip all [0..2] $ \n ->
      isNothing $ K.mkRational @Natural @Natural n 0

  , assert "mkRational Natural Integer 0" $
    flip all [0..2] $ \n ->
      isNothing $ K.mkRational @Natural @KI.Integer n 0

  , assert "mkRational Natural Rational 0" $
    flip all [0..2]$ \n ->
      isNothing $ K.mkRational @Natural @K.Rational n 0

  , assert "mkRational Integer Natural 0" $
    flip all [(-2)..2] $ \n ->
      isNothing $ K.mkRational @KI.Integer @Natural n 0

  , assert "mkRational Integer Integer 0" $
    flip all [(-2)..2] $ \n ->
      isNothing $ K.mkRational @KI.Integer @KI.Integer n 0

  , assert "mkRational Integer Rational 0" $
    flip all [(-2)..2] $ \n ->
      isNothing $ K.mkRational @KI.Integer @K.Rational n 0

  , assert "mkRational Rational Natural 0" $
    flip all (rats 4) $ \n ->
      isNothing $ K.mkRational @K.Rational @Natural n 0

  , assert "mkRational Rational Integer 0" $
    flip all (rats 4) $ \n ->
      isNothing $ K.mkRational @K.Rational @KI.Integer n 0

  , assert "mkRational Rational Rational 0" $
    flip all (rats 4) $ \n ->
      isNothing $ K.mkRational @K.Rational @K.Rational n 0

  , assert "sRecip (Z :% 1)" $
      isNothing $ K.sRecip' (K.SRational @(Z :% 1))

  , assert "sRecip (P 2 :% 1)" $
      isJust $ K.sRecip' (K.SRational @(P 2 :% 1))

  ] <> testsDivRem <> testsTermination

testsDivRem :: [IO Bool]
testsDivRem = do
  a@(n P.:% d) <- rats 4
  r :: K.Round <- [minBound .. maxBound]
  let tname :: String -> ShowS
      tname t = showString t . showChar ' ' . shows r . showChar ' '
              . shows n . showChar ' ' . shows d
  [   assert (tname "divRem" "") $
         case K.divRem r a of
           (q, x) -> a == toRational q + x
    , assert (tname "divRem/div" "") $ fst (K.divRem r a) == K.div r a
    , assert (tname "divRem/rem" "") $ snd (K.divRem r a) == K.rem r a
    ]

testsTermination  :: [IO Bool]
testsTermination = concat
    [ do a <- ok
         pure $ assert ("termination(ok) (" <> show a <> ")") $
           K.withSomeSRational a $ \(sa :: K.SRational a) ->
             K.termination False True sa

    , do a <- no
         pure $ assert ("termination(no) (" <> show a <> ")") $
           K.withSomeSRational a $ \(sa :: K.SRational a) ->
             K.termination True False sa

    , do a <- ok
         pure $ assert ("SRationalTerminating (" <> show a <> ")") $
           K.withSomeSRational a $ \case
             K.SRationalTerminating -> True
             K.SRationalNonTerminating -> False

    , do a <- no
         pure $ assert ("SRationalNonTerminating (" <> show a <> ")") $
           K.withSomeSRational a $ \case
             K.SRationalTerminating -> False
             K.SRationalNonTerminating -> True

    ]
  where
   ok :: [P.Rational]
   ok = [ 0 P.% 1
        , -1 P.% 1
        , 2 P.% 1
        , -1 P.% 2
        , 1 P.% 4
        , -1 P.% 5
        , 1 P.% 10
        , -1 P.% 20
        , 1 P.% 50
        , -1 P.% 10000000
        , 3 P.% 1
        , -3 P.% 1
        , 3 P.% 2
        , -3 P.% 3
        , 3 P.% 4
        , -3 P.% 5
        , 3 P.% 6
        , -3 P.% 10
        , 3 P.% 20
        , -3 P.% 50
        , 3 P.% 10000000
        ]
   no :: [P.Rational]
   no = [ 1 P.% 3
        , -1 P.% 12
        , 1 P.% 15
        , -2 P.% 3
        , 75 P.% 7
        , -8 P.% 3
        ]


_testSlash_Nat0_Nat1 = Dict @((Z :% 1) ~ (0 % 1))
_testSlash_Nat0_Nat2 = Dict @((Z :% 1) ~ (0 % 2))
_testSlash_Nat0_Nat3 = Dict @((Z :% 1) ~ (0 % 3))
_testSlash_Nat0_Nat4 = Dict @((Z :% 1) ~ (0 % 4))
_testSlash_Nat0_IntN4 = Dict @((Z :% 1) ~ (0 % (N 4)))
_testSlash_Nat0_IntN3 = Dict @((Z :% 1) ~ (0 % (N 3)))
_testSlash_Nat0_IntN2 = Dict @((Z :% 1) ~ (0 % (N 2)))
_testSlash_Nat0_IntN1 = Dict @((Z :% 1) ~ (0 % (N 1)))
_testSlash_Nat0_IntP1 = Dict @((Z :% 1) ~ (0 % (P 1)))
_testSlash_Nat0_IntP2 = Dict @((Z :% 1) ~ (0 % (P 2)))
_testSlash_Nat0_IntP3 = Dict @((Z :% 1) ~ (0 % (P 3)))
_testSlash_Nat0_IntP4 = Dict @((Z :% 1) ~ (0 % (P 4)))
_testSlash_Nat0_Rat4N1 = Dict @((Z :% 1) ~ (0 % (N 4 :% 1)))
_testSlash_Nat0_Rat3N1 = Dict @((Z :% 1) ~ (0 % (N 3 :% 1)))
_testSlash_Nat0_Rat2N1 = Dict @((Z :% 1) ~ (0 % (N 2 :% 1)))
_testSlash_Nat0_Rat3N2 = Dict @((Z :% 1) ~ (0 % (N 3 :% 2)))
_testSlash_Nat0_Rat4N3 = Dict @((Z :% 1) ~ (0 % (N 4 :% 3)))
_testSlash_Nat0_Rat1N1 = Dict @((Z :% 1) ~ (0 % (N 1 :% 1)))
_testSlash_Nat0_Rat3N4 = Dict @((Z :% 1) ~ (0 % (N 3 :% 4)))
_testSlash_Nat0_Rat2N3 = Dict @((Z :% 1) ~ (0 % (N 2 :% 3)))
_testSlash_Nat0_Rat1N2 = Dict @((Z :% 1) ~ (0 % (N 1 :% 2)))
_testSlash_Nat0_Rat1N3 = Dict @((Z :% 1) ~ (0 % (N 1 :% 3)))
_testSlash_Nat0_Rat1N4 = Dict @((Z :% 1) ~ (0 % (N 1 :% 4)))
_testSlash_Nat0_Rat1P4 = Dict @((Z :% 1) ~ (0 % (P 1 :% 4)))
_testSlash_Nat0_Rat1P3 = Dict @((Z :% 1) ~ (0 % (P 1 :% 3)))
_testSlash_Nat0_Rat1P2 = Dict @((Z :% 1) ~ (0 % (P 1 :% 2)))
_testSlash_Nat0_Rat2P3 = Dict @((Z :% 1) ~ (0 % (P 2 :% 3)))
_testSlash_Nat0_Rat3P4 = Dict @((Z :% 1) ~ (0 % (P 3 :% 4)))
_testSlash_Nat0_Rat1P1 = Dict @((Z :% 1) ~ (0 % (P 1 :% 1)))
_testSlash_Nat0_Rat4P3 = Dict @((Z :% 1) ~ (0 % (P 4 :% 3)))
_testSlash_Nat0_Rat3P2 = Dict @((Z :% 1) ~ (0 % (P 3 :% 2)))
_testSlash_Nat0_Rat2P1 = Dict @((Z :% 1) ~ (0 % (P 2 :% 1)))
_testSlash_Nat0_Rat3P1 = Dict @((Z :% 1) ~ (0 % (P 3 :% 1)))
_testSlash_Nat0_Rat4P1 = Dict @((Z :% 1) ~ (0 % (P 4 :% 1)))
_testSlash_Nat1_Nat1 = Dict @((P 1 :% 1) ~ (1 % 1))
_testSlash_Nat1_Nat2 = Dict @((P 1 :% 2) ~ (1 % 2))
_testSlash_Nat1_Nat3 = Dict @((P 1 :% 3) ~ (1 % 3))
_testSlash_Nat1_Nat4 = Dict @((P 1 :% 4) ~ (1 % 4))
_testSlash_Nat1_IntN4 = Dict @((N 1 :% 4) ~ (1 % (N 4)))
_testSlash_Nat1_IntN3 = Dict @((N 1 :% 3) ~ (1 % (N 3)))
_testSlash_Nat1_IntN2 = Dict @((N 1 :% 2) ~ (1 % (N 2)))
_testSlash_Nat1_IntN1 = Dict @((N 1 :% 1) ~ (1 % (N 1)))
_testSlash_Nat1_IntP1 = Dict @((P 1 :% 1) ~ (1 % (P 1)))
_testSlash_Nat1_IntP2 = Dict @((P 1 :% 2) ~ (1 % (P 2)))
_testSlash_Nat1_IntP3 = Dict @((P 1 :% 3) ~ (1 % (P 3)))
_testSlash_Nat1_IntP4 = Dict @((P 1 :% 4) ~ (1 % (P 4)))
_testSlash_Nat1_Rat4N1 = Dict @((N 1 :% 4) ~ (1 % (N 4 :% 1)))
_testSlash_Nat1_Rat3N1 = Dict @((N 1 :% 3) ~ (1 % (N 3 :% 1)))
_testSlash_Nat1_Rat2N1 = Dict @((N 1 :% 2) ~ (1 % (N 2 :% 1)))
_testSlash_Nat1_Rat3N2 = Dict @((N 2 :% 3) ~ (1 % (N 3 :% 2)))
_testSlash_Nat1_Rat4N3 = Dict @((N 3 :% 4) ~ (1 % (N 4 :% 3)))
_testSlash_Nat1_Rat1N1 = Dict @((N 1 :% 1) ~ (1 % (N 1 :% 1)))
_testSlash_Nat1_Rat3N4 = Dict @((N 4 :% 3) ~ (1 % (N 3 :% 4)))
_testSlash_Nat1_Rat2N3 = Dict @((N 3 :% 2) ~ (1 % (N 2 :% 3)))
_testSlash_Nat1_Rat1N2 = Dict @((N 2 :% 1) ~ (1 % (N 1 :% 2)))
_testSlash_Nat1_Rat1N3 = Dict @((N 3 :% 1) ~ (1 % (N 1 :% 3)))
_testSlash_Nat1_Rat1N4 = Dict @((N 4 :% 1) ~ (1 % (N 1 :% 4)))
_testSlash_Nat1_Rat1P4 = Dict @((P 4 :% 1) ~ (1 % (P 1 :% 4)))
_testSlash_Nat1_Rat1P3 = Dict @((P 3 :% 1) ~ (1 % (P 1 :% 3)))
_testSlash_Nat1_Rat1P2 = Dict @((P 2 :% 1) ~ (1 % (P 1 :% 2)))
_testSlash_Nat1_Rat2P3 = Dict @((P 3 :% 2) ~ (1 % (P 2 :% 3)))
_testSlash_Nat1_Rat3P4 = Dict @((P 4 :% 3) ~ (1 % (P 3 :% 4)))
_testSlash_Nat1_Rat1P1 = Dict @((P 1 :% 1) ~ (1 % (P 1 :% 1)))
_testSlash_Nat1_Rat4P3 = Dict @((P 3 :% 4) ~ (1 % (P 4 :% 3)))
_testSlash_Nat1_Rat3P2 = Dict @((P 2 :% 3) ~ (1 % (P 3 :% 2)))
_testSlash_Nat1_Rat2P1 = Dict @((P 1 :% 2) ~ (1 % (P 2 :% 1)))
_testSlash_Nat1_Rat3P1 = Dict @((P 1 :% 3) ~ (1 % (P 3 :% 1)))
_testSlash_Nat1_Rat4P1 = Dict @((P 1 :% 4) ~ (1 % (P 4 :% 1)))
_testSlash_Nat2_Nat1 = Dict @((P 2 :% 1) ~ (2 % 1))
_testSlash_Nat2_Nat2 = Dict @((P 1 :% 1) ~ (2 % 2))
_testSlash_Nat2_Nat3 = Dict @((P 2 :% 3) ~ (2 % 3))
_testSlash_Nat2_Nat4 = Dict @((P 1 :% 2) ~ (2 % 4))
_testSlash_Nat2_IntN4 = Dict @((N 1 :% 2) ~ (2 % (N 4)))
_testSlash_Nat2_IntN3 = Dict @((N 2 :% 3) ~ (2 % (N 3)))
_testSlash_Nat2_IntN2 = Dict @((N 1 :% 1) ~ (2 % (N 2)))
_testSlash_Nat2_IntN1 = Dict @((N 2 :% 1) ~ (2 % (N 1)))
_testSlash_Nat2_IntP1 = Dict @((P 2 :% 1) ~ (2 % (P 1)))
_testSlash_Nat2_IntP2 = Dict @((P 1 :% 1) ~ (2 % (P 2)))
_testSlash_Nat2_IntP3 = Dict @((P 2 :% 3) ~ (2 % (P 3)))
_testSlash_Nat2_IntP4 = Dict @((P 1 :% 2) ~ (2 % (P 4)))
_testSlash_Nat2_Rat4N1 = Dict @((N 1 :% 2) ~ (2 % (N 4 :% 1)))
_testSlash_Nat2_Rat3N1 = Dict @((N 2 :% 3) ~ (2 % (N 3 :% 1)))
_testSlash_Nat2_Rat2N1 = Dict @((N 1 :% 1) ~ (2 % (N 2 :% 1)))
_testSlash_Nat2_Rat3N2 = Dict @((N 4 :% 3) ~ (2 % (N 3 :% 2)))
_testSlash_Nat2_Rat4N3 = Dict @((N 3 :% 2) ~ (2 % (N 4 :% 3)))
_testSlash_Nat2_Rat1N1 = Dict @((N 2 :% 1) ~ (2 % (N 1 :% 1)))
_testSlash_Nat2_Rat3N4 = Dict @((N 8 :% 3) ~ (2 % (N 3 :% 4)))
_testSlash_Nat2_Rat2N3 = Dict @((N 3 :% 1) ~ (2 % (N 2 :% 3)))
_testSlash_Nat2_Rat1N2 = Dict @((N 4 :% 1) ~ (2 % (N 1 :% 2)))
_testSlash_Nat2_Rat1N3 = Dict @((N 6 :% 1) ~ (2 % (N 1 :% 3)))
_testSlash_Nat2_Rat1N4 = Dict @((N 8 :% 1) ~ (2 % (N 1 :% 4)))
_testSlash_Nat2_Rat1P4 = Dict @((P 8 :% 1) ~ (2 % (P 1 :% 4)))
_testSlash_Nat2_Rat1P3 = Dict @((P 6 :% 1) ~ (2 % (P 1 :% 3)))
_testSlash_Nat2_Rat1P2 = Dict @((P 4 :% 1) ~ (2 % (P 1 :% 2)))
_testSlash_Nat2_Rat2P3 = Dict @((P 3 :% 1) ~ (2 % (P 2 :% 3)))
_testSlash_Nat2_Rat3P4 = Dict @((P 8 :% 3) ~ (2 % (P 3 :% 4)))
_testSlash_Nat2_Rat1P1 = Dict @((P 2 :% 1) ~ (2 % (P 1 :% 1)))
_testSlash_Nat2_Rat4P3 = Dict @((P 3 :% 2) ~ (2 % (P 4 :% 3)))
_testSlash_Nat2_Rat3P2 = Dict @((P 4 :% 3) ~ (2 % (P 3 :% 2)))
_testSlash_Nat2_Rat2P1 = Dict @((P 1 :% 1) ~ (2 % (P 2 :% 1)))
_testSlash_Nat2_Rat3P1 = Dict @((P 2 :% 3) ~ (2 % (P 3 :% 1)))
_testSlash_Nat2_Rat4P1 = Dict @((P 1 :% 2) ~ (2 % (P 4 :% 1)))
_testSlash_Nat3_Nat1 = Dict @((P 3 :% 1) ~ (3 % 1))
_testSlash_Nat3_Nat2 = Dict @((P 3 :% 2) ~ (3 % 2))
_testSlash_Nat3_Nat3 = Dict @((P 1 :% 1) ~ (3 % 3))
_testSlash_Nat3_Nat4 = Dict @((P 3 :% 4) ~ (3 % 4))
_testSlash_Nat3_IntN4 = Dict @((N 3 :% 4) ~ (3 % (N 4)))
_testSlash_Nat3_IntN3 = Dict @((N 1 :% 1) ~ (3 % (N 3)))
_testSlash_Nat3_IntN2 = Dict @((N 3 :% 2) ~ (3 % (N 2)))
_testSlash_Nat3_IntN1 = Dict @((N 3 :% 1) ~ (3 % (N 1)))
_testSlash_Nat3_IntP1 = Dict @((P 3 :% 1) ~ (3 % (P 1)))
_testSlash_Nat3_IntP2 = Dict @((P 3 :% 2) ~ (3 % (P 2)))
_testSlash_Nat3_IntP3 = Dict @((P 1 :% 1) ~ (3 % (P 3)))
_testSlash_Nat3_IntP4 = Dict @((P 3 :% 4) ~ (3 % (P 4)))
_testSlash_Nat3_Rat4N1 = Dict @((N 3 :% 4) ~ (3 % (N 4 :% 1)))
_testSlash_Nat3_Rat3N1 = Dict @((N 1 :% 1) ~ (3 % (N 3 :% 1)))
_testSlash_Nat3_Rat2N1 = Dict @((N 3 :% 2) ~ (3 % (N 2 :% 1)))
_testSlash_Nat3_Rat3N2 = Dict @((N 2 :% 1) ~ (3 % (N 3 :% 2)))
_testSlash_Nat3_Rat4N3 = Dict @((N 9 :% 4) ~ (3 % (N 4 :% 3)))
_testSlash_Nat3_Rat1N1 = Dict @((N 3 :% 1) ~ (3 % (N 1 :% 1)))
_testSlash_Nat3_Rat3N4 = Dict @((N 4 :% 1) ~ (3 % (N 3 :% 4)))
_testSlash_Nat3_Rat2N3 = Dict @((N 9 :% 2) ~ (3 % (N 2 :% 3)))
_testSlash_Nat3_Rat1N2 = Dict @((N 6 :% 1) ~ (3 % (N 1 :% 2)))
_testSlash_Nat3_Rat1N3 = Dict @((N 9 :% 1) ~ (3 % (N 1 :% 3)))
_testSlash_Nat3_Rat1N4 = Dict @((N 12 :% 1) ~ (3 % (N 1 :% 4)))
_testSlash_Nat3_Rat1P4 = Dict @((P 12 :% 1) ~ (3 % (P 1 :% 4)))
_testSlash_Nat3_Rat1P3 = Dict @((P 9 :% 1) ~ (3 % (P 1 :% 3)))
_testSlash_Nat3_Rat1P2 = Dict @((P 6 :% 1) ~ (3 % (P 1 :% 2)))
_testSlash_Nat3_Rat2P3 = Dict @((P 9 :% 2) ~ (3 % (P 2 :% 3)))
_testSlash_Nat3_Rat3P4 = Dict @((P 4 :% 1) ~ (3 % (P 3 :% 4)))
_testSlash_Nat3_Rat1P1 = Dict @((P 3 :% 1) ~ (3 % (P 1 :% 1)))
_testSlash_Nat3_Rat4P3 = Dict @((P 9 :% 4) ~ (3 % (P 4 :% 3)))
_testSlash_Nat3_Rat3P2 = Dict @((P 2 :% 1) ~ (3 % (P 3 :% 2)))
_testSlash_Nat3_Rat2P1 = Dict @((P 3 :% 2) ~ (3 % (P 2 :% 1)))
_testSlash_Nat3_Rat3P1 = Dict @((P 1 :% 1) ~ (3 % (P 3 :% 1)))
_testSlash_Nat3_Rat4P1 = Dict @((P 3 :% 4) ~ (3 % (P 4 :% 1)))
_testSlash_Nat4_Nat1 = Dict @((P 4 :% 1) ~ (4 % 1))
_testSlash_Nat4_Nat2 = Dict @((P 2 :% 1) ~ (4 % 2))
_testSlash_Nat4_Nat3 = Dict @((P 4 :% 3) ~ (4 % 3))
_testSlash_Nat4_Nat4 = Dict @((P 1 :% 1) ~ (4 % 4))
_testSlash_Nat4_IntN4 = Dict @((N 1 :% 1) ~ (4 % (N 4)))
_testSlash_Nat4_IntN3 = Dict @((N 4 :% 3) ~ (4 % (N 3)))
_testSlash_Nat4_IntN2 = Dict @((N 2 :% 1) ~ (4 % (N 2)))
_testSlash_Nat4_IntN1 = Dict @((N 4 :% 1) ~ (4 % (N 1)))
_testSlash_Nat4_IntP1 = Dict @((P 4 :% 1) ~ (4 % (P 1)))
_testSlash_Nat4_IntP2 = Dict @((P 2 :% 1) ~ (4 % (P 2)))
_testSlash_Nat4_IntP3 = Dict @((P 4 :% 3) ~ (4 % (P 3)))
_testSlash_Nat4_IntP4 = Dict @((P 1 :% 1) ~ (4 % (P 4)))
_testSlash_Nat4_Rat4N1 = Dict @((N 1 :% 1) ~ (4 % (N 4 :% 1)))
_testSlash_Nat4_Rat3N1 = Dict @((N 4 :% 3) ~ (4 % (N 3 :% 1)))
_testSlash_Nat4_Rat2N1 = Dict @((N 2 :% 1) ~ (4 % (N 2 :% 1)))
_testSlash_Nat4_Rat3N2 = Dict @((N 8 :% 3) ~ (4 % (N 3 :% 2)))
_testSlash_Nat4_Rat4N3 = Dict @((N 3 :% 1) ~ (4 % (N 4 :% 3)))
_testSlash_Nat4_Rat1N1 = Dict @((N 4 :% 1) ~ (4 % (N 1 :% 1)))
_testSlash_Nat4_Rat3N4 = Dict @((N 16 :% 3) ~ (4 % (N 3 :% 4)))
_testSlash_Nat4_Rat2N3 = Dict @((N 6 :% 1) ~ (4 % (N 2 :% 3)))
_testSlash_Nat4_Rat1N2 = Dict @((N 8 :% 1) ~ (4 % (N 1 :% 2)))
_testSlash_Nat4_Rat1N3 = Dict @((N 12 :% 1) ~ (4 % (N 1 :% 3)))
_testSlash_Nat4_Rat1N4 = Dict @((N 16 :% 1) ~ (4 % (N 1 :% 4)))
_testSlash_Nat4_Rat1P4 = Dict @((P 16 :% 1) ~ (4 % (P 1 :% 4)))
_testSlash_Nat4_Rat1P3 = Dict @((P 12 :% 1) ~ (4 % (P 1 :% 3)))
_testSlash_Nat4_Rat1P2 = Dict @((P 8 :% 1) ~ (4 % (P 1 :% 2)))
_testSlash_Nat4_Rat2P3 = Dict @((P 6 :% 1) ~ (4 % (P 2 :% 3)))
_testSlash_Nat4_Rat3P4 = Dict @((P 16 :% 3) ~ (4 % (P 3 :% 4)))
_testSlash_Nat4_Rat1P1 = Dict @((P 4 :% 1) ~ (4 % (P 1 :% 1)))
_testSlash_Nat4_Rat4P3 = Dict @((P 3 :% 1) ~ (4 % (P 4 :% 3)))
_testSlash_Nat4_Rat3P2 = Dict @((P 8 :% 3) ~ (4 % (P 3 :% 2)))
_testSlash_Nat4_Rat2P1 = Dict @((P 2 :% 1) ~ (4 % (P 2 :% 1)))
_testSlash_Nat4_Rat3P1 = Dict @((P 4 :% 3) ~ (4 % (P 3 :% 1)))
_testSlash_Nat4_Rat4P1 = Dict @((P 1 :% 1) ~ (4 % (P 4 :% 1)))
_testSlash_IntN4_Nat1 = Dict @((N 4 :% 1) ~ ((N 4) % 1))
_testSlash_IntN4_Nat2 = Dict @((N 2 :% 1) ~ ((N 4) % 2))
_testSlash_IntN4_Nat3 = Dict @((N 4 :% 3) ~ ((N 4) % 3))
_testSlash_IntN4_Nat4 = Dict @((N 1 :% 1) ~ ((N 4) % 4))
_testSlash_IntN4_IntN4 = Dict @((P 1 :% 1) ~ ((N 4) % (N 4)))
_testSlash_IntN4_IntN3 = Dict @((P 4 :% 3) ~ ((N 4) % (N 3)))
_testSlash_IntN4_IntN2 = Dict @((P 2 :% 1) ~ ((N 4) % (N 2)))
_testSlash_IntN4_IntN1 = Dict @((P 4 :% 1) ~ ((N 4) % (N 1)))
_testSlash_IntN4_IntP1 = Dict @((N 4 :% 1) ~ ((N 4) % (P 1)))
_testSlash_IntN4_IntP2 = Dict @((N 2 :% 1) ~ ((N 4) % (P 2)))
_testSlash_IntN4_IntP3 = Dict @((N 4 :% 3) ~ ((N 4) % (P 3)))
_testSlash_IntN4_IntP4 = Dict @((N 1 :% 1) ~ ((N 4) % (P 4)))
_testSlash_IntN4_Rat4N1 = Dict @((P 1 :% 1) ~ ((N 4) % (N 4 :% 1)))
_testSlash_IntN4_Rat3N1 = Dict @((P 4 :% 3) ~ ((N 4) % (N 3 :% 1)))
_testSlash_IntN4_Rat2N1 = Dict @((P 2 :% 1) ~ ((N 4) % (N 2 :% 1)))
_testSlash_IntN4_Rat3N2 = Dict @((P 8 :% 3) ~ ((N 4) % (N 3 :% 2)))
_testSlash_IntN4_Rat4N3 = Dict @((P 3 :% 1) ~ ((N 4) % (N 4 :% 3)))
_testSlash_IntN4_Rat1N1 = Dict @((P 4 :% 1) ~ ((N 4) % (N 1 :% 1)))
_testSlash_IntN4_Rat3N4 = Dict @((P 16 :% 3) ~ ((N 4) % (N 3 :% 4)))
_testSlash_IntN4_Rat2N3 = Dict @((P 6 :% 1) ~ ((N 4) % (N 2 :% 3)))
_testSlash_IntN4_Rat1N2 = Dict @((P 8 :% 1) ~ ((N 4) % (N 1 :% 2)))
_testSlash_IntN4_Rat1N3 = Dict @((P 12 :% 1) ~ ((N 4) % (N 1 :% 3)))
_testSlash_IntN4_Rat1N4 = Dict @((P 16 :% 1) ~ ((N 4) % (N 1 :% 4)))
_testSlash_IntN4_Rat1P4 = Dict @((N 16 :% 1) ~ ((N 4) % (P 1 :% 4)))
_testSlash_IntN4_Rat1P3 = Dict @((N 12 :% 1) ~ ((N 4) % (P 1 :% 3)))
_testSlash_IntN4_Rat1P2 = Dict @((N 8 :% 1) ~ ((N 4) % (P 1 :% 2)))
_testSlash_IntN4_Rat2P3 = Dict @((N 6 :% 1) ~ ((N 4) % (P 2 :% 3)))
_testSlash_IntN4_Rat3P4 = Dict @((N 16 :% 3) ~ ((N 4) % (P 3 :% 4)))
_testSlash_IntN4_Rat1P1 = Dict @((N 4 :% 1) ~ ((N 4) % (P 1 :% 1)))
_testSlash_IntN4_Rat4P3 = Dict @((N 3 :% 1) ~ ((N 4) % (P 4 :% 3)))
_testSlash_IntN4_Rat3P2 = Dict @((N 8 :% 3) ~ ((N 4) % (P 3 :% 2)))
_testSlash_IntN4_Rat2P1 = Dict @((N 2 :% 1) ~ ((N 4) % (P 2 :% 1)))
_testSlash_IntN4_Rat3P1 = Dict @((N 4 :% 3) ~ ((N 4) % (P 3 :% 1)))
_testSlash_IntN4_Rat4P1 = Dict @((N 1 :% 1) ~ ((N 4) % (P 4 :% 1)))
_testSlash_IntN3_Nat1 = Dict @((N 3 :% 1) ~ ((N 3) % 1))
_testSlash_IntN3_Nat2 = Dict @((N 3 :% 2) ~ ((N 3) % 2))
_testSlash_IntN3_Nat3 = Dict @((N 1 :% 1) ~ ((N 3) % 3))
_testSlash_IntN3_Nat4 = Dict @((N 3 :% 4) ~ ((N 3) % 4))
_testSlash_IntN3_IntN4 = Dict @((P 3 :% 4) ~ ((N 3) % (N 4)))
_testSlash_IntN3_IntN3 = Dict @((P 1 :% 1) ~ ((N 3) % (N 3)))
_testSlash_IntN3_IntN2 = Dict @((P 3 :% 2) ~ ((N 3) % (N 2)))
_testSlash_IntN3_IntN1 = Dict @((P 3 :% 1) ~ ((N 3) % (N 1)))
_testSlash_IntN3_IntP1 = Dict @((N 3 :% 1) ~ ((N 3) % (P 1)))
_testSlash_IntN3_IntP2 = Dict @((N 3 :% 2) ~ ((N 3) % (P 2)))
_testSlash_IntN3_IntP3 = Dict @((N 1 :% 1) ~ ((N 3) % (P 3)))
_testSlash_IntN3_IntP4 = Dict @((N 3 :% 4) ~ ((N 3) % (P 4)))
_testSlash_IntN3_Rat4N1 = Dict @((P 3 :% 4) ~ ((N 3) % (N 4 :% 1)))
_testSlash_IntN3_Rat3N1 = Dict @((P 1 :% 1) ~ ((N 3) % (N 3 :% 1)))
_testSlash_IntN3_Rat2N1 = Dict @((P 3 :% 2) ~ ((N 3) % (N 2 :% 1)))
_testSlash_IntN3_Rat3N2 = Dict @((P 2 :% 1) ~ ((N 3) % (N 3 :% 2)))
_testSlash_IntN3_Rat4N3 = Dict @((P 9 :% 4) ~ ((N 3) % (N 4 :% 3)))
_testSlash_IntN3_Rat1N1 = Dict @((P 3 :% 1) ~ ((N 3) % (N 1 :% 1)))
_testSlash_IntN3_Rat3N4 = Dict @((P 4 :% 1) ~ ((N 3) % (N 3 :% 4)))
_testSlash_IntN3_Rat2N3 = Dict @((P 9 :% 2) ~ ((N 3) % (N 2 :% 3)))
_testSlash_IntN3_Rat1N2 = Dict @((P 6 :% 1) ~ ((N 3) % (N 1 :% 2)))
_testSlash_IntN3_Rat1N3 = Dict @((P 9 :% 1) ~ ((N 3) % (N 1 :% 3)))
_testSlash_IntN3_Rat1N4 = Dict @((P 12 :% 1) ~ ((N 3) % (N 1 :% 4)))
_testSlash_IntN3_Rat1P4 = Dict @((N 12 :% 1) ~ ((N 3) % (P 1 :% 4)))
_testSlash_IntN3_Rat1P3 = Dict @((N 9 :% 1) ~ ((N 3) % (P 1 :% 3)))
_testSlash_IntN3_Rat1P2 = Dict @((N 6 :% 1) ~ ((N 3) % (P 1 :% 2)))
_testSlash_IntN3_Rat2P3 = Dict @((N 9 :% 2) ~ ((N 3) % (P 2 :% 3)))
_testSlash_IntN3_Rat3P4 = Dict @((N 4 :% 1) ~ ((N 3) % (P 3 :% 4)))
_testSlash_IntN3_Rat1P1 = Dict @((N 3 :% 1) ~ ((N 3) % (P 1 :% 1)))
_testSlash_IntN3_Rat4P3 = Dict @((N 9 :% 4) ~ ((N 3) % (P 4 :% 3)))
_testSlash_IntN3_Rat3P2 = Dict @((N 2 :% 1) ~ ((N 3) % (P 3 :% 2)))
_testSlash_IntN3_Rat2P1 = Dict @((N 3 :% 2) ~ ((N 3) % (P 2 :% 1)))
_testSlash_IntN3_Rat3P1 = Dict @((N 1 :% 1) ~ ((N 3) % (P 3 :% 1)))
_testSlash_IntN3_Rat4P1 = Dict @((N 3 :% 4) ~ ((N 3) % (P 4 :% 1)))
_testSlash_IntN2_Nat1 = Dict @((N 2 :% 1) ~ ((N 2) % 1))
_testSlash_IntN2_Nat2 = Dict @((N 1 :% 1) ~ ((N 2) % 2))
_testSlash_IntN2_Nat3 = Dict @((N 2 :% 3) ~ ((N 2) % 3))
_testSlash_IntN2_Nat4 = Dict @((N 1 :% 2) ~ ((N 2) % 4))
_testSlash_IntN2_IntN4 = Dict @((P 1 :% 2) ~ ((N 2) % (N 4)))
_testSlash_IntN2_IntN3 = Dict @((P 2 :% 3) ~ ((N 2) % (N 3)))
_testSlash_IntN2_IntN2 = Dict @((P 1 :% 1) ~ ((N 2) % (N 2)))
_testSlash_IntN2_IntN1 = Dict @((P 2 :% 1) ~ ((N 2) % (N 1)))
_testSlash_IntN2_IntP1 = Dict @((N 2 :% 1) ~ ((N 2) % (P 1)))
_testSlash_IntN2_IntP2 = Dict @((N 1 :% 1) ~ ((N 2) % (P 2)))
_testSlash_IntN2_IntP3 = Dict @((N 2 :% 3) ~ ((N 2) % (P 3)))
_testSlash_IntN2_IntP4 = Dict @((N 1 :% 2) ~ ((N 2) % (P 4)))
_testSlash_IntN2_Rat4N1 = Dict @((P 1 :% 2) ~ ((N 2) % (N 4 :% 1)))
_testSlash_IntN2_Rat3N1 = Dict @((P 2 :% 3) ~ ((N 2) % (N 3 :% 1)))
_testSlash_IntN2_Rat2N1 = Dict @((P 1 :% 1) ~ ((N 2) % (N 2 :% 1)))
_testSlash_IntN2_Rat3N2 = Dict @((P 4 :% 3) ~ ((N 2) % (N 3 :% 2)))
_testSlash_IntN2_Rat4N3 = Dict @((P 3 :% 2) ~ ((N 2) % (N 4 :% 3)))
_testSlash_IntN2_Rat1N1 = Dict @((P 2 :% 1) ~ ((N 2) % (N 1 :% 1)))
_testSlash_IntN2_Rat3N4 = Dict @((P 8 :% 3) ~ ((N 2) % (N 3 :% 4)))
_testSlash_IntN2_Rat2N3 = Dict @((P 3 :% 1) ~ ((N 2) % (N 2 :% 3)))
_testSlash_IntN2_Rat1N2 = Dict @((P 4 :% 1) ~ ((N 2) % (N 1 :% 2)))
_testSlash_IntN2_Rat1N3 = Dict @((P 6 :% 1) ~ ((N 2) % (N 1 :% 3)))
_testSlash_IntN2_Rat1N4 = Dict @((P 8 :% 1) ~ ((N 2) % (N 1 :% 4)))
_testSlash_IntN2_Rat1P4 = Dict @((N 8 :% 1) ~ ((N 2) % (P 1 :% 4)))
_testSlash_IntN2_Rat1P3 = Dict @((N 6 :% 1) ~ ((N 2) % (P 1 :% 3)))
_testSlash_IntN2_Rat1P2 = Dict @((N 4 :% 1) ~ ((N 2) % (P 1 :% 2)))
_testSlash_IntN2_Rat2P3 = Dict @((N 3 :% 1) ~ ((N 2) % (P 2 :% 3)))
_testSlash_IntN2_Rat3P4 = Dict @((N 8 :% 3) ~ ((N 2) % (P 3 :% 4)))
_testSlash_IntN2_Rat1P1 = Dict @((N 2 :% 1) ~ ((N 2) % (P 1 :% 1)))
_testSlash_IntN2_Rat4P3 = Dict @((N 3 :% 2) ~ ((N 2) % (P 4 :% 3)))
_testSlash_IntN2_Rat3P2 = Dict @((N 4 :% 3) ~ ((N 2) % (P 3 :% 2)))
_testSlash_IntN2_Rat2P1 = Dict @((N 1 :% 1) ~ ((N 2) % (P 2 :% 1)))
_testSlash_IntN2_Rat3P1 = Dict @((N 2 :% 3) ~ ((N 2) % (P 3 :% 1)))
_testSlash_IntN2_Rat4P1 = Dict @((N 1 :% 2) ~ ((N 2) % (P 4 :% 1)))
_testSlash_IntN1_Nat1 = Dict @((N 1 :% 1) ~ ((N 1) % 1))
_testSlash_IntN1_Nat2 = Dict @((N 1 :% 2) ~ ((N 1) % 2))
_testSlash_IntN1_Nat3 = Dict @((N 1 :% 3) ~ ((N 1) % 3))
_testSlash_IntN1_Nat4 = Dict @((N 1 :% 4) ~ ((N 1) % 4))
_testSlash_IntN1_IntN4 = Dict @((P 1 :% 4) ~ ((N 1) % (N 4)))
_testSlash_IntN1_IntN3 = Dict @((P 1 :% 3) ~ ((N 1) % (N 3)))
_testSlash_IntN1_IntN2 = Dict @((P 1 :% 2) ~ ((N 1) % (N 2)))
_testSlash_IntN1_IntN1 = Dict @((P 1 :% 1) ~ ((N 1) % (N 1)))
_testSlash_IntN1_IntP1 = Dict @((N 1 :% 1) ~ ((N 1) % (P 1)))
_testSlash_IntN1_IntP2 = Dict @((N 1 :% 2) ~ ((N 1) % (P 2)))
_testSlash_IntN1_IntP3 = Dict @((N 1 :% 3) ~ ((N 1) % (P 3)))
_testSlash_IntN1_IntP4 = Dict @((N 1 :% 4) ~ ((N 1) % (P 4)))
_testSlash_IntN1_Rat4N1 = Dict @((P 1 :% 4) ~ ((N 1) % (N 4 :% 1)))
_testSlash_IntN1_Rat3N1 = Dict @((P 1 :% 3) ~ ((N 1) % (N 3 :% 1)))
_testSlash_IntN1_Rat2N1 = Dict @((P 1 :% 2) ~ ((N 1) % (N 2 :% 1)))
_testSlash_IntN1_Rat3N2 = Dict @((P 2 :% 3) ~ ((N 1) % (N 3 :% 2)))
_testSlash_IntN1_Rat4N3 = Dict @((P 3 :% 4) ~ ((N 1) % (N 4 :% 3)))
_testSlash_IntN1_Rat1N1 = Dict @((P 1 :% 1) ~ ((N 1) % (N 1 :% 1)))
_testSlash_IntN1_Rat3N4 = Dict @((P 4 :% 3) ~ ((N 1) % (N 3 :% 4)))
_testSlash_IntN1_Rat2N3 = Dict @((P 3 :% 2) ~ ((N 1) % (N 2 :% 3)))
_testSlash_IntN1_Rat1N2 = Dict @((P 2 :% 1) ~ ((N 1) % (N 1 :% 2)))
_testSlash_IntN1_Rat1N3 = Dict @((P 3 :% 1) ~ ((N 1) % (N 1 :% 3)))
_testSlash_IntN1_Rat1N4 = Dict @((P 4 :% 1) ~ ((N 1) % (N 1 :% 4)))
_testSlash_IntN1_Rat1P4 = Dict @((N 4 :% 1) ~ ((N 1) % (P 1 :% 4)))
_testSlash_IntN1_Rat1P3 = Dict @((N 3 :% 1) ~ ((N 1) % (P 1 :% 3)))
_testSlash_IntN1_Rat1P2 = Dict @((N 2 :% 1) ~ ((N 1) % (P 1 :% 2)))
_testSlash_IntN1_Rat2P3 = Dict @((N 3 :% 2) ~ ((N 1) % (P 2 :% 3)))
_testSlash_IntN1_Rat3P4 = Dict @((N 4 :% 3) ~ ((N 1) % (P 3 :% 4)))
_testSlash_IntN1_Rat1P1 = Dict @((N 1 :% 1) ~ ((N 1) % (P 1 :% 1)))
_testSlash_IntN1_Rat4P3 = Dict @((N 3 :% 4) ~ ((N 1) % (P 4 :% 3)))
_testSlash_IntN1_Rat3P2 = Dict @((N 2 :% 3) ~ ((N 1) % (P 3 :% 2)))
_testSlash_IntN1_Rat2P1 = Dict @((N 1 :% 2) ~ ((N 1) % (P 2 :% 1)))
_testSlash_IntN1_Rat3P1 = Dict @((N 1 :% 3) ~ ((N 1) % (P 3 :% 1)))
_testSlash_IntN1_Rat4P1 = Dict @((N 1 :% 4) ~ ((N 1) % (P 4 :% 1)))
_testSlash_IntP0_Nat1 = Dict @((Z :% 1) ~ (Z % 1))
_testSlash_IntP0_Nat2 = Dict @((Z :% 1) ~ (Z % 2))
_testSlash_IntP0_Nat3 = Dict @((Z :% 1) ~ (Z % 3))
_testSlash_IntP0_Nat4 = Dict @((Z :% 1) ~ (Z % 4))
_testSlash_IntP0_IntN4 = Dict @((Z :% 1) ~ (Z % (N 4)))
_testSlash_IntP0_IntN3 = Dict @((Z :% 1) ~ (Z % (N 3)))
_testSlash_IntP0_IntN2 = Dict @((Z :% 1) ~ (Z % (N 2)))
_testSlash_IntP0_IntN1 = Dict @((Z :% 1) ~ (Z % (N 1)))
_testSlash_IntP0_IntP1 = Dict @((Z :% 1) ~ (Z % (P 1)))
_testSlash_IntP0_IntP2 = Dict @((Z :% 1) ~ (Z % (P 2)))
_testSlash_IntP0_IntP3 = Dict @((Z :% 1) ~ (Z % (P 3)))
_testSlash_IntP0_IntP4 = Dict @((Z :% 1) ~ (Z % (P 4)))
_testSlash_IntP0_Rat4N1 = Dict @((Z :% 1) ~ (Z % (N 4 :% 1)))
_testSlash_IntP0_Rat3N1 = Dict @((Z :% 1) ~ (Z % (N 3 :% 1)))
_testSlash_IntP0_Rat2N1 = Dict @((Z :% 1) ~ (Z % (N 2 :% 1)))
_testSlash_IntP0_Rat3N2 = Dict @((Z :% 1) ~ (Z % (N 3 :% 2)))
_testSlash_IntP0_Rat4N3 = Dict @((Z :% 1) ~ (Z % (N 4 :% 3)))
_testSlash_IntP0_Rat1N1 = Dict @((Z :% 1) ~ (Z % (N 1 :% 1)))
_testSlash_IntP0_Rat3N4 = Dict @((Z :% 1) ~ (Z % (N 3 :% 4)))
_testSlash_IntP0_Rat2N3 = Dict @((Z :% 1) ~ (Z % (N 2 :% 3)))
_testSlash_IntP0_Rat1N2 = Dict @((Z :% 1) ~ (Z % (N 1 :% 2)))
_testSlash_IntP0_Rat1N3 = Dict @((Z :% 1) ~ (Z % (N 1 :% 3)))
_testSlash_IntP0_Rat1N4 = Dict @((Z :% 1) ~ (Z % (N 1 :% 4)))
_testSlash_IntP0_Rat1P4 = Dict @((Z :% 1) ~ (Z % (P 1 :% 4)))
_testSlash_IntP0_Rat1P3 = Dict @((Z :% 1) ~ (Z % (P 1 :% 3)))
_testSlash_IntP0_Rat1P2 = Dict @((Z :% 1) ~ (Z % (P 1 :% 2)))
_testSlash_IntP0_Rat2P3 = Dict @((Z :% 1) ~ (Z % (P 2 :% 3)))
_testSlash_IntP0_Rat3P4 = Dict @((Z :% 1) ~ (Z % (P 3 :% 4)))
_testSlash_IntP0_Rat1P1 = Dict @((Z :% 1) ~ (Z % (P 1 :% 1)))
_testSlash_IntP0_Rat4P3 = Dict @((Z :% 1) ~ (Z % (P 4 :% 3)))
_testSlash_IntP0_Rat3P2 = Dict @((Z :% 1) ~ (Z % (P 3 :% 2)))
_testSlash_IntP0_Rat2P1 = Dict @((Z :% 1) ~ (Z % (P 2 :% 1)))
_testSlash_IntP0_Rat3P1 = Dict @((Z :% 1) ~ (Z % (P 3 :% 1)))
_testSlash_IntP0_Rat4P1 = Dict @((Z :% 1) ~ (Z % (P 4 :% 1)))
_testSlash_IntP1_Nat1 = Dict @((P 1 :% 1) ~ ((P 1) % 1))
_testSlash_IntP1_Nat2 = Dict @((P 1 :% 2) ~ ((P 1) % 2))
_testSlash_IntP1_Nat3 = Dict @((P 1 :% 3) ~ ((P 1) % 3))
_testSlash_IntP1_Nat4 = Dict @((P 1 :% 4) ~ ((P 1) % 4))
_testSlash_IntP1_IntN4 = Dict @((N 1 :% 4) ~ ((P 1) % (N 4)))
_testSlash_IntP1_IntN3 = Dict @((N 1 :% 3) ~ ((P 1) % (N 3)))
_testSlash_IntP1_IntN2 = Dict @((N 1 :% 2) ~ ((P 1) % (N 2)))
_testSlash_IntP1_IntN1 = Dict @((N 1 :% 1) ~ ((P 1) % (N 1)))
_testSlash_IntP1_IntP1 = Dict @((P 1 :% 1) ~ ((P 1) % (P 1)))
_testSlash_IntP1_IntP2 = Dict @((P 1 :% 2) ~ ((P 1) % (P 2)))
_testSlash_IntP1_IntP3 = Dict @((P 1 :% 3) ~ ((P 1) % (P 3)))
_testSlash_IntP1_IntP4 = Dict @((P 1 :% 4) ~ ((P 1) % (P 4)))
_testSlash_IntP1_Rat4N1 = Dict @((N 1 :% 4) ~ ((P 1) % (N 4 :% 1)))
_testSlash_IntP1_Rat3N1 = Dict @((N 1 :% 3) ~ ((P 1) % (N 3 :% 1)))
_testSlash_IntP1_Rat2N1 = Dict @((N 1 :% 2) ~ ((P 1) % (N 2 :% 1)))
_testSlash_IntP1_Rat3N2 = Dict @((N 2 :% 3) ~ ((P 1) % (N 3 :% 2)))
_testSlash_IntP1_Rat4N3 = Dict @((N 3 :% 4) ~ ((P 1) % (N 4 :% 3)))
_testSlash_IntP1_Rat1N1 = Dict @((N 1 :% 1) ~ ((P 1) % (N 1 :% 1)))
_testSlash_IntP1_Rat3N4 = Dict @((N 4 :% 3) ~ ((P 1) % (N 3 :% 4)))
_testSlash_IntP1_Rat2N3 = Dict @((N 3 :% 2) ~ ((P 1) % (N 2 :% 3)))
_testSlash_IntP1_Rat1N2 = Dict @((N 2 :% 1) ~ ((P 1) % (N 1 :% 2)))
_testSlash_IntP1_Rat1N3 = Dict @((N 3 :% 1) ~ ((P 1) % (N 1 :% 3)))
_testSlash_IntP1_Rat1N4 = Dict @((N 4 :% 1) ~ ((P 1) % (N 1 :% 4)))
_testSlash_IntP1_Rat1P4 = Dict @((P 4 :% 1) ~ ((P 1) % (P 1 :% 4)))
_testSlash_IntP1_Rat1P3 = Dict @((P 3 :% 1) ~ ((P 1) % (P 1 :% 3)))
_testSlash_IntP1_Rat1P2 = Dict @((P 2 :% 1) ~ ((P 1) % (P 1 :% 2)))
_testSlash_IntP1_Rat2P3 = Dict @((P 3 :% 2) ~ ((P 1) % (P 2 :% 3)))
_testSlash_IntP1_Rat3P4 = Dict @((P 4 :% 3) ~ ((P 1) % (P 3 :% 4)))
_testSlash_IntP1_Rat1P1 = Dict @((P 1 :% 1) ~ ((P 1) % (P 1 :% 1)))
_testSlash_IntP1_Rat4P3 = Dict @((P 3 :% 4) ~ ((P 1) % (P 4 :% 3)))
_testSlash_IntP1_Rat3P2 = Dict @((P 2 :% 3) ~ ((P 1) % (P 3 :% 2)))
_testSlash_IntP1_Rat2P1 = Dict @((P 1 :% 2) ~ ((P 1) % (P 2 :% 1)))
_testSlash_IntP1_Rat3P1 = Dict @((P 1 :% 3) ~ ((P 1) % (P 3 :% 1)))
_testSlash_IntP1_Rat4P1 = Dict @((P 1 :% 4) ~ ((P 1) % (P 4 :% 1)))
_testSlash_IntP2_Nat1 = Dict @((P 2 :% 1) ~ ((P 2) % 1))
_testSlash_IntP2_Nat2 = Dict @((P 1 :% 1) ~ ((P 2) % 2))
_testSlash_IntP2_Nat3 = Dict @((P 2 :% 3) ~ ((P 2) % 3))
_testSlash_IntP2_Nat4 = Dict @((P 1 :% 2) ~ ((P 2) % 4))
_testSlash_IntP2_IntN4 = Dict @((N 1 :% 2) ~ ((P 2) % (N 4)))
_testSlash_IntP2_IntN3 = Dict @((N 2 :% 3) ~ ((P 2) % (N 3)))
_testSlash_IntP2_IntN2 = Dict @((N 1 :% 1) ~ ((P 2) % (N 2)))
_testSlash_IntP2_IntN1 = Dict @((N 2 :% 1) ~ ((P 2) % (N 1)))
_testSlash_IntP2_IntP1 = Dict @((P 2 :% 1) ~ ((P 2) % (P 1)))
_testSlash_IntP2_IntP2 = Dict @((P 1 :% 1) ~ ((P 2) % (P 2)))
_testSlash_IntP2_IntP3 = Dict @((P 2 :% 3) ~ ((P 2) % (P 3)))
_testSlash_IntP2_IntP4 = Dict @((P 1 :% 2) ~ ((P 2) % (P 4)))
_testSlash_IntP2_Rat4N1 = Dict @((N 1 :% 2) ~ ((P 2) % (N 4 :% 1)))
_testSlash_IntP2_Rat3N1 = Dict @((N 2 :% 3) ~ ((P 2) % (N 3 :% 1)))
_testSlash_IntP2_Rat2N1 = Dict @((N 1 :% 1) ~ ((P 2) % (N 2 :% 1)))
_testSlash_IntP2_Rat3N2 = Dict @((N 4 :% 3) ~ ((P 2) % (N 3 :% 2)))
_testSlash_IntP2_Rat4N3 = Dict @((N 3 :% 2) ~ ((P 2) % (N 4 :% 3)))
_testSlash_IntP2_Rat1N1 = Dict @((N 2 :% 1) ~ ((P 2) % (N 1 :% 1)))
_testSlash_IntP2_Rat3N4 = Dict @((N 8 :% 3) ~ ((P 2) % (N 3 :% 4)))
_testSlash_IntP2_Rat2N3 = Dict @((N 3 :% 1) ~ ((P 2) % (N 2 :% 3)))
_testSlash_IntP2_Rat1N2 = Dict @((N 4 :% 1) ~ ((P 2) % (N 1 :% 2)))
_testSlash_IntP2_Rat1N3 = Dict @((N 6 :% 1) ~ ((P 2) % (N 1 :% 3)))
_testSlash_IntP2_Rat1N4 = Dict @((N 8 :% 1) ~ ((P 2) % (N 1 :% 4)))
_testSlash_IntP2_Rat1P4 = Dict @((P 8 :% 1) ~ ((P 2) % (P 1 :% 4)))
_testSlash_IntP2_Rat1P3 = Dict @((P 6 :% 1) ~ ((P 2) % (P 1 :% 3)))
_testSlash_IntP2_Rat1P2 = Dict @((P 4 :% 1) ~ ((P 2) % (P 1 :% 2)))
_testSlash_IntP2_Rat2P3 = Dict @((P 3 :% 1) ~ ((P 2) % (P 2 :% 3)))
_testSlash_IntP2_Rat3P4 = Dict @((P 8 :% 3) ~ ((P 2) % (P 3 :% 4)))
_testSlash_IntP2_Rat1P1 = Dict @((P 2 :% 1) ~ ((P 2) % (P 1 :% 1)))
_testSlash_IntP2_Rat4P3 = Dict @((P 3 :% 2) ~ ((P 2) % (P 4 :% 3)))
_testSlash_IntP2_Rat3P2 = Dict @((P 4 :% 3) ~ ((P 2) % (P 3 :% 2)))
_testSlash_IntP2_Rat2P1 = Dict @((P 1 :% 1) ~ ((P 2) % (P 2 :% 1)))
_testSlash_IntP2_Rat3P1 = Dict @((P 2 :% 3) ~ ((P 2) % (P 3 :% 1)))
_testSlash_IntP2_Rat4P1 = Dict @((P 1 :% 2) ~ ((P 2) % (P 4 :% 1)))
_testSlash_IntP3_Nat1 = Dict @((P 3 :% 1) ~ ((P 3) % 1))
_testSlash_IntP3_Nat2 = Dict @((P 3 :% 2) ~ ((P 3) % 2))
_testSlash_IntP3_Nat3 = Dict @((P 1 :% 1) ~ ((P 3) % 3))
_testSlash_IntP3_Nat4 = Dict @((P 3 :% 4) ~ ((P 3) % 4))
_testSlash_IntP3_IntN4 = Dict @((N 3 :% 4) ~ ((P 3) % (N 4)))
_testSlash_IntP3_IntN3 = Dict @((N 1 :% 1) ~ ((P 3) % (N 3)))
_testSlash_IntP3_IntN2 = Dict @((N 3 :% 2) ~ ((P 3) % (N 2)))
_testSlash_IntP3_IntN1 = Dict @((N 3 :% 1) ~ ((P 3) % (N 1)))
_testSlash_IntP3_IntP1 = Dict @((P 3 :% 1) ~ ((P 3) % (P 1)))
_testSlash_IntP3_IntP2 = Dict @((P 3 :% 2) ~ ((P 3) % (P 2)))
_testSlash_IntP3_IntP3 = Dict @((P 1 :% 1) ~ ((P 3) % (P 3)))
_testSlash_IntP3_IntP4 = Dict @((P 3 :% 4) ~ ((P 3) % (P 4)))
_testSlash_IntP3_Rat4N1 = Dict @((N 3 :% 4) ~ ((P 3) % (N 4 :% 1)))
_testSlash_IntP3_Rat3N1 = Dict @((N 1 :% 1) ~ ((P 3) % (N 3 :% 1)))
_testSlash_IntP3_Rat2N1 = Dict @((N 3 :% 2) ~ ((P 3) % (N 2 :% 1)))
_testSlash_IntP3_Rat3N2 = Dict @((N 2 :% 1) ~ ((P 3) % (N 3 :% 2)))
_testSlash_IntP3_Rat4N3 = Dict @((N 9 :% 4) ~ ((P 3) % (N 4 :% 3)))
_testSlash_IntP3_Rat1N1 = Dict @((N 3 :% 1) ~ ((P 3) % (N 1 :% 1)))
_testSlash_IntP3_Rat3N4 = Dict @((N 4 :% 1) ~ ((P 3) % (N 3 :% 4)))
_testSlash_IntP3_Rat2N3 = Dict @((N 9 :% 2) ~ ((P 3) % (N 2 :% 3)))
_testSlash_IntP3_Rat1N2 = Dict @((N 6 :% 1) ~ ((P 3) % (N 1 :% 2)))
_testSlash_IntP3_Rat1N3 = Dict @((N 9 :% 1) ~ ((P 3) % (N 1 :% 3)))
_testSlash_IntP3_Rat1N4 = Dict @((N 12 :% 1) ~ ((P 3) % (N 1 :% 4)))
_testSlash_IntP3_Rat1P4 = Dict @((P 12 :% 1) ~ ((P 3) % (P 1 :% 4)))
_testSlash_IntP3_Rat1P3 = Dict @((P 9 :% 1) ~ ((P 3) % (P 1 :% 3)))
_testSlash_IntP3_Rat1P2 = Dict @((P 6 :% 1) ~ ((P 3) % (P 1 :% 2)))
_testSlash_IntP3_Rat2P3 = Dict @((P 9 :% 2) ~ ((P 3) % (P 2 :% 3)))
_testSlash_IntP3_Rat3P4 = Dict @((P 4 :% 1) ~ ((P 3) % (P 3 :% 4)))
_testSlash_IntP3_Rat1P1 = Dict @((P 3 :% 1) ~ ((P 3) % (P 1 :% 1)))
_testSlash_IntP3_Rat4P3 = Dict @((P 9 :% 4) ~ ((P 3) % (P 4 :% 3)))
_testSlash_IntP3_Rat3P2 = Dict @((P 2 :% 1) ~ ((P 3) % (P 3 :% 2)))
_testSlash_IntP3_Rat2P1 = Dict @((P 3 :% 2) ~ ((P 3) % (P 2 :% 1)))
_testSlash_IntP3_Rat3P1 = Dict @((P 1 :% 1) ~ ((P 3) % (P 3 :% 1)))
_testSlash_IntP3_Rat4P1 = Dict @((P 3 :% 4) ~ ((P 3) % (P 4 :% 1)))
_testSlash_IntP4_Nat1 = Dict @((P 4 :% 1) ~ ((P 4) % 1))
_testSlash_IntP4_Nat2 = Dict @((P 2 :% 1) ~ ((P 4) % 2))
_testSlash_IntP4_Nat3 = Dict @((P 4 :% 3) ~ ((P 4) % 3))
_testSlash_IntP4_Nat4 = Dict @((P 1 :% 1) ~ ((P 4) % 4))
_testSlash_IntP4_IntN4 = Dict @((N 1 :% 1) ~ ((P 4) % (N 4)))
_testSlash_IntP4_IntN3 = Dict @((N 4 :% 3) ~ ((P 4) % (N 3)))
_testSlash_IntP4_IntN2 = Dict @((N 2 :% 1) ~ ((P 4) % (N 2)))
_testSlash_IntP4_IntN1 = Dict @((N 4 :% 1) ~ ((P 4) % (N 1)))
_testSlash_IntP4_IntP1 = Dict @((P 4 :% 1) ~ ((P 4) % (P 1)))
_testSlash_IntP4_IntP2 = Dict @((P 2 :% 1) ~ ((P 4) % (P 2)))
_testSlash_IntP4_IntP3 = Dict @((P 4 :% 3) ~ ((P 4) % (P 3)))
_testSlash_IntP4_IntP4 = Dict @((P 1 :% 1) ~ ((P 4) % (P 4)))
_testSlash_IntP4_Rat4N1 = Dict @((N 1 :% 1) ~ ((P 4) % (N 4 :% 1)))
_testSlash_IntP4_Rat3N1 = Dict @((N 4 :% 3) ~ ((P 4) % (N 3 :% 1)))
_testSlash_IntP4_Rat2N1 = Dict @((N 2 :% 1) ~ ((P 4) % (N 2 :% 1)))
_testSlash_IntP4_Rat3N2 = Dict @((N 8 :% 3) ~ ((P 4) % (N 3 :% 2)))
_testSlash_IntP4_Rat4N3 = Dict @((N 3 :% 1) ~ ((P 4) % (N 4 :% 3)))
_testSlash_IntP4_Rat1N1 = Dict @((N 4 :% 1) ~ ((P 4) % (N 1 :% 1)))
_testSlash_IntP4_Rat3N4 = Dict @((N 16 :% 3) ~ ((P 4) % (N 3 :% 4)))
_testSlash_IntP4_Rat2N3 = Dict @((N 6 :% 1) ~ ((P 4) % (N 2 :% 3)))
_testSlash_IntP4_Rat1N2 = Dict @((N 8 :% 1) ~ ((P 4) % (N 1 :% 2)))
_testSlash_IntP4_Rat1N3 = Dict @((N 12 :% 1) ~ ((P 4) % (N 1 :% 3)))
_testSlash_IntP4_Rat1N4 = Dict @((N 16 :% 1) ~ ((P 4) % (N 1 :% 4)))
_testSlash_IntP4_Rat1P4 = Dict @((P 16 :% 1) ~ ((P 4) % (P 1 :% 4)))
_testSlash_IntP4_Rat1P3 = Dict @((P 12 :% 1) ~ ((P 4) % (P 1 :% 3)))
_testSlash_IntP4_Rat1P2 = Dict @((P 8 :% 1) ~ ((P 4) % (P 1 :% 2)))
_testSlash_IntP4_Rat2P3 = Dict @((P 6 :% 1) ~ ((P 4) % (P 2 :% 3)))
_testSlash_IntP4_Rat3P4 = Dict @((P 16 :% 3) ~ ((P 4) % (P 3 :% 4)))
_testSlash_IntP4_Rat1P1 = Dict @((P 4 :% 1) ~ ((P 4) % (P 1 :% 1)))
_testSlash_IntP4_Rat4P3 = Dict @((P 3 :% 1) ~ ((P 4) % (P 4 :% 3)))
_testSlash_IntP4_Rat3P2 = Dict @((P 8 :% 3) ~ ((P 4) % (P 3 :% 2)))
_testSlash_IntP4_Rat2P1 = Dict @((P 2 :% 1) ~ ((P 4) % (P 2 :% 1)))
_testSlash_IntP4_Rat3P1 = Dict @((P 4 :% 3) ~ ((P 4) % (P 3 :% 1)))
_testSlash_IntP4_Rat4P1 = Dict @((P 1 :% 1) ~ ((P 4) % (P 4 :% 1)))
_testSlash_Rat4N1_Nat1 = Dict @((N 4 :% 1) ~ ((N 4 :% 1) % 1))
_testSlash_Rat4N1_Nat2 = Dict @((N 2 :% 1) ~ ((N 4 :% 1) % 2))
_testSlash_Rat4N1_Nat3 = Dict @((N 4 :% 3) ~ ((N 4 :% 1) % 3))
_testSlash_Rat4N1_Nat4 = Dict @((N 1 :% 1) ~ ((N 4 :% 1) % 4))
_testSlash_Rat4N1_IntN4 = Dict @((P 1 :% 1) ~ ((N 4 :% 1) % (N 4)))
_testSlash_Rat4N1_IntN3 = Dict @((P 4 :% 3) ~ ((N 4 :% 1) % (N 3)))
_testSlash_Rat4N1_IntN2 = Dict @((P 2 :% 1) ~ ((N 4 :% 1) % (N 2)))
_testSlash_Rat4N1_IntN1 = Dict @((P 4 :% 1) ~ ((N 4 :% 1) % (N 1)))
_testSlash_Rat4N1_IntP1 = Dict @((N 4 :% 1) ~ ((N 4 :% 1) % (P 1)))
_testSlash_Rat4N1_IntP2 = Dict @((N 2 :% 1) ~ ((N 4 :% 1) % (P 2)))
_testSlash_Rat4N1_IntP3 = Dict @((N 4 :% 3) ~ ((N 4 :% 1) % (P 3)))
_testSlash_Rat4N1_IntP4 = Dict @((N 1 :% 1) ~ ((N 4 :% 1) % (P 4)))
_testSlash_Rat4N1_Rat4N1 = Dict @((P 1 :% 1) ~ ((N 4 :% 1) % (N 4 :% 1)))
_testSlash_Rat4N1_Rat3N1 = Dict @((P 4 :% 3) ~ ((N 4 :% 1) % (N 3 :% 1)))
_testSlash_Rat4N1_Rat2N1 = Dict @((P 2 :% 1) ~ ((N 4 :% 1) % (N 2 :% 1)))
_testSlash_Rat4N1_Rat3N2 = Dict @((P 8 :% 3) ~ ((N 4 :% 1) % (N 3 :% 2)))
_testSlash_Rat4N1_Rat4N3 = Dict @((P 3 :% 1) ~ ((N 4 :% 1) % (N 4 :% 3)))
_testSlash_Rat4N1_Rat1N1 = Dict @((P 4 :% 1) ~ ((N 4 :% 1) % (N 1 :% 1)))
_testSlash_Rat4N1_Rat3N4 = Dict @((P 16 :% 3) ~ ((N 4 :% 1) % (N 3 :% 4)))
_testSlash_Rat4N1_Rat2N3 = Dict @((P 6 :% 1) ~ ((N 4 :% 1) % (N 2 :% 3)))
_testSlash_Rat4N1_Rat1N2 = Dict @((P 8 :% 1) ~ ((N 4 :% 1) % (N 1 :% 2)))
_testSlash_Rat4N1_Rat1N3 = Dict @((P 12 :% 1) ~ ((N 4 :% 1) % (N 1 :% 3)))
_testSlash_Rat4N1_Rat1N4 = Dict @((P 16 :% 1) ~ ((N 4 :% 1) % (N 1 :% 4)))
_testSlash_Rat4N1_Rat1P4 = Dict @((N 16 :% 1) ~ ((N 4 :% 1) % (P 1 :% 4)))
_testSlash_Rat4N1_Rat1P3 = Dict @((N 12 :% 1) ~ ((N 4 :% 1) % (P 1 :% 3)))
_testSlash_Rat4N1_Rat1P2 = Dict @((N 8 :% 1) ~ ((N 4 :% 1) % (P 1 :% 2)))
_testSlash_Rat4N1_Rat2P3 = Dict @((N 6 :% 1) ~ ((N 4 :% 1) % (P 2 :% 3)))
_testSlash_Rat4N1_Rat3P4 = Dict @((N 16 :% 3) ~ ((N 4 :% 1) % (P 3 :% 4)))
_testSlash_Rat4N1_Rat1P1 = Dict @((N 4 :% 1) ~ ((N 4 :% 1) % (P 1 :% 1)))
_testSlash_Rat4N1_Rat4P3 = Dict @((N 3 :% 1) ~ ((N 4 :% 1) % (P 4 :% 3)))
_testSlash_Rat4N1_Rat3P2 = Dict @((N 8 :% 3) ~ ((N 4 :% 1) % (P 3 :% 2)))
_testSlash_Rat4N1_Rat2P1 = Dict @((N 2 :% 1) ~ ((N 4 :% 1) % (P 2 :% 1)))
_testSlash_Rat4N1_Rat3P1 = Dict @((N 4 :% 3) ~ ((N 4 :% 1) % (P 3 :% 1)))
_testSlash_Rat4N1_Rat4P1 = Dict @((N 1 :% 1) ~ ((N 4 :% 1) % (P 4 :% 1)))
_testSlash_Rat3N1_Nat1 = Dict @((N 3 :% 1) ~ ((N 3 :% 1) % 1))
_testSlash_Rat3N1_Nat2 = Dict @((N 3 :% 2) ~ ((N 3 :% 1) % 2))
_testSlash_Rat3N1_Nat3 = Dict @((N 1 :% 1) ~ ((N 3 :% 1) % 3))
_testSlash_Rat3N1_Nat4 = Dict @((N 3 :% 4) ~ ((N 3 :% 1) % 4))
_testSlash_Rat3N1_IntN4 = Dict @((P 3 :% 4) ~ ((N 3 :% 1) % (N 4)))
_testSlash_Rat3N1_IntN3 = Dict @((P 1 :% 1) ~ ((N 3 :% 1) % (N 3)))
_testSlash_Rat3N1_IntN2 = Dict @((P 3 :% 2) ~ ((N 3 :% 1) % (N 2)))
_testSlash_Rat3N1_IntN1 = Dict @((P 3 :% 1) ~ ((N 3 :% 1) % (N 1)))
_testSlash_Rat3N1_IntP1 = Dict @((N 3 :% 1) ~ ((N 3 :% 1) % (P 1)))
_testSlash_Rat3N1_IntP2 = Dict @((N 3 :% 2) ~ ((N 3 :% 1) % (P 2)))
_testSlash_Rat3N1_IntP3 = Dict @((N 1 :% 1) ~ ((N 3 :% 1) % (P 3)))
_testSlash_Rat3N1_IntP4 = Dict @((N 3 :% 4) ~ ((N 3 :% 1) % (P 4)))
_testSlash_Rat3N1_Rat4N1 = Dict @((P 3 :% 4) ~ ((N 3 :% 1) % (N 4 :% 1)))
_testSlash_Rat3N1_Rat3N1 = Dict @((P 1 :% 1) ~ ((N 3 :% 1) % (N 3 :% 1)))
_testSlash_Rat3N1_Rat2N1 = Dict @((P 3 :% 2) ~ ((N 3 :% 1) % (N 2 :% 1)))
_testSlash_Rat3N1_Rat3N2 = Dict @((P 2 :% 1) ~ ((N 3 :% 1) % (N 3 :% 2)))
_testSlash_Rat3N1_Rat4N3 = Dict @((P 9 :% 4) ~ ((N 3 :% 1) % (N 4 :% 3)))
_testSlash_Rat3N1_Rat1N1 = Dict @((P 3 :% 1) ~ ((N 3 :% 1) % (N 1 :% 1)))
_testSlash_Rat3N1_Rat3N4 = Dict @((P 4 :% 1) ~ ((N 3 :% 1) % (N 3 :% 4)))
_testSlash_Rat3N1_Rat2N3 = Dict @((P 9 :% 2) ~ ((N 3 :% 1) % (N 2 :% 3)))
_testSlash_Rat3N1_Rat1N2 = Dict @((P 6 :% 1) ~ ((N 3 :% 1) % (N 1 :% 2)))
_testSlash_Rat3N1_Rat1N3 = Dict @((P 9 :% 1) ~ ((N 3 :% 1) % (N 1 :% 3)))
_testSlash_Rat3N1_Rat1N4 = Dict @((P 12 :% 1) ~ ((N 3 :% 1) % (N 1 :% 4)))
_testSlash_Rat3N1_Rat1P4 = Dict @((N 12 :% 1) ~ ((N 3 :% 1) % (P 1 :% 4)))
_testSlash_Rat3N1_Rat1P3 = Dict @((N 9 :% 1) ~ ((N 3 :% 1) % (P 1 :% 3)))
_testSlash_Rat3N1_Rat1P2 = Dict @((N 6 :% 1) ~ ((N 3 :% 1) % (P 1 :% 2)))
_testSlash_Rat3N1_Rat2P3 = Dict @((N 9 :% 2) ~ ((N 3 :% 1) % (P 2 :% 3)))
_testSlash_Rat3N1_Rat3P4 = Dict @((N 4 :% 1) ~ ((N 3 :% 1) % (P 3 :% 4)))
_testSlash_Rat3N1_Rat1P1 = Dict @((N 3 :% 1) ~ ((N 3 :% 1) % (P 1 :% 1)))
_testSlash_Rat3N1_Rat4P3 = Dict @((N 9 :% 4) ~ ((N 3 :% 1) % (P 4 :% 3)))
_testSlash_Rat3N1_Rat3P2 = Dict @((N 2 :% 1) ~ ((N 3 :% 1) % (P 3 :% 2)))
_testSlash_Rat3N1_Rat2P1 = Dict @((N 3 :% 2) ~ ((N 3 :% 1) % (P 2 :% 1)))
_testSlash_Rat3N1_Rat3P1 = Dict @((N 1 :% 1) ~ ((N 3 :% 1) % (P 3 :% 1)))
_testSlash_Rat3N1_Rat4P1 = Dict @((N 3 :% 4) ~ ((N 3 :% 1) % (P 4 :% 1)))
_testSlash_Rat2N1_Nat1 = Dict @((N 2 :% 1) ~ ((N 2 :% 1) % 1))
_testSlash_Rat2N1_Nat2 = Dict @((N 1 :% 1) ~ ((N 2 :% 1) % 2))
_testSlash_Rat2N1_Nat3 = Dict @((N 2 :% 3) ~ ((N 2 :% 1) % 3))
_testSlash_Rat2N1_Nat4 = Dict @((N 1 :% 2) ~ ((N 2 :% 1) % 4))
_testSlash_Rat2N1_IntN4 = Dict @((P 1 :% 2) ~ ((N 2 :% 1) % (N 4)))
_testSlash_Rat2N1_IntN3 = Dict @((P 2 :% 3) ~ ((N 2 :% 1) % (N 3)))
_testSlash_Rat2N1_IntN2 = Dict @((P 1 :% 1) ~ ((N 2 :% 1) % (N 2)))
_testSlash_Rat2N1_IntN1 = Dict @((P 2 :% 1) ~ ((N 2 :% 1) % (N 1)))
_testSlash_Rat2N1_IntP1 = Dict @((N 2 :% 1) ~ ((N 2 :% 1) % (P 1)))
_testSlash_Rat2N1_IntP2 = Dict @((N 1 :% 1) ~ ((N 2 :% 1) % (P 2)))
_testSlash_Rat2N1_IntP3 = Dict @((N 2 :% 3) ~ ((N 2 :% 1) % (P 3)))
_testSlash_Rat2N1_IntP4 = Dict @((N 1 :% 2) ~ ((N 2 :% 1) % (P 4)))
_testSlash_Rat2N1_Rat4N1 = Dict @((P 1 :% 2) ~ ((N 2 :% 1) % (N 4 :% 1)))
_testSlash_Rat2N1_Rat3N1 = Dict @((P 2 :% 3) ~ ((N 2 :% 1) % (N 3 :% 1)))
_testSlash_Rat2N1_Rat2N1 = Dict @((P 1 :% 1) ~ ((N 2 :% 1) % (N 2 :% 1)))
_testSlash_Rat2N1_Rat3N2 = Dict @((P 4 :% 3) ~ ((N 2 :% 1) % (N 3 :% 2)))
_testSlash_Rat2N1_Rat4N3 = Dict @((P 3 :% 2) ~ ((N 2 :% 1) % (N 4 :% 3)))
_testSlash_Rat2N1_Rat1N1 = Dict @((P 2 :% 1) ~ ((N 2 :% 1) % (N 1 :% 1)))
_testSlash_Rat2N1_Rat3N4 = Dict @((P 8 :% 3) ~ ((N 2 :% 1) % (N 3 :% 4)))
_testSlash_Rat2N1_Rat2N3 = Dict @((P 3 :% 1) ~ ((N 2 :% 1) % (N 2 :% 3)))
_testSlash_Rat2N1_Rat1N2 = Dict @((P 4 :% 1) ~ ((N 2 :% 1) % (N 1 :% 2)))
_testSlash_Rat2N1_Rat1N3 = Dict @((P 6 :% 1) ~ ((N 2 :% 1) % (N 1 :% 3)))
_testSlash_Rat2N1_Rat1N4 = Dict @((P 8 :% 1) ~ ((N 2 :% 1) % (N 1 :% 4)))
_testSlash_Rat2N1_Rat1P4 = Dict @((N 8 :% 1) ~ ((N 2 :% 1) % (P 1 :% 4)))
_testSlash_Rat2N1_Rat1P3 = Dict @((N 6 :% 1) ~ ((N 2 :% 1) % (P 1 :% 3)))
_testSlash_Rat2N1_Rat1P2 = Dict @((N 4 :% 1) ~ ((N 2 :% 1) % (P 1 :% 2)))
_testSlash_Rat2N1_Rat2P3 = Dict @((N 3 :% 1) ~ ((N 2 :% 1) % (P 2 :% 3)))
_testSlash_Rat2N1_Rat3P4 = Dict @((N 8 :% 3) ~ ((N 2 :% 1) % (P 3 :% 4)))
_testSlash_Rat2N1_Rat1P1 = Dict @((N 2 :% 1) ~ ((N 2 :% 1) % (P 1 :% 1)))
_testSlash_Rat2N1_Rat4P3 = Dict @((N 3 :% 2) ~ ((N 2 :% 1) % (P 4 :% 3)))
_testSlash_Rat2N1_Rat3P2 = Dict @((N 4 :% 3) ~ ((N 2 :% 1) % (P 3 :% 2)))
_testSlash_Rat2N1_Rat2P1 = Dict @((N 1 :% 1) ~ ((N 2 :% 1) % (P 2 :% 1)))
_testSlash_Rat2N1_Rat3P1 = Dict @((N 2 :% 3) ~ ((N 2 :% 1) % (P 3 :% 1)))
_testSlash_Rat2N1_Rat4P1 = Dict @((N 1 :% 2) ~ ((N 2 :% 1) % (P 4 :% 1)))
_testSlash_Rat3N2_Nat1 = Dict @((N 3 :% 2) ~ ((N 3 :% 2) % 1))
_testSlash_Rat3N2_Nat2 = Dict @((N 3 :% 4) ~ ((N 3 :% 2) % 2))
_testSlash_Rat3N2_Nat3 = Dict @((N 1 :% 2) ~ ((N 3 :% 2) % 3))
_testSlash_Rat3N2_Nat4 = Dict @((N 3 :% 8) ~ ((N 3 :% 2) % 4))
_testSlash_Rat3N2_IntN4 = Dict @((P 3 :% 8) ~ ((N 3 :% 2) % (N 4)))
_testSlash_Rat3N2_IntN3 = Dict @((P 1 :% 2) ~ ((N 3 :% 2) % (N 3)))
_testSlash_Rat3N2_IntN2 = Dict @((P 3 :% 4) ~ ((N 3 :% 2) % (N 2)))
_testSlash_Rat3N2_IntN1 = Dict @((P 3 :% 2) ~ ((N 3 :% 2) % (N 1)))
_testSlash_Rat3N2_IntP1 = Dict @((N 3 :% 2) ~ ((N 3 :% 2) % (P 1)))
_testSlash_Rat3N2_IntP2 = Dict @((N 3 :% 4) ~ ((N 3 :% 2) % (P 2)))
_testSlash_Rat3N2_IntP3 = Dict @((N 1 :% 2) ~ ((N 3 :% 2) % (P 3)))
_testSlash_Rat3N2_IntP4 = Dict @((N 3 :% 8) ~ ((N 3 :% 2) % (P 4)))
_testSlash_Rat3N2_Rat4N1 = Dict @((P 3 :% 8) ~ ((N 3 :% 2) % (N 4 :% 1)))
_testSlash_Rat3N2_Rat3N1 = Dict @((P 1 :% 2) ~ ((N 3 :% 2) % (N 3 :% 1)))
_testSlash_Rat3N2_Rat2N1 = Dict @((P 3 :% 4) ~ ((N 3 :% 2) % (N 2 :% 1)))
_testSlash_Rat3N2_Rat3N2 = Dict @((P 1 :% 1) ~ ((N 3 :% 2) % (N 3 :% 2)))
_testSlash_Rat3N2_Rat4N3 = Dict @((P 9 :% 8) ~ ((N 3 :% 2) % (N 4 :% 3)))
_testSlash_Rat3N2_Rat1N1 = Dict @((P 3 :% 2) ~ ((N 3 :% 2) % (N 1 :% 1)))
_testSlash_Rat3N2_Rat3N4 = Dict @((P 2 :% 1) ~ ((N 3 :% 2) % (N 3 :% 4)))
_testSlash_Rat3N2_Rat2N3 = Dict @((P 9 :% 4) ~ ((N 3 :% 2) % (N 2 :% 3)))
_testSlash_Rat3N2_Rat1N2 = Dict @((P 3 :% 1) ~ ((N 3 :% 2) % (N 1 :% 2)))
_testSlash_Rat3N2_Rat1N3 = Dict @((P 9 :% 2) ~ ((N 3 :% 2) % (N 1 :% 3)))
_testSlash_Rat3N2_Rat1N4 = Dict @((P 6 :% 1) ~ ((N 3 :% 2) % (N 1 :% 4)))
_testSlash_Rat3N2_Rat1P4 = Dict @((N 6 :% 1) ~ ((N 3 :% 2) % (P 1 :% 4)))
_testSlash_Rat3N2_Rat1P3 = Dict @((N 9 :% 2) ~ ((N 3 :% 2) % (P 1 :% 3)))
_testSlash_Rat3N2_Rat1P2 = Dict @((N 3 :% 1) ~ ((N 3 :% 2) % (P 1 :% 2)))
_testSlash_Rat3N2_Rat2P3 = Dict @((N 9 :% 4) ~ ((N 3 :% 2) % (P 2 :% 3)))
_testSlash_Rat3N2_Rat3P4 = Dict @((N 2 :% 1) ~ ((N 3 :% 2) % (P 3 :% 4)))
_testSlash_Rat3N2_Rat1P1 = Dict @((N 3 :% 2) ~ ((N 3 :% 2) % (P 1 :% 1)))
_testSlash_Rat3N2_Rat4P3 = Dict @((N 9 :% 8) ~ ((N 3 :% 2) % (P 4 :% 3)))
_testSlash_Rat3N2_Rat3P2 = Dict @((N 1 :% 1) ~ ((N 3 :% 2) % (P 3 :% 2)))
_testSlash_Rat3N2_Rat2P1 = Dict @((N 3 :% 4) ~ ((N 3 :% 2) % (P 2 :% 1)))
_testSlash_Rat3N2_Rat3P1 = Dict @((N 1 :% 2) ~ ((N 3 :% 2) % (P 3 :% 1)))
_testSlash_Rat3N2_Rat4P1 = Dict @((N 3 :% 8) ~ ((N 3 :% 2) % (P 4 :% 1)))
_testSlash_Rat4N3_Nat1 = Dict @((N 4 :% 3) ~ ((N 4 :% 3) % 1))
_testSlash_Rat4N3_Nat2 = Dict @((N 2 :% 3) ~ ((N 4 :% 3) % 2))
_testSlash_Rat4N3_Nat3 = Dict @((N 4 :% 9) ~ ((N 4 :% 3) % 3))
_testSlash_Rat4N3_Nat4 = Dict @((N 1 :% 3) ~ ((N 4 :% 3) % 4))
_testSlash_Rat4N3_IntN4 = Dict @((P 1 :% 3) ~ ((N 4 :% 3) % (N 4)))
_testSlash_Rat4N3_IntN3 = Dict @((P 4 :% 9) ~ ((N 4 :% 3) % (N 3)))
_testSlash_Rat4N3_IntN2 = Dict @((P 2 :% 3) ~ ((N 4 :% 3) % (N 2)))
_testSlash_Rat4N3_IntN1 = Dict @((P 4 :% 3) ~ ((N 4 :% 3) % (N 1)))
_testSlash_Rat4N3_IntP1 = Dict @((N 4 :% 3) ~ ((N 4 :% 3) % (P 1)))
_testSlash_Rat4N3_IntP2 = Dict @((N 2 :% 3) ~ ((N 4 :% 3) % (P 2)))
_testSlash_Rat4N3_IntP3 = Dict @((N 4 :% 9) ~ ((N 4 :% 3) % (P 3)))
_testSlash_Rat4N3_IntP4 = Dict @((N 1 :% 3) ~ ((N 4 :% 3) % (P 4)))
_testSlash_Rat4N3_Rat4N1 = Dict @((P 1 :% 3) ~ ((N 4 :% 3) % (N 4 :% 1)))
_testSlash_Rat4N3_Rat3N1 = Dict @((P 4 :% 9) ~ ((N 4 :% 3) % (N 3 :% 1)))
_testSlash_Rat4N3_Rat2N1 = Dict @((P 2 :% 3) ~ ((N 4 :% 3) % (N 2 :% 1)))
_testSlash_Rat4N3_Rat3N2 = Dict @((P 8 :% 9) ~ ((N 4 :% 3) % (N 3 :% 2)))
_testSlash_Rat4N3_Rat4N3 = Dict @((P 1 :% 1) ~ ((N 4 :% 3) % (N 4 :% 3)))
_testSlash_Rat4N3_Rat1N1 = Dict @((P 4 :% 3) ~ ((N 4 :% 3) % (N 1 :% 1)))
_testSlash_Rat4N3_Rat3N4 = Dict @((P 16 :% 9) ~ ((N 4 :% 3) % (N 3 :% 4)))
_testSlash_Rat4N3_Rat2N3 = Dict @((P 2 :% 1) ~ ((N 4 :% 3) % (N 2 :% 3)))
_testSlash_Rat4N3_Rat1N2 = Dict @((P 8 :% 3) ~ ((N 4 :% 3) % (N 1 :% 2)))
_testSlash_Rat4N3_Rat1N3 = Dict @((P 4 :% 1) ~ ((N 4 :% 3) % (N 1 :% 3)))
_testSlash_Rat4N3_Rat1N4 = Dict @((P 16 :% 3) ~ ((N 4 :% 3) % (N 1 :% 4)))
_testSlash_Rat4N3_Rat1P4 = Dict @((N 16 :% 3) ~ ((N 4 :% 3) % (P 1 :% 4)))
_testSlash_Rat4N3_Rat1P3 = Dict @((N 4 :% 1) ~ ((N 4 :% 3) % (P 1 :% 3)))
_testSlash_Rat4N3_Rat1P2 = Dict @((N 8 :% 3) ~ ((N 4 :% 3) % (P 1 :% 2)))
_testSlash_Rat4N3_Rat2P3 = Dict @((N 2 :% 1) ~ ((N 4 :% 3) % (P 2 :% 3)))
_testSlash_Rat4N3_Rat3P4 = Dict @((N 16 :% 9) ~ ((N 4 :% 3) % (P 3 :% 4)))
_testSlash_Rat4N3_Rat1P1 = Dict @((N 4 :% 3) ~ ((N 4 :% 3) % (P 1 :% 1)))
_testSlash_Rat4N3_Rat4P3 = Dict @((N 1 :% 1) ~ ((N 4 :% 3) % (P 4 :% 3)))
_testSlash_Rat4N3_Rat3P2 = Dict @((N 8 :% 9) ~ ((N 4 :% 3) % (P 3 :% 2)))
_testSlash_Rat4N3_Rat2P1 = Dict @((N 2 :% 3) ~ ((N 4 :% 3) % (P 2 :% 1)))
_testSlash_Rat4N3_Rat3P1 = Dict @((N 4 :% 9) ~ ((N 4 :% 3) % (P 3 :% 1)))
_testSlash_Rat4N3_Rat4P1 = Dict @((N 1 :% 3) ~ ((N 4 :% 3) % (P 4 :% 1)))
_testSlash_Rat1N1_Nat1 = Dict @((N 1 :% 1) ~ ((N 1 :% 1) % 1))
_testSlash_Rat1N1_Nat2 = Dict @((N 1 :% 2) ~ ((N 1 :% 1) % 2))
_testSlash_Rat1N1_Nat3 = Dict @((N 1 :% 3) ~ ((N 1 :% 1) % 3))
_testSlash_Rat1N1_Nat4 = Dict @((N 1 :% 4) ~ ((N 1 :% 1) % 4))
_testSlash_Rat1N1_IntN4 = Dict @((P 1 :% 4) ~ ((N 1 :% 1) % (N 4)))
_testSlash_Rat1N1_IntN3 = Dict @((P 1 :% 3) ~ ((N 1 :% 1) % (N 3)))
_testSlash_Rat1N1_IntN2 = Dict @((P 1 :% 2) ~ ((N 1 :% 1) % (N 2)))
_testSlash_Rat1N1_IntN1 = Dict @((P 1 :% 1) ~ ((N 1 :% 1) % (N 1)))
_testSlash_Rat1N1_IntP1 = Dict @((N 1 :% 1) ~ ((N 1 :% 1) % (P 1)))
_testSlash_Rat1N1_IntP2 = Dict @((N 1 :% 2) ~ ((N 1 :% 1) % (P 2)))
_testSlash_Rat1N1_IntP3 = Dict @((N 1 :% 3) ~ ((N 1 :% 1) % (P 3)))
_testSlash_Rat1N1_IntP4 = Dict @((N 1 :% 4) ~ ((N 1 :% 1) % (P 4)))
_testSlash_Rat1N1_Rat4N1 = Dict @((P 1 :% 4) ~ ((N 1 :% 1) % (N 4 :% 1)))
_testSlash_Rat1N1_Rat3N1 = Dict @((P 1 :% 3) ~ ((N 1 :% 1) % (N 3 :% 1)))
_testSlash_Rat1N1_Rat2N1 = Dict @((P 1 :% 2) ~ ((N 1 :% 1) % (N 2 :% 1)))
_testSlash_Rat1N1_Rat3N2 = Dict @((P 2 :% 3) ~ ((N 1 :% 1) % (N 3 :% 2)))
_testSlash_Rat1N1_Rat4N3 = Dict @((P 3 :% 4) ~ ((N 1 :% 1) % (N 4 :% 3)))
_testSlash_Rat1N1_Rat1N1 = Dict @((P 1 :% 1) ~ ((N 1 :% 1) % (N 1 :% 1)))
_testSlash_Rat1N1_Rat3N4 = Dict @((P 4 :% 3) ~ ((N 1 :% 1) % (N 3 :% 4)))
_testSlash_Rat1N1_Rat2N3 = Dict @((P 3 :% 2) ~ ((N 1 :% 1) % (N 2 :% 3)))
_testSlash_Rat1N1_Rat1N2 = Dict @((P 2 :% 1) ~ ((N 1 :% 1) % (N 1 :% 2)))
_testSlash_Rat1N1_Rat1N3 = Dict @((P 3 :% 1) ~ ((N 1 :% 1) % (N 1 :% 3)))
_testSlash_Rat1N1_Rat1N4 = Dict @((P 4 :% 1) ~ ((N 1 :% 1) % (N 1 :% 4)))
_testSlash_Rat1N1_Rat1P4 = Dict @((N 4 :% 1) ~ ((N 1 :% 1) % (P 1 :% 4)))
_testSlash_Rat1N1_Rat1P3 = Dict @((N 3 :% 1) ~ ((N 1 :% 1) % (P 1 :% 3)))
_testSlash_Rat1N1_Rat1P2 = Dict @((N 2 :% 1) ~ ((N 1 :% 1) % (P 1 :% 2)))
_testSlash_Rat1N1_Rat2P3 = Dict @((N 3 :% 2) ~ ((N 1 :% 1) % (P 2 :% 3)))
_testSlash_Rat1N1_Rat3P4 = Dict @((N 4 :% 3) ~ ((N 1 :% 1) % (P 3 :% 4)))
_testSlash_Rat1N1_Rat1P1 = Dict @((N 1 :% 1) ~ ((N 1 :% 1) % (P 1 :% 1)))
_testSlash_Rat1N1_Rat4P3 = Dict @((N 3 :% 4) ~ ((N 1 :% 1) % (P 4 :% 3)))
_testSlash_Rat1N1_Rat3P2 = Dict @((N 2 :% 3) ~ ((N 1 :% 1) % (P 3 :% 2)))
_testSlash_Rat1N1_Rat2P1 = Dict @((N 1 :% 2) ~ ((N 1 :% 1) % (P 2 :% 1)))
_testSlash_Rat1N1_Rat3P1 = Dict @((N 1 :% 3) ~ ((N 1 :% 1) % (P 3 :% 1)))
_testSlash_Rat1N1_Rat4P1 = Dict @((N 1 :% 4) ~ ((N 1 :% 1) % (P 4 :% 1)))
_testSlash_Rat3N4_Nat1 = Dict @((N 3 :% 4) ~ ((N 3 :% 4) % 1))
_testSlash_Rat3N4_Nat2 = Dict @((N 3 :% 8) ~ ((N 3 :% 4) % 2))
_testSlash_Rat3N4_Nat3 = Dict @((N 1 :% 4) ~ ((N 3 :% 4) % 3))
_testSlash_Rat3N4_Nat4 = Dict @((N 3 :% 16) ~ ((N 3 :% 4) % 4))
_testSlash_Rat3N4_IntN4 = Dict @((P 3 :% 16) ~ ((N 3 :% 4) % (N 4)))
_testSlash_Rat3N4_IntN3 = Dict @((P 1 :% 4) ~ ((N 3 :% 4) % (N 3)))
_testSlash_Rat3N4_IntN2 = Dict @((P 3 :% 8) ~ ((N 3 :% 4) % (N 2)))
_testSlash_Rat3N4_IntN1 = Dict @((P 3 :% 4) ~ ((N 3 :% 4) % (N 1)))
_testSlash_Rat3N4_IntP1 = Dict @((N 3 :% 4) ~ ((N 3 :% 4) % (P 1)))
_testSlash_Rat3N4_IntP2 = Dict @((N 3 :% 8) ~ ((N 3 :% 4) % (P 2)))
_testSlash_Rat3N4_IntP3 = Dict @((N 1 :% 4) ~ ((N 3 :% 4) % (P 3)))
_testSlash_Rat3N4_IntP4 = Dict @((N 3 :% 16) ~ ((N 3 :% 4) % (P 4)))
_testSlash_Rat3N4_Rat4N1 = Dict @((P 3 :% 16) ~ ((N 3 :% 4) % (N 4 :% 1)))
_testSlash_Rat3N4_Rat3N1 = Dict @((P 1 :% 4) ~ ((N 3 :% 4) % (N 3 :% 1)))
_testSlash_Rat3N4_Rat2N1 = Dict @((P 3 :% 8) ~ ((N 3 :% 4) % (N 2 :% 1)))
_testSlash_Rat3N4_Rat3N2 = Dict @((P 1 :% 2) ~ ((N 3 :% 4) % (N 3 :% 2)))
_testSlash_Rat3N4_Rat4N3 = Dict @((P 9 :% 16) ~ ((N 3 :% 4) % (N 4 :% 3)))
_testSlash_Rat3N4_Rat1N1 = Dict @((P 3 :% 4) ~ ((N 3 :% 4) % (N 1 :% 1)))
_testSlash_Rat3N4_Rat3N4 = Dict @((P 1 :% 1) ~ ((N 3 :% 4) % (N 3 :% 4)))
_testSlash_Rat3N4_Rat2N3 = Dict @((P 9 :% 8) ~ ((N 3 :% 4) % (N 2 :% 3)))
_testSlash_Rat3N4_Rat1N2 = Dict @((P 3 :% 2) ~ ((N 3 :% 4) % (N 1 :% 2)))
_testSlash_Rat3N4_Rat1N3 = Dict @((P 9 :% 4) ~ ((N 3 :% 4) % (N 1 :% 3)))
_testSlash_Rat3N4_Rat1N4 = Dict @((P 3 :% 1) ~ ((N 3 :% 4) % (N 1 :% 4)))
_testSlash_Rat3N4_Rat1P4 = Dict @((N 3 :% 1) ~ ((N 3 :% 4) % (P 1 :% 4)))
_testSlash_Rat3N4_Rat1P3 = Dict @((N 9 :% 4) ~ ((N 3 :% 4) % (P 1 :% 3)))
_testSlash_Rat3N4_Rat1P2 = Dict @((N 3 :% 2) ~ ((N 3 :% 4) % (P 1 :% 2)))
_testSlash_Rat3N4_Rat2P3 = Dict @((N 9 :% 8) ~ ((N 3 :% 4) % (P 2 :% 3)))
_testSlash_Rat3N4_Rat3P4 = Dict @((N 1 :% 1) ~ ((N 3 :% 4) % (P 3 :% 4)))
_testSlash_Rat3N4_Rat1P1 = Dict @((N 3 :% 4) ~ ((N 3 :% 4) % (P 1 :% 1)))
_testSlash_Rat3N4_Rat4P3 = Dict @((N 9 :% 16) ~ ((N 3 :% 4) % (P 4 :% 3)))
_testSlash_Rat3N4_Rat3P2 = Dict @((N 1 :% 2) ~ ((N 3 :% 4) % (P 3 :% 2)))
_testSlash_Rat3N4_Rat2P1 = Dict @((N 3 :% 8) ~ ((N 3 :% 4) % (P 2 :% 1)))
_testSlash_Rat3N4_Rat3P1 = Dict @((N 1 :% 4) ~ ((N 3 :% 4) % (P 3 :% 1)))
_testSlash_Rat3N4_Rat4P1 = Dict @((N 3 :% 16) ~ ((N 3 :% 4) % (P 4 :% 1)))
_testSlash_Rat2N3_Nat1 = Dict @((N 2 :% 3) ~ ((N 2 :% 3) % 1))
_testSlash_Rat2N3_Nat2 = Dict @((N 1 :% 3) ~ ((N 2 :% 3) % 2))
_testSlash_Rat2N3_Nat3 = Dict @((N 2 :% 9) ~ ((N 2 :% 3) % 3))
_testSlash_Rat2N3_Nat4 = Dict @((N 1 :% 6) ~ ((N 2 :% 3) % 4))
_testSlash_Rat2N3_IntN4 = Dict @((P 1 :% 6) ~ ((N 2 :% 3) % (N 4)))
_testSlash_Rat2N3_IntN3 = Dict @((P 2 :% 9) ~ ((N 2 :% 3) % (N 3)))
_testSlash_Rat2N3_IntN2 = Dict @((P 1 :% 3) ~ ((N 2 :% 3) % (N 2)))
_testSlash_Rat2N3_IntN1 = Dict @((P 2 :% 3) ~ ((N 2 :% 3) % (N 1)))
_testSlash_Rat2N3_IntP1 = Dict @((N 2 :% 3) ~ ((N 2 :% 3) % (P 1)))
_testSlash_Rat2N3_IntP2 = Dict @((N 1 :% 3) ~ ((N 2 :% 3) % (P 2)))
_testSlash_Rat2N3_IntP3 = Dict @((N 2 :% 9) ~ ((N 2 :% 3) % (P 3)))
_testSlash_Rat2N3_IntP4 = Dict @((N 1 :% 6) ~ ((N 2 :% 3) % (P 4)))
_testSlash_Rat2N3_Rat4N1 = Dict @((P 1 :% 6) ~ ((N 2 :% 3) % (N 4 :% 1)))
_testSlash_Rat2N3_Rat3N1 = Dict @((P 2 :% 9) ~ ((N 2 :% 3) % (N 3 :% 1)))
_testSlash_Rat2N3_Rat2N1 = Dict @((P 1 :% 3) ~ ((N 2 :% 3) % (N 2 :% 1)))
_testSlash_Rat2N3_Rat3N2 = Dict @((P 4 :% 9) ~ ((N 2 :% 3) % (N 3 :% 2)))
_testSlash_Rat2N3_Rat4N3 = Dict @((P 1 :% 2) ~ ((N 2 :% 3) % (N 4 :% 3)))
_testSlash_Rat2N3_Rat1N1 = Dict @((P 2 :% 3) ~ ((N 2 :% 3) % (N 1 :% 1)))
_testSlash_Rat2N3_Rat3N4 = Dict @((P 8 :% 9) ~ ((N 2 :% 3) % (N 3 :% 4)))
_testSlash_Rat2N3_Rat2N3 = Dict @((P 1 :% 1) ~ ((N 2 :% 3) % (N 2 :% 3)))
_testSlash_Rat2N3_Rat1N2 = Dict @((P 4 :% 3) ~ ((N 2 :% 3) % (N 1 :% 2)))
_testSlash_Rat2N3_Rat1N3 = Dict @((P 2 :% 1) ~ ((N 2 :% 3) % (N 1 :% 3)))
_testSlash_Rat2N3_Rat1N4 = Dict @((P 8 :% 3) ~ ((N 2 :% 3) % (N 1 :% 4)))
_testSlash_Rat2N3_Rat1P4 = Dict @((N 8 :% 3) ~ ((N 2 :% 3) % (P 1 :% 4)))
_testSlash_Rat2N3_Rat1P3 = Dict @((N 2 :% 1) ~ ((N 2 :% 3) % (P 1 :% 3)))
_testSlash_Rat2N3_Rat1P2 = Dict @((N 4 :% 3) ~ ((N 2 :% 3) % (P 1 :% 2)))
_testSlash_Rat2N3_Rat2P3 = Dict @((N 1 :% 1) ~ ((N 2 :% 3) % (P 2 :% 3)))
_testSlash_Rat2N3_Rat3P4 = Dict @((N 8 :% 9) ~ ((N 2 :% 3) % (P 3 :% 4)))
_testSlash_Rat2N3_Rat1P1 = Dict @((N 2 :% 3) ~ ((N 2 :% 3) % (P 1 :% 1)))
_testSlash_Rat2N3_Rat4P3 = Dict @((N 1 :% 2) ~ ((N 2 :% 3) % (P 4 :% 3)))
_testSlash_Rat2N3_Rat3P2 = Dict @((N 4 :% 9) ~ ((N 2 :% 3) % (P 3 :% 2)))
_testSlash_Rat2N3_Rat2P1 = Dict @((N 1 :% 3) ~ ((N 2 :% 3) % (P 2 :% 1)))
_testSlash_Rat2N3_Rat3P1 = Dict @((N 2 :% 9) ~ ((N 2 :% 3) % (P 3 :% 1)))
_testSlash_Rat2N3_Rat4P1 = Dict @((N 1 :% 6) ~ ((N 2 :% 3) % (P 4 :% 1)))
_testSlash_Rat1N2_Nat1 = Dict @((N 1 :% 2) ~ ((N 1 :% 2) % 1))
_testSlash_Rat1N2_Nat2 = Dict @((N 1 :% 4) ~ ((N 1 :% 2) % 2))
_testSlash_Rat1N2_Nat3 = Dict @((N 1 :% 6) ~ ((N 1 :% 2) % 3))
_testSlash_Rat1N2_Nat4 = Dict @((N 1 :% 8) ~ ((N 1 :% 2) % 4))
_testSlash_Rat1N2_IntN4 = Dict @((P 1 :% 8) ~ ((N 1 :% 2) % (N 4)))
_testSlash_Rat1N2_IntN3 = Dict @((P 1 :% 6) ~ ((N 1 :% 2) % (N 3)))
_testSlash_Rat1N2_IntN2 = Dict @((P 1 :% 4) ~ ((N 1 :% 2) % (N 2)))
_testSlash_Rat1N2_IntN1 = Dict @((P 1 :% 2) ~ ((N 1 :% 2) % (N 1)))
_testSlash_Rat1N2_IntP1 = Dict @((N 1 :% 2) ~ ((N 1 :% 2) % (P 1)))
_testSlash_Rat1N2_IntP2 = Dict @((N 1 :% 4) ~ ((N 1 :% 2) % (P 2)))
_testSlash_Rat1N2_IntP3 = Dict @((N 1 :% 6) ~ ((N 1 :% 2) % (P 3)))
_testSlash_Rat1N2_IntP4 = Dict @((N 1 :% 8) ~ ((N 1 :% 2) % (P 4)))
_testSlash_Rat1N2_Rat4N1 = Dict @((P 1 :% 8) ~ ((N 1 :% 2) % (N 4 :% 1)))
_testSlash_Rat1N2_Rat3N1 = Dict @((P 1 :% 6) ~ ((N 1 :% 2) % (N 3 :% 1)))
_testSlash_Rat1N2_Rat2N1 = Dict @((P 1 :% 4) ~ ((N 1 :% 2) % (N 2 :% 1)))
_testSlash_Rat1N2_Rat3N2 = Dict @((P 1 :% 3) ~ ((N 1 :% 2) % (N 3 :% 2)))
_testSlash_Rat1N2_Rat4N3 = Dict @((P 3 :% 8) ~ ((N 1 :% 2) % (N 4 :% 3)))
_testSlash_Rat1N2_Rat1N1 = Dict @((P 1 :% 2) ~ ((N 1 :% 2) % (N 1 :% 1)))
_testSlash_Rat1N2_Rat3N4 = Dict @((P 2 :% 3) ~ ((N 1 :% 2) % (N 3 :% 4)))
_testSlash_Rat1N2_Rat2N3 = Dict @((P 3 :% 4) ~ ((N 1 :% 2) % (N 2 :% 3)))
_testSlash_Rat1N2_Rat1N2 = Dict @((P 1 :% 1) ~ ((N 1 :% 2) % (N 1 :% 2)))
_testSlash_Rat1N2_Rat1N3 = Dict @((P 3 :% 2) ~ ((N 1 :% 2) % (N 1 :% 3)))
_testSlash_Rat1N2_Rat1N4 = Dict @((P 2 :% 1) ~ ((N 1 :% 2) % (N 1 :% 4)))
_testSlash_Rat1N2_Rat1P4 = Dict @((N 2 :% 1) ~ ((N 1 :% 2) % (P 1 :% 4)))
_testSlash_Rat1N2_Rat1P3 = Dict @((N 3 :% 2) ~ ((N 1 :% 2) % (P 1 :% 3)))
_testSlash_Rat1N2_Rat1P2 = Dict @((N 1 :% 1) ~ ((N 1 :% 2) % (P 1 :% 2)))
_testSlash_Rat1N2_Rat2P3 = Dict @((N 3 :% 4) ~ ((N 1 :% 2) % (P 2 :% 3)))
_testSlash_Rat1N2_Rat3P4 = Dict @((N 2 :% 3) ~ ((N 1 :% 2) % (P 3 :% 4)))
_testSlash_Rat1N2_Rat1P1 = Dict @((N 1 :% 2) ~ ((N 1 :% 2) % (P 1 :% 1)))
_testSlash_Rat1N2_Rat4P3 = Dict @((N 3 :% 8) ~ ((N 1 :% 2) % (P 4 :% 3)))
_testSlash_Rat1N2_Rat3P2 = Dict @((N 1 :% 3) ~ ((N 1 :% 2) % (P 3 :% 2)))
_testSlash_Rat1N2_Rat2P1 = Dict @((N 1 :% 4) ~ ((N 1 :% 2) % (P 2 :% 1)))
_testSlash_Rat1N2_Rat3P1 = Dict @((N 1 :% 6) ~ ((N 1 :% 2) % (P 3 :% 1)))
_testSlash_Rat1N2_Rat4P1 = Dict @((N 1 :% 8) ~ ((N 1 :% 2) % (P 4 :% 1)))
_testSlash_Rat1N3_Nat1 = Dict @((N 1 :% 3) ~ ((N 1 :% 3) % 1))
_testSlash_Rat1N3_Nat2 = Dict @((N 1 :% 6) ~ ((N 1 :% 3) % 2))
_testSlash_Rat1N3_Nat3 = Dict @((N 1 :% 9) ~ ((N 1 :% 3) % 3))
_testSlash_Rat1N3_Nat4 = Dict @((N 1 :% 12) ~ ((N 1 :% 3) % 4))
_testSlash_Rat1N3_IntN4 = Dict @((P 1 :% 12) ~ ((N 1 :% 3) % (N 4)))
_testSlash_Rat1N3_IntN3 = Dict @((P 1 :% 9) ~ ((N 1 :% 3) % (N 3)))
_testSlash_Rat1N3_IntN2 = Dict @((P 1 :% 6) ~ ((N 1 :% 3) % (N 2)))
_testSlash_Rat1N3_IntN1 = Dict @((P 1 :% 3) ~ ((N 1 :% 3) % (N 1)))
_testSlash_Rat1N3_IntP1 = Dict @((N 1 :% 3) ~ ((N 1 :% 3) % (P 1)))
_testSlash_Rat1N3_IntP2 = Dict @((N 1 :% 6) ~ ((N 1 :% 3) % (P 2)))
_testSlash_Rat1N3_IntP3 = Dict @((N 1 :% 9) ~ ((N 1 :% 3) % (P 3)))
_testSlash_Rat1N3_IntP4 = Dict @((N 1 :% 12) ~ ((N 1 :% 3) % (P 4)))
_testSlash_Rat1N3_Rat4N1 = Dict @((P 1 :% 12) ~ ((N 1 :% 3) % (N 4 :% 1)))
_testSlash_Rat1N3_Rat3N1 = Dict @((P 1 :% 9) ~ ((N 1 :% 3) % (N 3 :% 1)))
_testSlash_Rat1N3_Rat2N1 = Dict @((P 1 :% 6) ~ ((N 1 :% 3) % (N 2 :% 1)))
_testSlash_Rat1N3_Rat3N2 = Dict @((P 2 :% 9) ~ ((N 1 :% 3) % (N 3 :% 2)))
_testSlash_Rat1N3_Rat4N3 = Dict @((P 1 :% 4) ~ ((N 1 :% 3) % (N 4 :% 3)))
_testSlash_Rat1N3_Rat1N1 = Dict @((P 1 :% 3) ~ ((N 1 :% 3) % (N 1 :% 1)))
_testSlash_Rat1N3_Rat3N4 = Dict @((P 4 :% 9) ~ ((N 1 :% 3) % (N 3 :% 4)))
_testSlash_Rat1N3_Rat2N3 = Dict @((P 1 :% 2) ~ ((N 1 :% 3) % (N 2 :% 3)))
_testSlash_Rat1N3_Rat1N2 = Dict @((P 2 :% 3) ~ ((N 1 :% 3) % (N 1 :% 2)))
_testSlash_Rat1N3_Rat1N3 = Dict @((P 1 :% 1) ~ ((N 1 :% 3) % (N 1 :% 3)))
_testSlash_Rat1N3_Rat1N4 = Dict @((P 4 :% 3) ~ ((N 1 :% 3) % (N 1 :% 4)))
_testSlash_Rat1N3_Rat1P4 = Dict @((N 4 :% 3) ~ ((N 1 :% 3) % (P 1 :% 4)))
_testSlash_Rat1N3_Rat1P3 = Dict @((N 1 :% 1) ~ ((N 1 :% 3) % (P 1 :% 3)))
_testSlash_Rat1N3_Rat1P2 = Dict @((N 2 :% 3) ~ ((N 1 :% 3) % (P 1 :% 2)))
_testSlash_Rat1N3_Rat2P3 = Dict @((N 1 :% 2) ~ ((N 1 :% 3) % (P 2 :% 3)))
_testSlash_Rat1N3_Rat3P4 = Dict @((N 4 :% 9) ~ ((N 1 :% 3) % (P 3 :% 4)))
_testSlash_Rat1N3_Rat1P1 = Dict @((N 1 :% 3) ~ ((N 1 :% 3) % (P 1 :% 1)))
_testSlash_Rat1N3_Rat4P3 = Dict @((N 1 :% 4) ~ ((N 1 :% 3) % (P 4 :% 3)))
_testSlash_Rat1N3_Rat3P2 = Dict @((N 2 :% 9) ~ ((N 1 :% 3) % (P 3 :% 2)))
_testSlash_Rat1N3_Rat2P1 = Dict @((N 1 :% 6) ~ ((N 1 :% 3) % (P 2 :% 1)))
_testSlash_Rat1N3_Rat3P1 = Dict @((N 1 :% 9) ~ ((N 1 :% 3) % (P 3 :% 1)))
_testSlash_Rat1N3_Rat4P1 = Dict @((N 1 :% 12) ~ ((N 1 :% 3) % (P 4 :% 1)))
_testSlash_Rat1N4_Nat1 = Dict @((N 1 :% 4) ~ ((N 1 :% 4) % 1))
_testSlash_Rat1N4_Nat2 = Dict @((N 1 :% 8) ~ ((N 1 :% 4) % 2))
_testSlash_Rat1N4_Nat3 = Dict @((N 1 :% 12) ~ ((N 1 :% 4) % 3))
_testSlash_Rat1N4_Nat4 = Dict @((N 1 :% 16) ~ ((N 1 :% 4) % 4))
_testSlash_Rat1N4_IntN4 = Dict @((P 1 :% 16) ~ ((N 1 :% 4) % (N 4)))
_testSlash_Rat1N4_IntN3 = Dict @((P 1 :% 12) ~ ((N 1 :% 4) % (N 3)))
_testSlash_Rat1N4_IntN2 = Dict @((P 1 :% 8) ~ ((N 1 :% 4) % (N 2)))
_testSlash_Rat1N4_IntN1 = Dict @((P 1 :% 4) ~ ((N 1 :% 4) % (N 1)))
_testSlash_Rat1N4_IntP1 = Dict @((N 1 :% 4) ~ ((N 1 :% 4) % (P 1)))
_testSlash_Rat1N4_IntP2 = Dict @((N 1 :% 8) ~ ((N 1 :% 4) % (P 2)))
_testSlash_Rat1N4_IntP3 = Dict @((N 1 :% 12) ~ ((N 1 :% 4) % (P 3)))
_testSlash_Rat1N4_IntP4 = Dict @((N 1 :% 16) ~ ((N 1 :% 4) % (P 4)))
_testSlash_Rat1N4_Rat4N1 = Dict @((P 1 :% 16) ~ ((N 1 :% 4) % (N 4 :% 1)))
_testSlash_Rat1N4_Rat3N1 = Dict @((P 1 :% 12) ~ ((N 1 :% 4) % (N 3 :% 1)))
_testSlash_Rat1N4_Rat2N1 = Dict @((P 1 :% 8) ~ ((N 1 :% 4) % (N 2 :% 1)))
_testSlash_Rat1N4_Rat3N2 = Dict @((P 1 :% 6) ~ ((N 1 :% 4) % (N 3 :% 2)))
_testSlash_Rat1N4_Rat4N3 = Dict @((P 3 :% 16) ~ ((N 1 :% 4) % (N 4 :% 3)))
_testSlash_Rat1N4_Rat1N1 = Dict @((P 1 :% 4) ~ ((N 1 :% 4) % (N 1 :% 1)))
_testSlash_Rat1N4_Rat3N4 = Dict @((P 1 :% 3) ~ ((N 1 :% 4) % (N 3 :% 4)))
_testSlash_Rat1N4_Rat2N3 = Dict @((P 3 :% 8) ~ ((N 1 :% 4) % (N 2 :% 3)))
_testSlash_Rat1N4_Rat1N2 = Dict @((P 1 :% 2) ~ ((N 1 :% 4) % (N 1 :% 2)))
_testSlash_Rat1N4_Rat1N3 = Dict @((P 3 :% 4) ~ ((N 1 :% 4) % (N 1 :% 3)))
_testSlash_Rat1N4_Rat1N4 = Dict @((P 1 :% 1) ~ ((N 1 :% 4) % (N 1 :% 4)))
_testSlash_Rat1N4_Rat1P4 = Dict @((N 1 :% 1) ~ ((N 1 :% 4) % (P 1 :% 4)))
_testSlash_Rat1N4_Rat1P3 = Dict @((N 3 :% 4) ~ ((N 1 :% 4) % (P 1 :% 3)))
_testSlash_Rat1N4_Rat1P2 = Dict @((N 1 :% 2) ~ ((N 1 :% 4) % (P 1 :% 2)))
_testSlash_Rat1N4_Rat2P3 = Dict @((N 3 :% 8) ~ ((N 1 :% 4) % (P 2 :% 3)))
_testSlash_Rat1N4_Rat3P4 = Dict @((N 1 :% 3) ~ ((N 1 :% 4) % (P 3 :% 4)))
_testSlash_Rat1N4_Rat1P1 = Dict @((N 1 :% 4) ~ ((N 1 :% 4) % (P 1 :% 1)))
_testSlash_Rat1N4_Rat4P3 = Dict @((N 3 :% 16) ~ ((N 1 :% 4) % (P 4 :% 3)))
_testSlash_Rat1N4_Rat3P2 = Dict @((N 1 :% 6) ~ ((N 1 :% 4) % (P 3 :% 2)))
_testSlash_Rat1N4_Rat2P1 = Dict @((N 1 :% 8) ~ ((N 1 :% 4) % (P 2 :% 1)))
_testSlash_Rat1N4_Rat3P1 = Dict @((N 1 :% 12) ~ ((N 1 :% 4) % (P 3 :% 1)))
_testSlash_Rat1N4_Rat4P1 = Dict @((N 1 :% 16) ~ ((N 1 :% 4) % (P 4 :% 1)))
_testSlash_Rat0P1_Nat1 = Dict @((Z :% 1) ~ ((Z :% 1) % 1))
_testSlash_Rat0P1_Nat2 = Dict @((Z :% 1) ~ ((Z :% 1) % 2))
_testSlash_Rat0P1_Nat3 = Dict @((Z :% 1) ~ ((Z :% 1) % 3))
_testSlash_Rat0P1_Nat4 = Dict @((Z :% 1) ~ ((Z :% 1) % 4))
_testSlash_Rat0P1_IntN4 = Dict @((Z :% 1) ~ ((Z :% 1) % (N 4)))
_testSlash_Rat0P1_IntN3 = Dict @((Z :% 1) ~ ((Z :% 1) % (N 3)))
_testSlash_Rat0P1_IntN2 = Dict @((Z :% 1) ~ ((Z :% 1) % (N 2)))
_testSlash_Rat0P1_IntN1 = Dict @((Z :% 1) ~ ((Z :% 1) % (N 1)))
_testSlash_Rat0P1_IntP1 = Dict @((Z :% 1) ~ ((Z :% 1) % (P 1)))
_testSlash_Rat0P1_IntP2 = Dict @((Z :% 1) ~ ((Z :% 1) % (P 2)))
_testSlash_Rat0P1_IntP3 = Dict @((Z :% 1) ~ ((Z :% 1) % (P 3)))
_testSlash_Rat0P1_IntP4 = Dict @((Z :% 1) ~ ((Z :% 1) % (P 4)))
_testSlash_Rat0P1_Rat4N1 = Dict @((Z :% 1) ~ ((Z :% 1) % (N 4 :% 1)))
_testSlash_Rat0P1_Rat3N1 = Dict @((Z :% 1) ~ ((Z :% 1) % (N 3 :% 1)))
_testSlash_Rat0P1_Rat2N1 = Dict @((Z :% 1) ~ ((Z :% 1) % (N 2 :% 1)))
_testSlash_Rat0P1_Rat3N2 = Dict @((Z :% 1) ~ ((Z :% 1) % (N 3 :% 2)))
_testSlash_Rat0P1_Rat4N3 = Dict @((Z :% 1) ~ ((Z :% 1) % (N 4 :% 3)))
_testSlash_Rat0P1_Rat1N1 = Dict @((Z :% 1) ~ ((Z :% 1) % (N 1 :% 1)))
_testSlash_Rat0P1_Rat3N4 = Dict @((Z :% 1) ~ ((Z :% 1) % (N 3 :% 4)))
_testSlash_Rat0P1_Rat2N3 = Dict @((Z :% 1) ~ ((Z :% 1) % (N 2 :% 3)))
_testSlash_Rat0P1_Rat1N2 = Dict @((Z :% 1) ~ ((Z :% 1) % (N 1 :% 2)))
_testSlash_Rat0P1_Rat1N3 = Dict @((Z :% 1) ~ ((Z :% 1) % (N 1 :% 3)))
_testSlash_Rat0P1_Rat1N4 = Dict @((Z :% 1) ~ ((Z :% 1) % (N 1 :% 4)))
_testSlash_Rat0P1_Rat1P4 = Dict @((Z :% 1) ~ ((Z :% 1) % (P 1 :% 4)))
_testSlash_Rat0P1_Rat1P3 = Dict @((Z :% 1) ~ ((Z :% 1) % (P 1 :% 3)))
_testSlash_Rat0P1_Rat1P2 = Dict @((Z :% 1) ~ ((Z :% 1) % (P 1 :% 2)))
_testSlash_Rat0P1_Rat2P3 = Dict @((Z :% 1) ~ ((Z :% 1) % (P 2 :% 3)))
_testSlash_Rat0P1_Rat3P4 = Dict @((Z :% 1) ~ ((Z :% 1) % (P 3 :% 4)))
_testSlash_Rat0P1_Rat1P1 = Dict @((Z :% 1) ~ ((Z :% 1) % (P 1 :% 1)))
_testSlash_Rat0P1_Rat4P3 = Dict @((Z :% 1) ~ ((Z :% 1) % (P 4 :% 3)))
_testSlash_Rat0P1_Rat3P2 = Dict @((Z :% 1) ~ ((Z :% 1) % (P 3 :% 2)))
_testSlash_Rat0P1_Rat2P1 = Dict @((Z :% 1) ~ ((Z :% 1) % (P 2 :% 1)))
_testSlash_Rat0P1_Rat3P1 = Dict @((Z :% 1) ~ ((Z :% 1) % (P 3 :% 1)))
_testSlash_Rat0P1_Rat4P1 = Dict @((Z :% 1) ~ ((Z :% 1) % (P 4 :% 1)))
_testSlash_Rat1P4_Nat1 = Dict @((P 1 :% 4) ~ ((P 1 :% 4) % 1))
_testSlash_Rat1P4_Nat2 = Dict @((P 1 :% 8) ~ ((P 1 :% 4) % 2))
_testSlash_Rat1P4_Nat3 = Dict @((P 1 :% 12) ~ ((P 1 :% 4) % 3))
_testSlash_Rat1P4_Nat4 = Dict @((P 1 :% 16) ~ ((P 1 :% 4) % 4))
_testSlash_Rat1P4_IntN4 = Dict @((N 1 :% 16) ~ ((P 1 :% 4) % (N 4)))
_testSlash_Rat1P4_IntN3 = Dict @((N 1 :% 12) ~ ((P 1 :% 4) % (N 3)))
_testSlash_Rat1P4_IntN2 = Dict @((N 1 :% 8) ~ ((P 1 :% 4) % (N 2)))
_testSlash_Rat1P4_IntN1 = Dict @((N 1 :% 4) ~ ((P 1 :% 4) % (N 1)))
_testSlash_Rat1P4_IntP1 = Dict @((P 1 :% 4) ~ ((P 1 :% 4) % (P 1)))
_testSlash_Rat1P4_IntP2 = Dict @((P 1 :% 8) ~ ((P 1 :% 4) % (P 2)))
_testSlash_Rat1P4_IntP3 = Dict @((P 1 :% 12) ~ ((P 1 :% 4) % (P 3)))
_testSlash_Rat1P4_IntP4 = Dict @((P 1 :% 16) ~ ((P 1 :% 4) % (P 4)))
_testSlash_Rat1P4_Rat4N1 = Dict @((N 1 :% 16) ~ ((P 1 :% 4) % (N 4 :% 1)))
_testSlash_Rat1P4_Rat3N1 = Dict @((N 1 :% 12) ~ ((P 1 :% 4) % (N 3 :% 1)))
_testSlash_Rat1P4_Rat2N1 = Dict @((N 1 :% 8) ~ ((P 1 :% 4) % (N 2 :% 1)))
_testSlash_Rat1P4_Rat3N2 = Dict @((N 1 :% 6) ~ ((P 1 :% 4) % (N 3 :% 2)))
_testSlash_Rat1P4_Rat4N3 = Dict @((N 3 :% 16) ~ ((P 1 :% 4) % (N 4 :% 3)))
_testSlash_Rat1P4_Rat1N1 = Dict @((N 1 :% 4) ~ ((P 1 :% 4) % (N 1 :% 1)))
_testSlash_Rat1P4_Rat3N4 = Dict @((N 1 :% 3) ~ ((P 1 :% 4) % (N 3 :% 4)))
_testSlash_Rat1P4_Rat2N3 = Dict @((N 3 :% 8) ~ ((P 1 :% 4) % (N 2 :% 3)))
_testSlash_Rat1P4_Rat1N2 = Dict @((N 1 :% 2) ~ ((P 1 :% 4) % (N 1 :% 2)))
_testSlash_Rat1P4_Rat1N3 = Dict @((N 3 :% 4) ~ ((P 1 :% 4) % (N 1 :% 3)))
_testSlash_Rat1P4_Rat1N4 = Dict @((N 1 :% 1) ~ ((P 1 :% 4) % (N 1 :% 4)))
_testSlash_Rat1P4_Rat1P4 = Dict @((P 1 :% 1) ~ ((P 1 :% 4) % (P 1 :% 4)))
_testSlash_Rat1P4_Rat1P3 = Dict @((P 3 :% 4) ~ ((P 1 :% 4) % (P 1 :% 3)))
_testSlash_Rat1P4_Rat1P2 = Dict @((P 1 :% 2) ~ ((P 1 :% 4) % (P 1 :% 2)))
_testSlash_Rat1P4_Rat2P3 = Dict @((P 3 :% 8) ~ ((P 1 :% 4) % (P 2 :% 3)))
_testSlash_Rat1P4_Rat3P4 = Dict @((P 1 :% 3) ~ ((P 1 :% 4) % (P 3 :% 4)))
_testSlash_Rat1P4_Rat1P1 = Dict @((P 1 :% 4) ~ ((P 1 :% 4) % (P 1 :% 1)))
_testSlash_Rat1P4_Rat4P3 = Dict @((P 3 :% 16) ~ ((P 1 :% 4) % (P 4 :% 3)))
_testSlash_Rat1P4_Rat3P2 = Dict @((P 1 :% 6) ~ ((P 1 :% 4) % (P 3 :% 2)))
_testSlash_Rat1P4_Rat2P1 = Dict @((P 1 :% 8) ~ ((P 1 :% 4) % (P 2 :% 1)))
_testSlash_Rat1P4_Rat3P1 = Dict @((P 1 :% 12) ~ ((P 1 :% 4) % (P 3 :% 1)))
_testSlash_Rat1P4_Rat4P1 = Dict @((P 1 :% 16) ~ ((P 1 :% 4) % (P 4 :% 1)))
_testSlash_Rat1P3_Nat1 = Dict @((P 1 :% 3) ~ ((P 1 :% 3) % 1))
_testSlash_Rat1P3_Nat2 = Dict @((P 1 :% 6) ~ ((P 1 :% 3) % 2))
_testSlash_Rat1P3_Nat3 = Dict @((P 1 :% 9) ~ ((P 1 :% 3) % 3))
_testSlash_Rat1P3_Nat4 = Dict @((P 1 :% 12) ~ ((P 1 :% 3) % 4))
_testSlash_Rat1P3_IntN4 = Dict @((N 1 :% 12) ~ ((P 1 :% 3) % (N 4)))
_testSlash_Rat1P3_IntN3 = Dict @((N 1 :% 9) ~ ((P 1 :% 3) % (N 3)))
_testSlash_Rat1P3_IntN2 = Dict @((N 1 :% 6) ~ ((P 1 :% 3) % (N 2)))
_testSlash_Rat1P3_IntN1 = Dict @((N 1 :% 3) ~ ((P 1 :% 3) % (N 1)))
_testSlash_Rat1P3_IntP1 = Dict @((P 1 :% 3) ~ ((P 1 :% 3) % (P 1)))
_testSlash_Rat1P3_IntP2 = Dict @((P 1 :% 6) ~ ((P 1 :% 3) % (P 2)))
_testSlash_Rat1P3_IntP3 = Dict @((P 1 :% 9) ~ ((P 1 :% 3) % (P 3)))
_testSlash_Rat1P3_IntP4 = Dict @((P 1 :% 12) ~ ((P 1 :% 3) % (P 4)))
_testSlash_Rat1P3_Rat4N1 = Dict @((N 1 :% 12) ~ ((P 1 :% 3) % (N 4 :% 1)))
_testSlash_Rat1P3_Rat3N1 = Dict @((N 1 :% 9) ~ ((P 1 :% 3) % (N 3 :% 1)))
_testSlash_Rat1P3_Rat2N1 = Dict @((N 1 :% 6) ~ ((P 1 :% 3) % (N 2 :% 1)))
_testSlash_Rat1P3_Rat3N2 = Dict @((N 2 :% 9) ~ ((P 1 :% 3) % (N 3 :% 2)))
_testSlash_Rat1P3_Rat4N3 = Dict @((N 1 :% 4) ~ ((P 1 :% 3) % (N 4 :% 3)))
_testSlash_Rat1P3_Rat1N1 = Dict @((N 1 :% 3) ~ ((P 1 :% 3) % (N 1 :% 1)))
_testSlash_Rat1P3_Rat3N4 = Dict @((N 4 :% 9) ~ ((P 1 :% 3) % (N 3 :% 4)))
_testSlash_Rat1P3_Rat2N3 = Dict @((N 1 :% 2) ~ ((P 1 :% 3) % (N 2 :% 3)))
_testSlash_Rat1P3_Rat1N2 = Dict @((N 2 :% 3) ~ ((P 1 :% 3) % (N 1 :% 2)))
_testSlash_Rat1P3_Rat1N3 = Dict @((N 1 :% 1) ~ ((P 1 :% 3) % (N 1 :% 3)))
_testSlash_Rat1P3_Rat1N4 = Dict @((N 4 :% 3) ~ ((P 1 :% 3) % (N 1 :% 4)))
_testSlash_Rat1P3_Rat1P4 = Dict @((P 4 :% 3) ~ ((P 1 :% 3) % (P 1 :% 4)))
_testSlash_Rat1P3_Rat1P3 = Dict @((P 1 :% 1) ~ ((P 1 :% 3) % (P 1 :% 3)))
_testSlash_Rat1P3_Rat1P2 = Dict @((P 2 :% 3) ~ ((P 1 :% 3) % (P 1 :% 2)))
_testSlash_Rat1P3_Rat2P3 = Dict @((P 1 :% 2) ~ ((P 1 :% 3) % (P 2 :% 3)))
_testSlash_Rat1P3_Rat3P4 = Dict @((P 4 :% 9) ~ ((P 1 :% 3) % (P 3 :% 4)))
_testSlash_Rat1P3_Rat1P1 = Dict @((P 1 :% 3) ~ ((P 1 :% 3) % (P 1 :% 1)))
_testSlash_Rat1P3_Rat4P3 = Dict @((P 1 :% 4) ~ ((P 1 :% 3) % (P 4 :% 3)))
_testSlash_Rat1P3_Rat3P2 = Dict @((P 2 :% 9) ~ ((P 1 :% 3) % (P 3 :% 2)))
_testSlash_Rat1P3_Rat2P1 = Dict @((P 1 :% 6) ~ ((P 1 :% 3) % (P 2 :% 1)))
_testSlash_Rat1P3_Rat3P1 = Dict @((P 1 :% 9) ~ ((P 1 :% 3) % (P 3 :% 1)))
_testSlash_Rat1P3_Rat4P1 = Dict @((P 1 :% 12) ~ ((P 1 :% 3) % (P 4 :% 1)))
_testSlash_Rat1P2_Nat1 = Dict @((P 1 :% 2) ~ ((P 1 :% 2) % 1))
_testSlash_Rat1P2_Nat2 = Dict @((P 1 :% 4) ~ ((P 1 :% 2) % 2))
_testSlash_Rat1P2_Nat3 = Dict @((P 1 :% 6) ~ ((P 1 :% 2) % 3))
_testSlash_Rat1P2_Nat4 = Dict @((P 1 :% 8) ~ ((P 1 :% 2) % 4))
_testSlash_Rat1P2_IntN4 = Dict @((N 1 :% 8) ~ ((P 1 :% 2) % (N 4)))
_testSlash_Rat1P2_IntN3 = Dict @((N 1 :% 6) ~ ((P 1 :% 2) % (N 3)))
_testSlash_Rat1P2_IntN2 = Dict @((N 1 :% 4) ~ ((P 1 :% 2) % (N 2)))
_testSlash_Rat1P2_IntN1 = Dict @((N 1 :% 2) ~ ((P 1 :% 2) % (N 1)))
_testSlash_Rat1P2_IntP1 = Dict @((P 1 :% 2) ~ ((P 1 :% 2) % (P 1)))
_testSlash_Rat1P2_IntP2 = Dict @((P 1 :% 4) ~ ((P 1 :% 2) % (P 2)))
_testSlash_Rat1P2_IntP3 = Dict @((P 1 :% 6) ~ ((P 1 :% 2) % (P 3)))
_testSlash_Rat1P2_IntP4 = Dict @((P 1 :% 8) ~ ((P 1 :% 2) % (P 4)))
_testSlash_Rat1P2_Rat4N1 = Dict @((N 1 :% 8) ~ ((P 1 :% 2) % (N 4 :% 1)))
_testSlash_Rat1P2_Rat3N1 = Dict @((N 1 :% 6) ~ ((P 1 :% 2) % (N 3 :% 1)))
_testSlash_Rat1P2_Rat2N1 = Dict @((N 1 :% 4) ~ ((P 1 :% 2) % (N 2 :% 1)))
_testSlash_Rat1P2_Rat3N2 = Dict @((N 1 :% 3) ~ ((P 1 :% 2) % (N 3 :% 2)))
_testSlash_Rat1P2_Rat4N3 = Dict @((N 3 :% 8) ~ ((P 1 :% 2) % (N 4 :% 3)))
_testSlash_Rat1P2_Rat1N1 = Dict @((N 1 :% 2) ~ ((P 1 :% 2) % (N 1 :% 1)))
_testSlash_Rat1P2_Rat3N4 = Dict @((N 2 :% 3) ~ ((P 1 :% 2) % (N 3 :% 4)))
_testSlash_Rat1P2_Rat2N3 = Dict @((N 3 :% 4) ~ ((P 1 :% 2) % (N 2 :% 3)))
_testSlash_Rat1P2_Rat1N2 = Dict @((N 1 :% 1) ~ ((P 1 :% 2) % (N 1 :% 2)))
_testSlash_Rat1P2_Rat1N3 = Dict @((N 3 :% 2) ~ ((P 1 :% 2) % (N 1 :% 3)))
_testSlash_Rat1P2_Rat1N4 = Dict @((N 2 :% 1) ~ ((P 1 :% 2) % (N 1 :% 4)))
_testSlash_Rat1P2_Rat1P4 = Dict @((P 2 :% 1) ~ ((P 1 :% 2) % (P 1 :% 4)))
_testSlash_Rat1P2_Rat1P3 = Dict @((P 3 :% 2) ~ ((P 1 :% 2) % (P 1 :% 3)))
_testSlash_Rat1P2_Rat1P2 = Dict @((P 1 :% 1) ~ ((P 1 :% 2) % (P 1 :% 2)))
_testSlash_Rat1P2_Rat2P3 = Dict @((P 3 :% 4) ~ ((P 1 :% 2) % (P 2 :% 3)))
_testSlash_Rat1P2_Rat3P4 = Dict @((P 2 :% 3) ~ ((P 1 :% 2) % (P 3 :% 4)))
_testSlash_Rat1P2_Rat1P1 = Dict @((P 1 :% 2) ~ ((P 1 :% 2) % (P 1 :% 1)))
_testSlash_Rat1P2_Rat4P3 = Dict @((P 3 :% 8) ~ ((P 1 :% 2) % (P 4 :% 3)))
_testSlash_Rat1P2_Rat3P2 = Dict @((P 1 :% 3) ~ ((P 1 :% 2) % (P 3 :% 2)))
_testSlash_Rat1P2_Rat2P1 = Dict @((P 1 :% 4) ~ ((P 1 :% 2) % (P 2 :% 1)))
_testSlash_Rat1P2_Rat3P1 = Dict @((P 1 :% 6) ~ ((P 1 :% 2) % (P 3 :% 1)))
_testSlash_Rat1P2_Rat4P1 = Dict @((P 1 :% 8) ~ ((P 1 :% 2) % (P 4 :% 1)))
_testSlash_Rat2P3_Nat1 = Dict @((P 2 :% 3) ~ ((P 2 :% 3) % 1))
_testSlash_Rat2P3_Nat2 = Dict @((P 1 :% 3) ~ ((P 2 :% 3) % 2))
_testSlash_Rat2P3_Nat3 = Dict @((P 2 :% 9) ~ ((P 2 :% 3) % 3))
_testSlash_Rat2P3_Nat4 = Dict @((P 1 :% 6) ~ ((P 2 :% 3) % 4))
_testSlash_Rat2P3_IntN4 = Dict @((N 1 :% 6) ~ ((P 2 :% 3) % (N 4)))
_testSlash_Rat2P3_IntN3 = Dict @((N 2 :% 9) ~ ((P 2 :% 3) % (N 3)))
_testSlash_Rat2P3_IntN2 = Dict @((N 1 :% 3) ~ ((P 2 :% 3) % (N 2)))
_testSlash_Rat2P3_IntN1 = Dict @((N 2 :% 3) ~ ((P 2 :% 3) % (N 1)))
_testSlash_Rat2P3_IntP1 = Dict @((P 2 :% 3) ~ ((P 2 :% 3) % (P 1)))
_testSlash_Rat2P3_IntP2 = Dict @((P 1 :% 3) ~ ((P 2 :% 3) % (P 2)))
_testSlash_Rat2P3_IntP3 = Dict @((P 2 :% 9) ~ ((P 2 :% 3) % (P 3)))
_testSlash_Rat2P3_IntP4 = Dict @((P 1 :% 6) ~ ((P 2 :% 3) % (P 4)))
_testSlash_Rat2P3_Rat4N1 = Dict @((N 1 :% 6) ~ ((P 2 :% 3) % (N 4 :% 1)))
_testSlash_Rat2P3_Rat3N1 = Dict @((N 2 :% 9) ~ ((P 2 :% 3) % (N 3 :% 1)))
_testSlash_Rat2P3_Rat2N1 = Dict @((N 1 :% 3) ~ ((P 2 :% 3) % (N 2 :% 1)))
_testSlash_Rat2P3_Rat3N2 = Dict @((N 4 :% 9) ~ ((P 2 :% 3) % (N 3 :% 2)))
_testSlash_Rat2P3_Rat4N3 = Dict @((N 1 :% 2) ~ ((P 2 :% 3) % (N 4 :% 3)))
_testSlash_Rat2P3_Rat1N1 = Dict @((N 2 :% 3) ~ ((P 2 :% 3) % (N 1 :% 1)))
_testSlash_Rat2P3_Rat3N4 = Dict @((N 8 :% 9) ~ ((P 2 :% 3) % (N 3 :% 4)))
_testSlash_Rat2P3_Rat2N3 = Dict @((N 1 :% 1) ~ ((P 2 :% 3) % (N 2 :% 3)))
_testSlash_Rat2P3_Rat1N2 = Dict @((N 4 :% 3) ~ ((P 2 :% 3) % (N 1 :% 2)))
_testSlash_Rat2P3_Rat1N3 = Dict @((N 2 :% 1) ~ ((P 2 :% 3) % (N 1 :% 3)))
_testSlash_Rat2P3_Rat1N4 = Dict @((N 8 :% 3) ~ ((P 2 :% 3) % (N 1 :% 4)))
_testSlash_Rat2P3_Rat1P4 = Dict @((P 8 :% 3) ~ ((P 2 :% 3) % (P 1 :% 4)))
_testSlash_Rat2P3_Rat1P3 = Dict @((P 2 :% 1) ~ ((P 2 :% 3) % (P 1 :% 3)))
_testSlash_Rat2P3_Rat1P2 = Dict @((P 4 :% 3) ~ ((P 2 :% 3) % (P 1 :% 2)))
_testSlash_Rat2P3_Rat2P3 = Dict @((P 1 :% 1) ~ ((P 2 :% 3) % (P 2 :% 3)))
_testSlash_Rat2P3_Rat3P4 = Dict @((P 8 :% 9) ~ ((P 2 :% 3) % (P 3 :% 4)))
_testSlash_Rat2P3_Rat1P1 = Dict @((P 2 :% 3) ~ ((P 2 :% 3) % (P 1 :% 1)))
_testSlash_Rat2P3_Rat4P3 = Dict @((P 1 :% 2) ~ ((P 2 :% 3) % (P 4 :% 3)))
_testSlash_Rat2P3_Rat3P2 = Dict @((P 4 :% 9) ~ ((P 2 :% 3) % (P 3 :% 2)))
_testSlash_Rat2P3_Rat2P1 = Dict @((P 1 :% 3) ~ ((P 2 :% 3) % (P 2 :% 1)))
_testSlash_Rat2P3_Rat3P1 = Dict @((P 2 :% 9) ~ ((P 2 :% 3) % (P 3 :% 1)))
_testSlash_Rat2P3_Rat4P1 = Dict @((P 1 :% 6) ~ ((P 2 :% 3) % (P 4 :% 1)))
_testSlash_Rat3P4_Nat1 = Dict @((P 3 :% 4) ~ ((P 3 :% 4) % 1))
_testSlash_Rat3P4_Nat2 = Dict @((P 3 :% 8) ~ ((P 3 :% 4) % 2))
_testSlash_Rat3P4_Nat3 = Dict @((P 1 :% 4) ~ ((P 3 :% 4) % 3))
_testSlash_Rat3P4_Nat4 = Dict @((P 3 :% 16) ~ ((P 3 :% 4) % 4))
_testSlash_Rat3P4_IntN4 = Dict @((N 3 :% 16) ~ ((P 3 :% 4) % (N 4)))
_testSlash_Rat3P4_IntN3 = Dict @((N 1 :% 4) ~ ((P 3 :% 4) % (N 3)))
_testSlash_Rat3P4_IntN2 = Dict @((N 3 :% 8) ~ ((P 3 :% 4) % (N 2)))
_testSlash_Rat3P4_IntN1 = Dict @((N 3 :% 4) ~ ((P 3 :% 4) % (N 1)))
_testSlash_Rat3P4_IntP1 = Dict @((P 3 :% 4) ~ ((P 3 :% 4) % (P 1)))
_testSlash_Rat3P4_IntP2 = Dict @((P 3 :% 8) ~ ((P 3 :% 4) % (P 2)))
_testSlash_Rat3P4_IntP3 = Dict @((P 1 :% 4) ~ ((P 3 :% 4) % (P 3)))
_testSlash_Rat3P4_IntP4 = Dict @((P 3 :% 16) ~ ((P 3 :% 4) % (P 4)))
_testSlash_Rat3P4_Rat4N1 = Dict @((N 3 :% 16) ~ ((P 3 :% 4) % (N 4 :% 1)))
_testSlash_Rat3P4_Rat3N1 = Dict @((N 1 :% 4) ~ ((P 3 :% 4) % (N 3 :% 1)))
_testSlash_Rat3P4_Rat2N1 = Dict @((N 3 :% 8) ~ ((P 3 :% 4) % (N 2 :% 1)))
_testSlash_Rat3P4_Rat3N2 = Dict @((N 1 :% 2) ~ ((P 3 :% 4) % (N 3 :% 2)))
_testSlash_Rat3P4_Rat4N3 = Dict @((N 9 :% 16) ~ ((P 3 :% 4) % (N 4 :% 3)))
_testSlash_Rat3P4_Rat1N1 = Dict @((N 3 :% 4) ~ ((P 3 :% 4) % (N 1 :% 1)))
_testSlash_Rat3P4_Rat3N4 = Dict @((N 1 :% 1) ~ ((P 3 :% 4) % (N 3 :% 4)))
_testSlash_Rat3P4_Rat2N3 = Dict @((N 9 :% 8) ~ ((P 3 :% 4) % (N 2 :% 3)))
_testSlash_Rat3P4_Rat1N2 = Dict @((N 3 :% 2) ~ ((P 3 :% 4) % (N 1 :% 2)))
_testSlash_Rat3P4_Rat1N3 = Dict @((N 9 :% 4) ~ ((P 3 :% 4) % (N 1 :% 3)))
_testSlash_Rat3P4_Rat1N4 = Dict @((N 3 :% 1) ~ ((P 3 :% 4) % (N 1 :% 4)))
_testSlash_Rat3P4_Rat1P4 = Dict @((P 3 :% 1) ~ ((P 3 :% 4) % (P 1 :% 4)))
_testSlash_Rat3P4_Rat1P3 = Dict @((P 9 :% 4) ~ ((P 3 :% 4) % (P 1 :% 3)))
_testSlash_Rat3P4_Rat1P2 = Dict @((P 3 :% 2) ~ ((P 3 :% 4) % (P 1 :% 2)))
_testSlash_Rat3P4_Rat2P3 = Dict @((P 9 :% 8) ~ ((P 3 :% 4) % (P 2 :% 3)))
_testSlash_Rat3P4_Rat3P4 = Dict @((P 1 :% 1) ~ ((P 3 :% 4) % (P 3 :% 4)))
_testSlash_Rat3P4_Rat1P1 = Dict @((P 3 :% 4) ~ ((P 3 :% 4) % (P 1 :% 1)))
_testSlash_Rat3P4_Rat4P3 = Dict @((P 9 :% 16) ~ ((P 3 :% 4) % (P 4 :% 3)))
_testSlash_Rat3P4_Rat3P2 = Dict @((P 1 :% 2) ~ ((P 3 :% 4) % (P 3 :% 2)))
_testSlash_Rat3P4_Rat2P1 = Dict @((P 3 :% 8) ~ ((P 3 :% 4) % (P 2 :% 1)))
_testSlash_Rat3P4_Rat3P1 = Dict @((P 1 :% 4) ~ ((P 3 :% 4) % (P 3 :% 1)))
_testSlash_Rat3P4_Rat4P1 = Dict @((P 3 :% 16) ~ ((P 3 :% 4) % (P 4 :% 1)))
_testSlash_Rat1P1_Nat1 = Dict @((P 1 :% 1) ~ ((P 1 :% 1) % 1))
_testSlash_Rat1P1_Nat2 = Dict @((P 1 :% 2) ~ ((P 1 :% 1) % 2))
_testSlash_Rat1P1_Nat3 = Dict @((P 1 :% 3) ~ ((P 1 :% 1) % 3))
_testSlash_Rat1P1_Nat4 = Dict @((P 1 :% 4) ~ ((P 1 :% 1) % 4))
_testSlash_Rat1P1_IntN4 = Dict @((N 1 :% 4) ~ ((P 1 :% 1) % (N 4)))
_testSlash_Rat1P1_IntN3 = Dict @((N 1 :% 3) ~ ((P 1 :% 1) % (N 3)))
_testSlash_Rat1P1_IntN2 = Dict @((N 1 :% 2) ~ ((P 1 :% 1) % (N 2)))
_testSlash_Rat1P1_IntN1 = Dict @((N 1 :% 1) ~ ((P 1 :% 1) % (N 1)))
_testSlash_Rat1P1_IntP1 = Dict @((P 1 :% 1) ~ ((P 1 :% 1) % (P 1)))
_testSlash_Rat1P1_IntP2 = Dict @((P 1 :% 2) ~ ((P 1 :% 1) % (P 2)))
_testSlash_Rat1P1_IntP3 = Dict @((P 1 :% 3) ~ ((P 1 :% 1) % (P 3)))
_testSlash_Rat1P1_IntP4 = Dict @((P 1 :% 4) ~ ((P 1 :% 1) % (P 4)))
_testSlash_Rat1P1_Rat4N1 = Dict @((N 1 :% 4) ~ ((P 1 :% 1) % (N 4 :% 1)))
_testSlash_Rat1P1_Rat3N1 = Dict @((N 1 :% 3) ~ ((P 1 :% 1) % (N 3 :% 1)))
_testSlash_Rat1P1_Rat2N1 = Dict @((N 1 :% 2) ~ ((P 1 :% 1) % (N 2 :% 1)))
_testSlash_Rat1P1_Rat3N2 = Dict @((N 2 :% 3) ~ ((P 1 :% 1) % (N 3 :% 2)))
_testSlash_Rat1P1_Rat4N3 = Dict @((N 3 :% 4) ~ ((P 1 :% 1) % (N 4 :% 3)))
_testSlash_Rat1P1_Rat1N1 = Dict @((N 1 :% 1) ~ ((P 1 :% 1) % (N 1 :% 1)))
_testSlash_Rat1P1_Rat3N4 = Dict @((N 4 :% 3) ~ ((P 1 :% 1) % (N 3 :% 4)))
_testSlash_Rat1P1_Rat2N3 = Dict @((N 3 :% 2) ~ ((P 1 :% 1) % (N 2 :% 3)))
_testSlash_Rat1P1_Rat1N2 = Dict @((N 2 :% 1) ~ ((P 1 :% 1) % (N 1 :% 2)))
_testSlash_Rat1P1_Rat1N3 = Dict @((N 3 :% 1) ~ ((P 1 :% 1) % (N 1 :% 3)))
_testSlash_Rat1P1_Rat1N4 = Dict @((N 4 :% 1) ~ ((P 1 :% 1) % (N 1 :% 4)))
_testSlash_Rat1P1_Rat1P4 = Dict @((P 4 :% 1) ~ ((P 1 :% 1) % (P 1 :% 4)))
_testSlash_Rat1P1_Rat1P3 = Dict @((P 3 :% 1) ~ ((P 1 :% 1) % (P 1 :% 3)))
_testSlash_Rat1P1_Rat1P2 = Dict @((P 2 :% 1) ~ ((P 1 :% 1) % (P 1 :% 2)))
_testSlash_Rat1P1_Rat2P3 = Dict @((P 3 :% 2) ~ ((P 1 :% 1) % (P 2 :% 3)))
_testSlash_Rat1P1_Rat3P4 = Dict @((P 4 :% 3) ~ ((P 1 :% 1) % (P 3 :% 4)))
_testSlash_Rat1P1_Rat1P1 = Dict @((P 1 :% 1) ~ ((P 1 :% 1) % (P 1 :% 1)))
_testSlash_Rat1P1_Rat4P3 = Dict @((P 3 :% 4) ~ ((P 1 :% 1) % (P 4 :% 3)))
_testSlash_Rat1P1_Rat3P2 = Dict @((P 2 :% 3) ~ ((P 1 :% 1) % (P 3 :% 2)))
_testSlash_Rat1P1_Rat2P1 = Dict @((P 1 :% 2) ~ ((P 1 :% 1) % (P 2 :% 1)))
_testSlash_Rat1P1_Rat3P1 = Dict @((P 1 :% 3) ~ ((P 1 :% 1) % (P 3 :% 1)))
_testSlash_Rat1P1_Rat4P1 = Dict @((P 1 :% 4) ~ ((P 1 :% 1) % (P 4 :% 1)))
_testSlash_Rat4P3_Nat1 = Dict @((P 4 :% 3) ~ ((P 4 :% 3) % 1))
_testSlash_Rat4P3_Nat2 = Dict @((P 2 :% 3) ~ ((P 4 :% 3) % 2))
_testSlash_Rat4P3_Nat3 = Dict @((P 4 :% 9) ~ ((P 4 :% 3) % 3))
_testSlash_Rat4P3_Nat4 = Dict @((P 1 :% 3) ~ ((P 4 :% 3) % 4))
_testSlash_Rat4P3_IntN4 = Dict @((N 1 :% 3) ~ ((P 4 :% 3) % (N 4)))
_testSlash_Rat4P3_IntN3 = Dict @((N 4 :% 9) ~ ((P 4 :% 3) % (N 3)))
_testSlash_Rat4P3_IntN2 = Dict @((N 2 :% 3) ~ ((P 4 :% 3) % (N 2)))
_testSlash_Rat4P3_IntN1 = Dict @((N 4 :% 3) ~ ((P 4 :% 3) % (N 1)))
_testSlash_Rat4P3_IntP1 = Dict @((P 4 :% 3) ~ ((P 4 :% 3) % (P 1)))
_testSlash_Rat4P3_IntP2 = Dict @((P 2 :% 3) ~ ((P 4 :% 3) % (P 2)))
_testSlash_Rat4P3_IntP3 = Dict @((P 4 :% 9) ~ ((P 4 :% 3) % (P 3)))
_testSlash_Rat4P3_IntP4 = Dict @((P 1 :% 3) ~ ((P 4 :% 3) % (P 4)))
_testSlash_Rat4P3_Rat4N1 = Dict @((N 1 :% 3) ~ ((P 4 :% 3) % (N 4 :% 1)))
_testSlash_Rat4P3_Rat3N1 = Dict @((N 4 :% 9) ~ ((P 4 :% 3) % (N 3 :% 1)))
_testSlash_Rat4P3_Rat2N1 = Dict @((N 2 :% 3) ~ ((P 4 :% 3) % (N 2 :% 1)))
_testSlash_Rat4P3_Rat3N2 = Dict @((N 8 :% 9) ~ ((P 4 :% 3) % (N 3 :% 2)))
_testSlash_Rat4P3_Rat4N3 = Dict @((N 1 :% 1) ~ ((P 4 :% 3) % (N 4 :% 3)))
_testSlash_Rat4P3_Rat1N1 = Dict @((N 4 :% 3) ~ ((P 4 :% 3) % (N 1 :% 1)))
_testSlash_Rat4P3_Rat3N4 = Dict @((N 16 :% 9) ~ ((P 4 :% 3) % (N 3 :% 4)))
_testSlash_Rat4P3_Rat2N3 = Dict @((N 2 :% 1) ~ ((P 4 :% 3) % (N 2 :% 3)))
_testSlash_Rat4P3_Rat1N2 = Dict @((N 8 :% 3) ~ ((P 4 :% 3) % (N 1 :% 2)))
_testSlash_Rat4P3_Rat1N3 = Dict @((N 4 :% 1) ~ ((P 4 :% 3) % (N 1 :% 3)))
_testSlash_Rat4P3_Rat1N4 = Dict @((N 16 :% 3) ~ ((P 4 :% 3) % (N 1 :% 4)))
_testSlash_Rat4P3_Rat1P4 = Dict @((P 16 :% 3) ~ ((P 4 :% 3) % (P 1 :% 4)))
_testSlash_Rat4P3_Rat1P3 = Dict @((P 4 :% 1) ~ ((P 4 :% 3) % (P 1 :% 3)))
_testSlash_Rat4P3_Rat1P2 = Dict @((P 8 :% 3) ~ ((P 4 :% 3) % (P 1 :% 2)))
_testSlash_Rat4P3_Rat2P3 = Dict @((P 2 :% 1) ~ ((P 4 :% 3) % (P 2 :% 3)))
_testSlash_Rat4P3_Rat3P4 = Dict @((P 16 :% 9) ~ ((P 4 :% 3) % (P 3 :% 4)))
_testSlash_Rat4P3_Rat1P1 = Dict @((P 4 :% 3) ~ ((P 4 :% 3) % (P 1 :% 1)))
_testSlash_Rat4P3_Rat4P3 = Dict @((P 1 :% 1) ~ ((P 4 :% 3) % (P 4 :% 3)))
_testSlash_Rat4P3_Rat3P2 = Dict @((P 8 :% 9) ~ ((P 4 :% 3) % (P 3 :% 2)))
_testSlash_Rat4P3_Rat2P1 = Dict @((P 2 :% 3) ~ ((P 4 :% 3) % (P 2 :% 1)))
_testSlash_Rat4P3_Rat3P1 = Dict @((P 4 :% 9) ~ ((P 4 :% 3) % (P 3 :% 1)))
_testSlash_Rat4P3_Rat4P1 = Dict @((P 1 :% 3) ~ ((P 4 :% 3) % (P 4 :% 1)))
_testSlash_Rat3P2_Nat1 = Dict @((P 3 :% 2) ~ ((P 3 :% 2) % 1))
_testSlash_Rat3P2_Nat2 = Dict @((P 3 :% 4) ~ ((P 3 :% 2) % 2))
_testSlash_Rat3P2_Nat3 = Dict @((P 1 :% 2) ~ ((P 3 :% 2) % 3))
_testSlash_Rat3P2_Nat4 = Dict @((P 3 :% 8) ~ ((P 3 :% 2) % 4))
_testSlash_Rat3P2_IntN4 = Dict @((N 3 :% 8) ~ ((P 3 :% 2) % (N 4)))
_testSlash_Rat3P2_IntN3 = Dict @((N 1 :% 2) ~ ((P 3 :% 2) % (N 3)))
_testSlash_Rat3P2_IntN2 = Dict @((N 3 :% 4) ~ ((P 3 :% 2) % (N 2)))
_testSlash_Rat3P2_IntN1 = Dict @((N 3 :% 2) ~ ((P 3 :% 2) % (N 1)))
_testSlash_Rat3P2_IntP1 = Dict @((P 3 :% 2) ~ ((P 3 :% 2) % (P 1)))
_testSlash_Rat3P2_IntP2 = Dict @((P 3 :% 4) ~ ((P 3 :% 2) % (P 2)))
_testSlash_Rat3P2_IntP3 = Dict @((P 1 :% 2) ~ ((P 3 :% 2) % (P 3)))
_testSlash_Rat3P2_IntP4 = Dict @((P 3 :% 8) ~ ((P 3 :% 2) % (P 4)))
_testSlash_Rat3P2_Rat4N1 = Dict @((N 3 :% 8) ~ ((P 3 :% 2) % (N 4 :% 1)))
_testSlash_Rat3P2_Rat3N1 = Dict @((N 1 :% 2) ~ ((P 3 :% 2) % (N 3 :% 1)))
_testSlash_Rat3P2_Rat2N1 = Dict @((N 3 :% 4) ~ ((P 3 :% 2) % (N 2 :% 1)))
_testSlash_Rat3P2_Rat3N2 = Dict @((N 1 :% 1) ~ ((P 3 :% 2) % (N 3 :% 2)))
_testSlash_Rat3P2_Rat4N3 = Dict @((N 9 :% 8) ~ ((P 3 :% 2) % (N 4 :% 3)))
_testSlash_Rat3P2_Rat1N1 = Dict @((N 3 :% 2) ~ ((P 3 :% 2) % (N 1 :% 1)))
_testSlash_Rat3P2_Rat3N4 = Dict @((N 2 :% 1) ~ ((P 3 :% 2) % (N 3 :% 4)))
_testSlash_Rat3P2_Rat2N3 = Dict @((N 9 :% 4) ~ ((P 3 :% 2) % (N 2 :% 3)))
_testSlash_Rat3P2_Rat1N2 = Dict @((N 3 :% 1) ~ ((P 3 :% 2) % (N 1 :% 2)))
_testSlash_Rat3P2_Rat1N3 = Dict @((N 9 :% 2) ~ ((P 3 :% 2) % (N 1 :% 3)))
_testSlash_Rat3P2_Rat1N4 = Dict @((N 6 :% 1) ~ ((P 3 :% 2) % (N 1 :% 4)))
_testSlash_Rat3P2_Rat1P4 = Dict @((P 6 :% 1) ~ ((P 3 :% 2) % (P 1 :% 4)))
_testSlash_Rat3P2_Rat1P3 = Dict @((P 9 :% 2) ~ ((P 3 :% 2) % (P 1 :% 3)))
_testSlash_Rat3P2_Rat1P2 = Dict @((P 3 :% 1) ~ ((P 3 :% 2) % (P 1 :% 2)))
_testSlash_Rat3P2_Rat2P3 = Dict @((P 9 :% 4) ~ ((P 3 :% 2) % (P 2 :% 3)))
_testSlash_Rat3P2_Rat3P4 = Dict @((P 2 :% 1) ~ ((P 3 :% 2) % (P 3 :% 4)))
_testSlash_Rat3P2_Rat1P1 = Dict @((P 3 :% 2) ~ ((P 3 :% 2) % (P 1 :% 1)))
_testSlash_Rat3P2_Rat4P3 = Dict @((P 9 :% 8) ~ ((P 3 :% 2) % (P 4 :% 3)))
_testSlash_Rat3P2_Rat3P2 = Dict @((P 1 :% 1) ~ ((P 3 :% 2) % (P 3 :% 2)))
_testSlash_Rat3P2_Rat2P1 = Dict @((P 3 :% 4) ~ ((P 3 :% 2) % (P 2 :% 1)))
_testSlash_Rat3P2_Rat3P1 = Dict @((P 1 :% 2) ~ ((P 3 :% 2) % (P 3 :% 1)))
_testSlash_Rat3P2_Rat4P1 = Dict @((P 3 :% 8) ~ ((P 3 :% 2) % (P 4 :% 1)))
_testSlash_Rat2P1_Nat1 = Dict @((P 2 :% 1) ~ ((P 2 :% 1) % 1))
_testSlash_Rat2P1_Nat2 = Dict @((P 1 :% 1) ~ ((P 2 :% 1) % 2))
_testSlash_Rat2P1_Nat3 = Dict @((P 2 :% 3) ~ ((P 2 :% 1) % 3))
_testSlash_Rat2P1_Nat4 = Dict @((P 1 :% 2) ~ ((P 2 :% 1) % 4))
_testSlash_Rat2P1_IntN4 = Dict @((N 1 :% 2) ~ ((P 2 :% 1) % (N 4)))
_testSlash_Rat2P1_IntN3 = Dict @((N 2 :% 3) ~ ((P 2 :% 1) % (N 3)))
_testSlash_Rat2P1_IntN2 = Dict @((N 1 :% 1) ~ ((P 2 :% 1) % (N 2)))
_testSlash_Rat2P1_IntN1 = Dict @((N 2 :% 1) ~ ((P 2 :% 1) % (N 1)))
_testSlash_Rat2P1_IntP1 = Dict @((P 2 :% 1) ~ ((P 2 :% 1) % (P 1)))
_testSlash_Rat2P1_IntP2 = Dict @((P 1 :% 1) ~ ((P 2 :% 1) % (P 2)))
_testSlash_Rat2P1_IntP3 = Dict @((P 2 :% 3) ~ ((P 2 :% 1) % (P 3)))
_testSlash_Rat2P1_IntP4 = Dict @((P 1 :% 2) ~ ((P 2 :% 1) % (P 4)))
_testSlash_Rat2P1_Rat4N1 = Dict @((N 1 :% 2) ~ ((P 2 :% 1) % (N 4 :% 1)))
_testSlash_Rat2P1_Rat3N1 = Dict @((N 2 :% 3) ~ ((P 2 :% 1) % (N 3 :% 1)))
_testSlash_Rat2P1_Rat2N1 = Dict @((N 1 :% 1) ~ ((P 2 :% 1) % (N 2 :% 1)))
_testSlash_Rat2P1_Rat3N2 = Dict @((N 4 :% 3) ~ ((P 2 :% 1) % (N 3 :% 2)))
_testSlash_Rat2P1_Rat4N3 = Dict @((N 3 :% 2) ~ ((P 2 :% 1) % (N 4 :% 3)))
_testSlash_Rat2P1_Rat1N1 = Dict @((N 2 :% 1) ~ ((P 2 :% 1) % (N 1 :% 1)))
_testSlash_Rat2P1_Rat3N4 = Dict @((N 8 :% 3) ~ ((P 2 :% 1) % (N 3 :% 4)))
_testSlash_Rat2P1_Rat2N3 = Dict @((N 3 :% 1) ~ ((P 2 :% 1) % (N 2 :% 3)))
_testSlash_Rat2P1_Rat1N2 = Dict @((N 4 :% 1) ~ ((P 2 :% 1) % (N 1 :% 2)))
_testSlash_Rat2P1_Rat1N3 = Dict @((N 6 :% 1) ~ ((P 2 :% 1) % (N 1 :% 3)))
_testSlash_Rat2P1_Rat1N4 = Dict @((N 8 :% 1) ~ ((P 2 :% 1) % (N 1 :% 4)))
_testSlash_Rat2P1_Rat1P4 = Dict @((P 8 :% 1) ~ ((P 2 :% 1) % (P 1 :% 4)))
_testSlash_Rat2P1_Rat1P3 = Dict @((P 6 :% 1) ~ ((P 2 :% 1) % (P 1 :% 3)))
_testSlash_Rat2P1_Rat1P2 = Dict @((P 4 :% 1) ~ ((P 2 :% 1) % (P 1 :% 2)))
_testSlash_Rat2P1_Rat2P3 = Dict @((P 3 :% 1) ~ ((P 2 :% 1) % (P 2 :% 3)))
_testSlash_Rat2P1_Rat3P4 = Dict @((P 8 :% 3) ~ ((P 2 :% 1) % (P 3 :% 4)))
_testSlash_Rat2P1_Rat1P1 = Dict @((P 2 :% 1) ~ ((P 2 :% 1) % (P 1 :% 1)))
_testSlash_Rat2P1_Rat4P3 = Dict @((P 3 :% 2) ~ ((P 2 :% 1) % (P 4 :% 3)))
_testSlash_Rat2P1_Rat3P2 = Dict @((P 4 :% 3) ~ ((P 2 :% 1) % (P 3 :% 2)))
_testSlash_Rat2P1_Rat2P1 = Dict @((P 1 :% 1) ~ ((P 2 :% 1) % (P 2 :% 1)))
_testSlash_Rat2P1_Rat3P1 = Dict @((P 2 :% 3) ~ ((P 2 :% 1) % (P 3 :% 1)))
_testSlash_Rat2P1_Rat4P1 = Dict @((P 1 :% 2) ~ ((P 2 :% 1) % (P 4 :% 1)))
_testSlash_Rat3P1_Nat1 = Dict @((P 3 :% 1) ~ ((P 3 :% 1) % 1))
_testSlash_Rat3P1_Nat2 = Dict @((P 3 :% 2) ~ ((P 3 :% 1) % 2))
_testSlash_Rat3P1_Nat3 = Dict @((P 1 :% 1) ~ ((P 3 :% 1) % 3))
_testSlash_Rat3P1_Nat4 = Dict @((P 3 :% 4) ~ ((P 3 :% 1) % 4))
_testSlash_Rat3P1_IntN4 = Dict @((N 3 :% 4) ~ ((P 3 :% 1) % (N 4)))
_testSlash_Rat3P1_IntN3 = Dict @((N 1 :% 1) ~ ((P 3 :% 1) % (N 3)))
_testSlash_Rat3P1_IntN2 = Dict @((N 3 :% 2) ~ ((P 3 :% 1) % (N 2)))
_testSlash_Rat3P1_IntN1 = Dict @((N 3 :% 1) ~ ((P 3 :% 1) % (N 1)))
_testSlash_Rat3P1_IntP1 = Dict @((P 3 :% 1) ~ ((P 3 :% 1) % (P 1)))
_testSlash_Rat3P1_IntP2 = Dict @((P 3 :% 2) ~ ((P 3 :% 1) % (P 2)))
_testSlash_Rat3P1_IntP3 = Dict @((P 1 :% 1) ~ ((P 3 :% 1) % (P 3)))
_testSlash_Rat3P1_IntP4 = Dict @((P 3 :% 4) ~ ((P 3 :% 1) % (P 4)))
_testSlash_Rat3P1_Rat4N1 = Dict @((N 3 :% 4) ~ ((P 3 :% 1) % (N 4 :% 1)))
_testSlash_Rat3P1_Rat3N1 = Dict @((N 1 :% 1) ~ ((P 3 :% 1) % (N 3 :% 1)))
_testSlash_Rat3P1_Rat2N1 = Dict @((N 3 :% 2) ~ ((P 3 :% 1) % (N 2 :% 1)))
_testSlash_Rat3P1_Rat3N2 = Dict @((N 2 :% 1) ~ ((P 3 :% 1) % (N 3 :% 2)))
_testSlash_Rat3P1_Rat4N3 = Dict @((N 9 :% 4) ~ ((P 3 :% 1) % (N 4 :% 3)))
_testSlash_Rat3P1_Rat1N1 = Dict @((N 3 :% 1) ~ ((P 3 :% 1) % (N 1 :% 1)))
_testSlash_Rat3P1_Rat3N4 = Dict @((N 4 :% 1) ~ ((P 3 :% 1) % (N 3 :% 4)))
_testSlash_Rat3P1_Rat2N3 = Dict @((N 9 :% 2) ~ ((P 3 :% 1) % (N 2 :% 3)))
_testSlash_Rat3P1_Rat1N2 = Dict @((N 6 :% 1) ~ ((P 3 :% 1) % (N 1 :% 2)))
_testSlash_Rat3P1_Rat1N3 = Dict @((N 9 :% 1) ~ ((P 3 :% 1) % (N 1 :% 3)))
_testSlash_Rat3P1_Rat1N4 = Dict @((N 12 :% 1) ~ ((P 3 :% 1) % (N 1 :% 4)))
_testSlash_Rat3P1_Rat1P4 = Dict @((P 12 :% 1) ~ ((P 3 :% 1) % (P 1 :% 4)))
_testSlash_Rat3P1_Rat1P3 = Dict @((P 9 :% 1) ~ ((P 3 :% 1) % (P 1 :% 3)))
_testSlash_Rat3P1_Rat1P2 = Dict @((P 6 :% 1) ~ ((P 3 :% 1) % (P 1 :% 2)))
_testSlash_Rat3P1_Rat2P3 = Dict @((P 9 :% 2) ~ ((P 3 :% 1) % (P 2 :% 3)))
_testSlash_Rat3P1_Rat3P4 = Dict @((P 4 :% 1) ~ ((P 3 :% 1) % (P 3 :% 4)))
_testSlash_Rat3P1_Rat1P1 = Dict @((P 3 :% 1) ~ ((P 3 :% 1) % (P 1 :% 1)))
_testSlash_Rat3P1_Rat4P3 = Dict @((P 9 :% 4) ~ ((P 3 :% 1) % (P 4 :% 3)))
_testSlash_Rat3P1_Rat3P2 = Dict @((P 2 :% 1) ~ ((P 3 :% 1) % (P 3 :% 2)))
_testSlash_Rat3P1_Rat2P1 = Dict @((P 3 :% 2) ~ ((P 3 :% 1) % (P 2 :% 1)))
_testSlash_Rat3P1_Rat3P1 = Dict @((P 1 :% 1) ~ ((P 3 :% 1) % (P 3 :% 1)))
_testSlash_Rat3P1_Rat4P1 = Dict @((P 3 :% 4) ~ ((P 3 :% 1) % (P 4 :% 1)))
_testSlash_Rat4P1_Nat1 = Dict @((P 4 :% 1) ~ ((P 4 :% 1) % 1))
_testSlash_Rat4P1_Nat2 = Dict @((P 2 :% 1) ~ ((P 4 :% 1) % 2))
_testSlash_Rat4P1_Nat3 = Dict @((P 4 :% 3) ~ ((P 4 :% 1) % 3))
_testSlash_Rat4P1_Nat4 = Dict @((P 1 :% 1) ~ ((P 4 :% 1) % 4))
_testSlash_Rat4P1_IntN4 = Dict @((N 1 :% 1) ~ ((P 4 :% 1) % (N 4)))
_testSlash_Rat4P1_IntN3 = Dict @((N 4 :% 3) ~ ((P 4 :% 1) % (N 3)))
_testSlash_Rat4P1_IntN2 = Dict @((N 2 :% 1) ~ ((P 4 :% 1) % (N 2)))
_testSlash_Rat4P1_IntN1 = Dict @((N 4 :% 1) ~ ((P 4 :% 1) % (N 1)))
_testSlash_Rat4P1_IntP1 = Dict @((P 4 :% 1) ~ ((P 4 :% 1) % (P 1)))
_testSlash_Rat4P1_IntP2 = Dict @((P 2 :% 1) ~ ((P 4 :% 1) % (P 2)))
_testSlash_Rat4P1_IntP3 = Dict @((P 4 :% 3) ~ ((P 4 :% 1) % (P 3)))
_testSlash_Rat4P1_IntP4 = Dict @((P 1 :% 1) ~ ((P 4 :% 1) % (P 4)))
_testSlash_Rat4P1_Rat4N1 = Dict @((N 1 :% 1) ~ ((P 4 :% 1) % (N 4 :% 1)))
_testSlash_Rat4P1_Rat3N1 = Dict @((N 4 :% 3) ~ ((P 4 :% 1) % (N 3 :% 1)))
_testSlash_Rat4P1_Rat2N1 = Dict @((N 2 :% 1) ~ ((P 4 :% 1) % (N 2 :% 1)))
_testSlash_Rat4P1_Rat3N2 = Dict @((N 8 :% 3) ~ ((P 4 :% 1) % (N 3 :% 2)))
_testSlash_Rat4P1_Rat4N3 = Dict @((N 3 :% 1) ~ ((P 4 :% 1) % (N 4 :% 3)))
_testSlash_Rat4P1_Rat1N1 = Dict @((N 4 :% 1) ~ ((P 4 :% 1) % (N 1 :% 1)))
_testSlash_Rat4P1_Rat3N4 = Dict @((N 16 :% 3) ~ ((P 4 :% 1) % (N 3 :% 4)))
_testSlash_Rat4P1_Rat2N3 = Dict @((N 6 :% 1) ~ ((P 4 :% 1) % (N 2 :% 3)))
_testSlash_Rat4P1_Rat1N2 = Dict @((N 8 :% 1) ~ ((P 4 :% 1) % (N 1 :% 2)))
_testSlash_Rat4P1_Rat1N3 = Dict @((N 12 :% 1) ~ ((P 4 :% 1) % (N 1 :% 3)))
_testSlash_Rat4P1_Rat1N4 = Dict @((N 16 :% 1) ~ ((P 4 :% 1) % (N 1 :% 4)))
_testSlash_Rat4P1_Rat1P4 = Dict @((P 16 :% 1) ~ ((P 4 :% 1) % (P 1 :% 4)))
_testSlash_Rat4P1_Rat1P3 = Dict @((P 12 :% 1) ~ ((P 4 :% 1) % (P 1 :% 3)))
_testSlash_Rat4P1_Rat1P2 = Dict @((P 8 :% 1) ~ ((P 4 :% 1) % (P 1 :% 2)))
_testSlash_Rat4P1_Rat2P3 = Dict @((P 6 :% 1) ~ ((P 4 :% 1) % (P 2 :% 3)))
_testSlash_Rat4P1_Rat3P4 = Dict @((P 16 :% 3) ~ ((P 4 :% 1) % (P 3 :% 4)))
_testSlash_Rat4P1_Rat1P1 = Dict @((P 4 :% 1) ~ ((P 4 :% 1) % (P 1 :% 1)))
_testSlash_Rat4P1_Rat4P3 = Dict @((P 3 :% 1) ~ ((P 4 :% 1) % (P 4 :% 3)))
_testSlash_Rat4P1_Rat3P2 = Dict @((P 8 :% 3) ~ ((P 4 :% 1) % (P 3 :% 2)))
_testSlash_Rat4P1_Rat2P1 = Dict @((P 2 :% 1) ~ ((P 4 :% 1) % (P 2 :% 1)))
_testSlash_Rat4P1_Rat3P1 = Dict @((P 4 :% 3) ~ ((P 4 :% 1) % (P 3 :% 1)))
_testSlash_Rat4P1_Rat4P1 = Dict @((P 1 :% 1) ~ ((P 4 :% 1) % (P 4 :% 1)))
