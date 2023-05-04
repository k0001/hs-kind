{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE UndecidableInstances #-}

-- | We define 'Round' here keep the TemplateHaskell stuff contained.
module KindInteger.Round {--}
  ( Round(..)
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
  , SRound(..)
  ) --}
  where

import Data.Bool.Singletons
import Data.Eq.Singletons
import Data.Ord.Singletons
import Data.Singletons.TH
import Prelude.Singletons

--------------------------------------------------------------------------------

-- | Rounding strategy.
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
  deriving stock (Read)

$(fmap concat $ sequence
    [ genSingletons [''Round]
    , singletons [d|
      deriving stock instance Bounded Round
      deriving stock instance Enum Round
      deriving stock instance Eq Round
      deriving stock instance Ord Round
      deriving stock instance Show Round
      |]
    ])

