module Main {--}
  ( main
  ) --}
  where

import Control.Applicative
import Control.Exception qualified as Ex
import Control.Monad
import Data.List qualified as List
import Data.Maybe
import Data.Ratio as P
import Data.Type.Equality (TestEquality(..))
import Data.Type.Ord (type (<=))
import GHC.Exts (Constraint)
import Prelude hiding (Integer)
import Prelude qualified as P
import System.Exit
import Text.Read

import KindInteger (P, N)
import KindInteger qualified as K

--------------------------------------------------------------------------------

data Dict (c :: Constraint) where
  Dict :: c => Dict c

--------------------------------------------------------------------------------

_testEq =  Dict
_testEq :: Dict
  ( P 0 K.== P 0,   'True ~ (P 0 K.==? P 0)
  , N 0 K.== N 0,   'True ~ (N 0 K.==? N 0)
  , P 0 K.== N 0,   'True ~ (P 0 K.==? N 0)
  , N 0 K.== P 0,   'True ~ (N 0 K.==? P 0)

  , P 0 K./= P 1,   'True ~ (P 0 K./=? P 1)
  , P 0 K./= N 1,   'True ~ (P 0 K./=? N 1)

  , N 0 K./= N 1,   'True ~ (N 0 K./=? N 1)
  , N 0 K./= N 1,   'True ~ (N 0 K./=? N 1)

  , P 1 K./= P 0,   'True ~ (P 1 K./=? P 0)
  , P 1 K./= N 0,   'True ~ (P 1 K./=? N 0)

  , N 1 K./= N 0,   'True ~ (N 1 K./=? N 0)
  , N 1 K./= N 0,   'True ~ (N 1 K./=? N 0)
  )

_testCmp =  Dict
_testCmp :: Dict
  ( P 0 <= P 0
  , P 0 <= N 0
  , N 0 <= P 0
  , N 0 <= N 0

  , N 2 <= N 1
  , N 1 <= N 0
  , N 0 <= P 1

  , P 0 <= P 1
  , P 1 <= P 2
  )

_testAdd  = Dict
_testAdd :: Dict
  ( P 0 ~ P 0 K.+ P 0
  , P 0 ~ N 0 K.+ N 0
  , P 0 ~ P 0 K.+ N 0
  , P 0 ~ N 0 K.+ P 0

  , P 1 ~ P 1 K.+ P 0
  , N 1 ~ N 1 K.+ N 0
  , P 1 ~ P 1 K.+ N 0
  , N 1 ~ N 1 K.+ P 0

  , P 1 ~ P 0 K.+ P 1
  , N 1 ~ N 0 K.+ N 1
  , N 1 ~ P 0 K.+ N 1
  , P 1 ~ N 0 K.+ P 1

  , P 2 ~ P 1 K.+ P 1
  , N 2 ~ N 1 K.+ N 1
  , P 0 ~ P 1 K.+ N 1
  , P 0 ~ N 1 K.+ P 1
  )

_testMul  = Dict
_testMul :: Dict
  ( P 0 ~ P 0 K.* P 0
  , P 0 ~ N 0 K.* N 0
  , P 0 ~ P 0 K.* N 0
  , P 0 ~ N 0 K.* P 0

  , P 0 ~ P 1 K.* P 0
  , P 0 ~ N 1 K.* N 0
  , P 0 ~ P 1 K.* N 0
  , P 0 ~ N 1 K.* P 0

  , P 0 ~ P 0 K.* P 1
  , P 0 ~ N 0 K.* N 1
  , P 0 ~ P 0 K.* N 1
  , P 0 ~ N 0 K.* P 1

  , P 1 ~ P 1 K.* P 1
  , P 1 ~ N 1 K.* N 1
  , N 1 ~ P 1 K.* N 1
  , N 1 ~ N 1 K.* P 1

  , P 2 ~ P 2 K.* P 1
  , P 2 ~ N 2 K.* N 1
  , N 2 ~ P 2 K.* N 1
  , N 2 ~ N 2 K.* P 1

  , P 6 ~ P 2 K.* P 3
  , P 6 ~ N 2 K.* N 3
  , N 6 ~ P 2 K.* N 3
  , N 6 ~ N 2 K.* P 3
  )

_testLog2 =  Dict
_testLog2 :: Dict
  ( P 0 ~ K.Log2 (P 1)
  , P 1 ~ K.Log2 (P 2)
  , P 1 ~ K.Log2 (P 3)
  , P 2 ~ K.Log2 (P 4)
  , P 2 ~ K.Log2 (P 5)
  , P 2 ~ K.Log2 (P 6)
  , P 2 ~ K.Log2 (P 7)
  , P 3 ~ K.Log2 (P 8)
  , P 3 ~ K.Log2 (P 9)
  , P 3 ~ K.Log2 (P 10)
  , P 3 ~ K.Log2 (P 11)
  , P 3 ~ K.Log2 (P 12)
  , P 3 ~ K.Log2 (P 13)
  , P 3 ~ K.Log2 (P 14)
  , P 3 ~ K.Log2 (P 15)
  , P 4 ~ K.Log2 (P 16)
  , P 4 ~ K.Log2 (P 17)
  , P 4 ~ K.Log2 (P 18)
  , P 4 ~ K.Log2 (P 19)
  , P 4 ~ K.Log2 (P 20)
  , P 4 ~ K.Log2 (P 21)
  , P 4 ~ K.Log2 (P 22)
  , P 4 ~ K.Log2 (P 23)
  , P 4 ~ K.Log2 (P 24)
  , P 4 ~ K.Log2 (P 25)
  , P 4 ~ K.Log2 (P 26)
  , P 4 ~ K.Log2 (P 27)
  , P 4 ~ K.Log2 (P 28)
  , P 4 ~ K.Log2 (P 29)
  , P 4 ~ K.Log2 (P 30)
  , P 4 ~ K.Log2 (P 31)
  , P 5 ~ K.Log2 (P 32)
  )

_testNegate =  Dict
_testNegate :: Dict
  ( P 0 ~ K.Negate (P 0)
  , P 0 ~ K.Negate (N 0)
  , N 1 ~ K.Negate (P 1)
  , P 1 ~ K.Negate (N 1)
  , N 2 ~ K.Negate (P 2)
  , P 2 ~ K.Negate (N 2)
  )

_testSign =  Dict
_testSign :: Dict
  ( P 0 ~ K.Sign (P 0)
  , P 0 ~ K.Sign (N 0)
  , P 1 ~ K.Sign (P 1)
  , N 1 ~ K.Sign (N 1)
  , P 1 ~ K.Sign (P 2)
  , N 1 ~ K.Sign (N 2)
  )

_testAbs =  Dict
_testAbs :: Dict
  ( 0 ~ K.Abs (P 0)
  , 0 ~ K.Abs (N 0)
  , 1 ~ K.Abs (P 1)
  , 1 ~ K.Abs (N 1)
  , 2 ~ K.Abs (P 2)
  , 2 ~ K.Abs (N 2)
  )

_testEven =  Dict
_testEven :: Dict
  ( 'True  ~ K.Even (P 0)
  , 'True  ~ K.Even (N 0)
  , 'False ~ K.Even (P 1)
  , 'False ~ K.Even (N 1)
  , 'True  ~ K.Even (P 2)
  , 'True  ~ K.Even (N 2)
  )

_testOdd =  Dict
_testOdd :: Dict
  ( 'False  ~ K.Odd (P 0)
  , 'False  ~ K.Odd (N 0)
  , 'True   ~ K.Odd (P 1)
  , 'True   ~ K.Odd (N 1)
  , 'False  ~ K.Odd (P 2)
  , 'False  ~ K.Odd (N 2)
  )

_testGCD =  Dict
_testGCD :: Dict
  ( 0 ~ K.GCD (P 0) (P 0)
  , 0 ~ K.GCD (P 0) (N 0)
  , 0 ~ K.GCD (N 0) (P 0)
  , 0 ~ K.GCD (N 0) (N 0)

  , 1 ~ K.GCD (P 1) (P 0)
  , 1 ~ K.GCD (P 1) (N 0)
  , 1 ~ K.GCD (N 1) (P 0)
  , 1 ~ K.GCD (N 1) (N 0)

  , 1 ~ K.GCD (P 0) (P 1)
  , 1 ~ K.GCD (P 0) (N 1)
  , 1 ~ K.GCD (N 0) (P 1)
  , 1 ~ K.GCD (N 0) (N 1)

  , 1 ~ K.GCD (P 1) (P 2)
  , 1 ~ K.GCD (P 1) (N 2)
  , 1 ~ K.GCD (N 1) (P 2)
  , 1 ~ K.GCD (N 1) (N 2)

  , 1 ~ K.GCD (P 2) (P 1)
  , 1 ~ K.GCD (P 2) (N 1)
  , 1 ~ K.GCD (N 2) (P 1)
  , 1 ~ K.GCD (N 2) (N 1)

  , 3 ~ K.GCD (P 6) (P 9)
  , 3 ~ K.GCD (P 6) (N 9)
  , 3 ~ K.GCD (N 6) (P 9)
  , 3 ~ K.GCD (N 6) (N 9)

  , 3 ~ K.GCD (P 9) (P 6)
  , 3 ~ K.GCD (P 9) (N 6)
  , 3 ~ K.GCD (N 9) (P 6)
  , 3 ~ K.GCD (N 9) (N 6)
  )

_testLCM =  Dict
_testLCM :: Dict
  ( 0 ~ K.LCM (P 0) (P 0)
  , 0 ~ K.LCM (P 0) (N 0)
  , 0 ~ K.LCM (N 0) (P 0)
  , 0 ~ K.LCM (N 0) (N 0)

  , 0 ~ K.LCM (P 1) (P 0)
  , 0 ~ K.LCM (P 1) (N 0)
  , 0 ~ K.LCM (N 1) (P 0)
  , 0 ~ K.LCM (N 1) (N 0)

  , 0 ~ K.LCM (P 0) (P 1)
  , 0 ~ K.LCM (P 0) (N 1)
  , 0 ~ K.LCM (N 0) (P 1)
  , 0 ~ K.LCM (N 0) (N 1)

  , 2 ~ K.LCM (P 1) (P 2)
  , 2 ~ K.LCM (P 1) (N 2)
  , 2 ~ K.LCM (N 1) (P 2)
  , 2 ~ K.LCM (N 1) (N 2)

  , 2 ~ K.LCM (P 2) (P 1)
  , 2 ~ K.LCM (P 2) (N 1)
  , 2 ~ K.LCM (N 2) (P 1)
  , 2 ~ K.LCM (N 2) (N 1)

  , 18 ~ K.LCM (P 6) (P 9)
  , 18 ~ K.LCM (P 6) (N 9)
  , 18 ~ K.LCM (N 6) (P 9)
  , 18 ~ K.LCM (N 6) (N 9)

  , 18 ~ K.LCM (P 9) (P 6)
  , 18 ~ K.LCM (P 9) (N 6)
  , 18 ~ K.LCM (N 9) (P 6)
  , 18 ~ K.LCM (N 9) (N 6)
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

main :: IO ()
main = testsMain $
  [ assert "Prelude % throws RatioZeroDenominator" =<< do
      ea <- Ex.try (Ex.evaluate (1 P.% 0 :: P.Rational))
      pure (ea == Left Ex.RatioZeroDenominator)

  , assert "integerVal . someIntegerVal == id" $
    flip all [-5 .. 5] $ \a ->
      let a' = K.fromPrelude a
      in case K.someIntegerVal a' of
           K.SomeInteger pa ->
             a' == K.integerVal pa

  , assert "sameIntegerVal a a" $
    flip all [-5 .. 5] $ \a ->
      let a' = K.fromPrelude a
      in case K.someIntegerVal a' of
           K.SomeInteger pa ->
             isJust (K.sameInteger pa pa)

  , assert "sameIntegerVal a a'" $
    flip all [-5 .. 5] $ \a ->
      let a' = K.fromPrelude a
      in case (K.someIntegerVal a', K.someIntegerVal a') of
           (K.SomeInteger pa1, K.SomeInteger pa2) ->
             isJust (K.sameInteger pa1 pa2)

  , assert "sameIntegerVal a b" $
    flip all (liftA2 (,) [-5 .. 5] [-5 .. 5])$ \(a, b) ->
      let a' = K.fromPrelude a
          b' = K.fromPrelude b
      in case (K.someIntegerVal a', K.someIntegerVal b') of
           (K.SomeInteger pa, K.SomeInteger pb)
             | a == b    -> isJust    (K.sameInteger pa pb)
             | otherwise -> isNothing (K.sameInteger pa pb)

  , assert "Eq fromPrelude" $
    flip all (liftA2 (,) [-5 .. 5] [-5 .. 5])$ \(a, b) ->
      (a == b) == (K.fromPrelude a == K.fromPrelude b)

  , assert "Ord fromPrelude" $
    flip all (liftA2 (,) [-5 .. 5] [-5 .. 5])$ \(a, b) ->
      (a `compare` b) == (K.fromPrelude a `compare` K.fromPrelude b)

  , assert "Show fromPrelude" $
    flip all [-5 .. 5] $ \i ->
      show i == show (K.fromPrelude i)

  , assert "Read fromPrelude" $
    flip all [-5 .. 5] $ \i ->
      let str = show (i :: P.Integer)
      in readMaybe @P.Integer str
            == fmap K.toPrelude (readMaybe @K.Integer str)

  , assert "Eq SomeInteger" $
    flip all (liftA2 (,) [-5 .. 5] [-5 .. 5])$ \(a, b) ->
      let a' = K.fromPrelude a
          b' = K.fromPrelude b
      in (a == b) == (K.someIntegerVal a' == K.someIntegerVal b')

  , assert "Ord SomeInteger" $
    flip all (liftA2 (,) [-5 .. 5] [-5 .. 5])$ \(a, b) ->
      compare a b ==
         compare (K.someIntegerVal (K.fromPrelude a))
                 (K.someIntegerVal (K.fromPrelude b))

  , assert "Show SomeInteger" $
    flip all [-5 .. 5] $ \a ->
      let a' = K.fromPrelude a
      in show a == show (K.someIntegerVal a')

  , assert "Read SomeInteger" $
    flip all [-5 .. 5] $ \i ->
      let str = show (i :: P.Integer)
      in readMaybe @P.Integer str
            == fmap (\(K.SomeInteger p) -> K.toPrelude (K.integerVal p))
                    (readMaybe @K.SomeInteger str)

  , assert "TestEquality +0 +0" $
     isJust (testEquality (K.SInteger @(P 0)) (K.SInteger @(P 0)))
  , assert "TestEquality -0 -0" $
     isJust (testEquality (K.SInteger @(N 0)) (K.SInteger @(N 0)))
  , assert "TestEquality +0 -0" $
     isNothing (testEquality (K.SInteger @(P 0)) (K.SInteger @(N 0)))
  , assert "TestEquality -0 +0" $
     isNothing (testEquality (K.SInteger @(N 0)) (K.SInteger @(P 0)))
  , assert "TestEquality +0 +1" $
     isNothing (testEquality (K.SInteger @(P 0)) (K.SInteger @(P 1)))
  , assert "TestEquality +0 -1" $
     isNothing (testEquality (K.SInteger @(P 0)) (K.SInteger @(N 1)))
  , assert "TestEquality -0 +1" $
     isNothing (testEquality (K.SInteger @(N 0)) (K.SInteger @(P 1)))
  , assert "TestEquality -0 -1" $
     isNothing (testEquality (K.SInteger @(N 0)) (K.SInteger @(N 1)))
  , assert "TestEquality +1 +0" $
     isNothing (testEquality (K.SInteger @(P 1)) (K.SInteger @(P 0)))
  , assert "TestEquality +1 -0" $
     isNothing (testEquality (K.SInteger @(P 1)) (K.SInteger @(N 0)))
  , assert "TestEquality -1 +0" $
     isNothing (testEquality (K.SInteger @(N 1)) (K.SInteger @(P 0)))
  , assert "TestEquality -1 -0" $
     isNothing (testEquality (K.SInteger @(N 1)) (K.SInteger @(N 0)))

  , assert "Show Integer +0" $
     "0" == show (K.fromSInteger (K.SInteger @(P 0)))
  , assert "Show Integer -0" $
     "0" == show (K.fromSInteger (K.SInteger @(N 0)))
  , assert "Show Integer +1" $
     "1" == show (K.fromSInteger (K.SInteger @(P 1)))
  , assert "Show Integer -1" $
     "-1" == show (K.fromSInteger (K.SInteger @(N 1)))

  , assert "Show SInteger +0" $
     "SInteger @(P 0)" == show (K.SInteger @(P 0))
  , assert "Show SInteger -0" $
     "SInteger @(N 0)" == show (K.SInteger @(N 0))
  , assert "Show SInteger +1" $
     "SInteger @(P 1)" == show (K.SInteger @(P 1))
  , assert "Show SInteger -1" $
     "SInteger @(N 1)" == show (K.SInteger @(N 1))

  ] <> testsDivRem

testsDivRem :: [IO Bool]
testsDivRem = do
  b :: P.Integer <- [-5 .. 5]
  guard (b P./= 0)
  a :: P.Integer <- [-5 .. 5]
  r :: K.Round <- [minBound .. maxBound]
  let tname :: String -> ShowS
      tname t = showString t . showChar ' ' . shows r . showChar ' '
              . shows a . showChar ' ' . shows b
  [ assert (tname "divRem" "") $ case K.divRem r a b of
                                   (q, m) -> m == a - b * q
    , assert (tname "div" "") $ fst (K.divRem r a b) == K.div r a b
    , assert (tname "rem" "") $ snd (K.divRem r a b) == K.rem r a b
    ]


--------------------------------------------------------------------------------

_divRemTestCode :: String
_divRemTestCode = unlines $ List.sort $ do
  b <- [-4 .. 4]
  guard (b P./= 0)
  a <- [-4 .. 4]
  r <- [minBound..maxBound]
  let (q, m) = K.divRem r a b
      sname :: String -> ShowS
      sname t = showString "_test_"
              . showString t
              . showChar '_'
              . showsPrec 0 r
              . showChar '_'
              . showChar (if a < 0 then 'N' else 'P')
              . shows (abs a)
              . showChar '_'
              . showChar (if b < 0 then 'N' else 'P')
              . shows (abs b)
      sDiv :: ShowS
      sDiv = sname "Div"
           . showString " :: Dict (K.Div 'K."
           . shows r
           . showChar ' '
           . K.showsPrecTypeLit 11 (K.fromPrelude a)
           . showChar ' '
           . K.showsPrecTypeLit 11 (K.fromPrelude b)
           . showString " ~ "
           . K.showsPrecTypeLit 0 (K.fromPrelude q)
           . showString ")\n"
           . sname "Div"
           . showString " =  Dict"
      sRem :: ShowS
      sRem = sname "Rem"
           . showString " :: Dict (K.Rem 'K."
           . shows r
           . showChar ' '
           . K.showsPrecTypeLit 11 (K.fromPrelude a)
           . showChar ' '
           . K.showsPrecTypeLit 11 (K.fromPrelude b)
           . showString " ~ "
           . K.showsPrecTypeLit 0 (K.fromPrelude m)
           . showString ")\n"
           . sname "Rem"
           . showString " =  Dict"
      ss :: ShowS
      ss = sDiv . showChar '\n' . sRem
  pure (ss "")


-- The following tests are generated by `_divRemTestCode` in this remule.
-- Copy and paste by hand.
_test_Div_RoundAway_N1_N1 :: Dict (K.Div 'K.RoundAway (N 1) (N 1) ~ P 1)
_test_Div_RoundAway_N1_N1 =  Dict
_test_Rem_RoundAway_N1_N1 :: Dict (K.Rem 'K.RoundAway (N 1) (N 1) ~ P 0)
_test_Rem_RoundAway_N1_N1 =  Dict
_test_Div_RoundAway_N1_N2 :: Dict (K.Div 'K.RoundAway (N 1) (N 2) ~ P 1)
_test_Div_RoundAway_N1_N2 =  Dict
_test_Rem_RoundAway_N1_N2 :: Dict (K.Rem 'K.RoundAway (N 1) (N 2) ~ P 1)
_test_Rem_RoundAway_N1_N2 =  Dict
_test_Div_RoundAway_N1_N3 :: Dict (K.Div 'K.RoundAway (N 1) (N 3) ~ P 1)
_test_Div_RoundAway_N1_N3 =  Dict
_test_Rem_RoundAway_N1_N3 :: Dict (K.Rem 'K.RoundAway (N 1) (N 3) ~ P 2)
_test_Rem_RoundAway_N1_N3 =  Dict
_test_Div_RoundAway_N1_N4 :: Dict (K.Div 'K.RoundAway (N 1) (N 4) ~ P 1)
_test_Div_RoundAway_N1_N4 =  Dict
_test_Rem_RoundAway_N1_N4 :: Dict (K.Rem 'K.RoundAway (N 1) (N 4) ~ P 3)
_test_Rem_RoundAway_N1_N4 =  Dict
_test_Div_RoundAway_N1_P1 :: Dict (K.Div 'K.RoundAway (N 1) (P 1) ~ N 1)
_test_Div_RoundAway_N1_P1 =  Dict
_test_Rem_RoundAway_N1_P1 :: Dict (K.Rem 'K.RoundAway (N 1) (P 1) ~ P 0)
_test_Rem_RoundAway_N1_P1 =  Dict
_test_Div_RoundAway_N1_P2 :: Dict (K.Div 'K.RoundAway (N 1) (P 2) ~ N 1)
_test_Div_RoundAway_N1_P2 =  Dict
_test_Rem_RoundAway_N1_P2 :: Dict (K.Rem 'K.RoundAway (N 1) (P 2) ~ P 1)
_test_Rem_RoundAway_N1_P2 =  Dict
_test_Div_RoundAway_N1_P3 :: Dict (K.Div 'K.RoundAway (N 1) (P 3) ~ N 1)
_test_Div_RoundAway_N1_P3 =  Dict
_test_Rem_RoundAway_N1_P3 :: Dict (K.Rem 'K.RoundAway (N 1) (P 3) ~ P 2)
_test_Rem_RoundAway_N1_P3 =  Dict
_test_Div_RoundAway_N1_P4 :: Dict (K.Div 'K.RoundAway (N 1) (P 4) ~ N 1)
_test_Div_RoundAway_N1_P4 =  Dict
_test_Rem_RoundAway_N1_P4 :: Dict (K.Rem 'K.RoundAway (N 1) (P 4) ~ P 3)
_test_Rem_RoundAway_N1_P4 =  Dict
_test_Div_RoundAway_N2_N1 :: Dict (K.Div 'K.RoundAway (N 2) (N 1) ~ P 2)
_test_Div_RoundAway_N2_N1 =  Dict
_test_Rem_RoundAway_N2_N1 :: Dict (K.Rem 'K.RoundAway (N 2) (N 1) ~ P 0)
_test_Rem_RoundAway_N2_N1 =  Dict
_test_Div_RoundAway_N2_N2 :: Dict (K.Div 'K.RoundAway (N 2) (N 2) ~ P 1)
_test_Div_RoundAway_N2_N2 =  Dict
_test_Rem_RoundAway_N2_N2 :: Dict (K.Rem 'K.RoundAway (N 2) (N 2) ~ P 0)
_test_Rem_RoundAway_N2_N2 =  Dict
_test_Div_RoundAway_N2_N3 :: Dict (K.Div 'K.RoundAway (N 2) (N 3) ~ P 1)
_test_Div_RoundAway_N2_N3 =  Dict
_test_Rem_RoundAway_N2_N3 :: Dict (K.Rem 'K.RoundAway (N 2) (N 3) ~ P 1)
_test_Rem_RoundAway_N2_N3 =  Dict
_test_Div_RoundAway_N2_N4 :: Dict (K.Div 'K.RoundAway (N 2) (N 4) ~ P 1)
_test_Div_RoundAway_N2_N4 =  Dict
_test_Rem_RoundAway_N2_N4 :: Dict (K.Rem 'K.RoundAway (N 2) (N 4) ~ P 2)
_test_Rem_RoundAway_N2_N4 =  Dict
_test_Div_RoundAway_N2_P1 :: Dict (K.Div 'K.RoundAway (N 2) (P 1) ~ N 2)
_test_Div_RoundAway_N2_P1 =  Dict
_test_Rem_RoundAway_N2_P1 :: Dict (K.Rem 'K.RoundAway (N 2) (P 1) ~ P 0)
_test_Rem_RoundAway_N2_P1 =  Dict
_test_Div_RoundAway_N2_P2 :: Dict (K.Div 'K.RoundAway (N 2) (P 2) ~ N 1)
_test_Div_RoundAway_N2_P2 =  Dict
_test_Rem_RoundAway_N2_P2 :: Dict (K.Rem 'K.RoundAway (N 2) (P 2) ~ P 0)
_test_Rem_RoundAway_N2_P2 =  Dict
_test_Div_RoundAway_N2_P3 :: Dict (K.Div 'K.RoundAway (N 2) (P 3) ~ N 1)
_test_Div_RoundAway_N2_P3 =  Dict
_test_Rem_RoundAway_N2_P3 :: Dict (K.Rem 'K.RoundAway (N 2) (P 3) ~ P 1)
_test_Rem_RoundAway_N2_P3 =  Dict
_test_Div_RoundAway_N2_P4 :: Dict (K.Div 'K.RoundAway (N 2) (P 4) ~ N 1)
_test_Div_RoundAway_N2_P4 =  Dict
_test_Rem_RoundAway_N2_P4 :: Dict (K.Rem 'K.RoundAway (N 2) (P 4) ~ P 2)
_test_Rem_RoundAway_N2_P4 =  Dict
_test_Div_RoundAway_N3_N1 :: Dict (K.Div 'K.RoundAway (N 3) (N 1) ~ P 3)
_test_Div_RoundAway_N3_N1 =  Dict
_test_Rem_RoundAway_N3_N1 :: Dict (K.Rem 'K.RoundAway (N 3) (N 1) ~ P 0)
_test_Rem_RoundAway_N3_N1 =  Dict
_test_Div_RoundAway_N3_N2 :: Dict (K.Div 'K.RoundAway (N 3) (N 2) ~ P 2)
_test_Div_RoundAway_N3_N2 =  Dict
_test_Rem_RoundAway_N3_N2 :: Dict (K.Rem 'K.RoundAway (N 3) (N 2) ~ P 1)
_test_Rem_RoundAway_N3_N2 =  Dict
_test_Div_RoundAway_N3_N3 :: Dict (K.Div 'K.RoundAway (N 3) (N 3) ~ P 1)
_test_Div_RoundAway_N3_N3 =  Dict
_test_Rem_RoundAway_N3_N3 :: Dict (K.Rem 'K.RoundAway (N 3) (N 3) ~ P 0)
_test_Rem_RoundAway_N3_N3 =  Dict
_test_Div_RoundAway_N3_N4 :: Dict (K.Div 'K.RoundAway (N 3) (N 4) ~ P 1)
_test_Div_RoundAway_N3_N4 =  Dict
_test_Rem_RoundAway_N3_N4 :: Dict (K.Rem 'K.RoundAway (N 3) (N 4) ~ P 1)
_test_Rem_RoundAway_N3_N4 =  Dict
_test_Div_RoundAway_N3_P1 :: Dict (K.Div 'K.RoundAway (N 3) (P 1) ~ N 3)
_test_Div_RoundAway_N3_P1 =  Dict
_test_Rem_RoundAway_N3_P1 :: Dict (K.Rem 'K.RoundAway (N 3) (P 1) ~ P 0)
_test_Rem_RoundAway_N3_P1 =  Dict
_test_Div_RoundAway_N3_P2 :: Dict (K.Div 'K.RoundAway (N 3) (P 2) ~ N 2)
_test_Div_RoundAway_N3_P2 =  Dict
_test_Rem_RoundAway_N3_P2 :: Dict (K.Rem 'K.RoundAway (N 3) (P 2) ~ P 1)
_test_Rem_RoundAway_N3_P2 =  Dict
_test_Div_RoundAway_N3_P3 :: Dict (K.Div 'K.RoundAway (N 3) (P 3) ~ N 1)
_test_Div_RoundAway_N3_P3 =  Dict
_test_Rem_RoundAway_N3_P3 :: Dict (K.Rem 'K.RoundAway (N 3) (P 3) ~ P 0)
_test_Rem_RoundAway_N3_P3 =  Dict
_test_Div_RoundAway_N3_P4 :: Dict (K.Div 'K.RoundAway (N 3) (P 4) ~ N 1)
_test_Div_RoundAway_N3_P4 =  Dict
_test_Rem_RoundAway_N3_P4 :: Dict (K.Rem 'K.RoundAway (N 3) (P 4) ~ P 1)
_test_Rem_RoundAway_N3_P4 =  Dict
_test_Div_RoundAway_N4_N1 :: Dict (K.Div 'K.RoundAway (N 4) (N 1) ~ P 4)
_test_Div_RoundAway_N4_N1 =  Dict
_test_Rem_RoundAway_N4_N1 :: Dict (K.Rem 'K.RoundAway (N 4) (N 1) ~ P 0)
_test_Rem_RoundAway_N4_N1 =  Dict
_test_Div_RoundAway_N4_N2 :: Dict (K.Div 'K.RoundAway (N 4) (N 2) ~ P 2)
_test_Div_RoundAway_N4_N2 =  Dict
_test_Rem_RoundAway_N4_N2 :: Dict (K.Rem 'K.RoundAway (N 4) (N 2) ~ P 0)
_test_Rem_RoundAway_N4_N2 =  Dict
_test_Div_RoundAway_N4_N3 :: Dict (K.Div 'K.RoundAway (N 4) (N 3) ~ P 2)
_test_Div_RoundAway_N4_N3 =  Dict
_test_Rem_RoundAway_N4_N3 :: Dict (K.Rem 'K.RoundAway (N 4) (N 3) ~ P 2)
_test_Rem_RoundAway_N4_N3 =  Dict
_test_Div_RoundAway_N4_N4 :: Dict (K.Div 'K.RoundAway (N 4) (N 4) ~ P 1)
_test_Div_RoundAway_N4_N4 =  Dict
_test_Rem_RoundAway_N4_N4 :: Dict (K.Rem 'K.RoundAway (N 4) (N 4) ~ P 0)
_test_Rem_RoundAway_N4_N4 =  Dict
_test_Div_RoundAway_N4_P1 :: Dict (K.Div 'K.RoundAway (N 4) (P 1) ~ N 4)
_test_Div_RoundAway_N4_P1 =  Dict
_test_Rem_RoundAway_N4_P1 :: Dict (K.Rem 'K.RoundAway (N 4) (P 1) ~ P 0)
_test_Rem_RoundAway_N4_P1 =  Dict
_test_Div_RoundAway_N4_P2 :: Dict (K.Div 'K.RoundAway (N 4) (P 2) ~ N 2)
_test_Div_RoundAway_N4_P2 =  Dict
_test_Rem_RoundAway_N4_P2 :: Dict (K.Rem 'K.RoundAway (N 4) (P 2) ~ P 0)
_test_Rem_RoundAway_N4_P2 =  Dict
_test_Div_RoundAway_N4_P3 :: Dict (K.Div 'K.RoundAway (N 4) (P 3) ~ N 2)
_test_Div_RoundAway_N4_P3 =  Dict
_test_Rem_RoundAway_N4_P3 :: Dict (K.Rem 'K.RoundAway (N 4) (P 3) ~ P 2)
_test_Rem_RoundAway_N4_P3 =  Dict
_test_Div_RoundAway_N4_P4 :: Dict (K.Div 'K.RoundAway (N 4) (P 4) ~ N 1)
_test_Div_RoundAway_N4_P4 =  Dict
_test_Rem_RoundAway_N4_P4 :: Dict (K.Rem 'K.RoundAway (N 4) (P 4) ~ P 0)
_test_Rem_RoundAway_N4_P4 =  Dict
_test_Div_RoundAway_P0_N1 :: Dict (K.Div 'K.RoundAway (P 0) (N 1) ~ P 0)
_test_Div_RoundAway_P0_N1 =  Dict
_test_Rem_RoundAway_P0_N1 :: Dict (K.Rem 'K.RoundAway (P 0) (N 1) ~ P 0)
_test_Rem_RoundAway_P0_N1 =  Dict
_test_Div_RoundAway_P0_N2 :: Dict (K.Div 'K.RoundAway (P 0) (N 2) ~ P 0)
_test_Div_RoundAway_P0_N2 =  Dict
_test_Rem_RoundAway_P0_N2 :: Dict (K.Rem 'K.RoundAway (P 0) (N 2) ~ P 0)
_test_Rem_RoundAway_P0_N2 =  Dict
_test_Div_RoundAway_P0_N3 :: Dict (K.Div 'K.RoundAway (P 0) (N 3) ~ P 0)
_test_Div_RoundAway_P0_N3 =  Dict
_test_Rem_RoundAway_P0_N3 :: Dict (K.Rem 'K.RoundAway (P 0) (N 3) ~ P 0)
_test_Rem_RoundAway_P0_N3 =  Dict
_test_Div_RoundAway_P0_N4 :: Dict (K.Div 'K.RoundAway (P 0) (N 4) ~ P 0)
_test_Div_RoundAway_P0_N4 =  Dict
_test_Rem_RoundAway_P0_N4 :: Dict (K.Rem 'K.RoundAway (P 0) (N 4) ~ P 0)
_test_Rem_RoundAway_P0_N4 =  Dict
_test_Div_RoundAway_P0_P1 :: Dict (K.Div 'K.RoundAway (P 0) (P 1) ~ P 0)
_test_Div_RoundAway_P0_P1 =  Dict
_test_Rem_RoundAway_P0_P1 :: Dict (K.Rem 'K.RoundAway (P 0) (P 1) ~ P 0)
_test_Rem_RoundAway_P0_P1 =  Dict
_test_Div_RoundAway_P0_P2 :: Dict (K.Div 'K.RoundAway (P 0) (P 2) ~ P 0)
_test_Div_RoundAway_P0_P2 =  Dict
_test_Rem_RoundAway_P0_P2 :: Dict (K.Rem 'K.RoundAway (P 0) (P 2) ~ P 0)
_test_Rem_RoundAway_P0_P2 =  Dict
_test_Div_RoundAway_P0_P3 :: Dict (K.Div 'K.RoundAway (P 0) (P 3) ~ P 0)
_test_Div_RoundAway_P0_P3 =  Dict
_test_Rem_RoundAway_P0_P3 :: Dict (K.Rem 'K.RoundAway (P 0) (P 3) ~ P 0)
_test_Rem_RoundAway_P0_P3 =  Dict
_test_Div_RoundAway_P0_P4 :: Dict (K.Div 'K.RoundAway (P 0) (P 4) ~ P 0)
_test_Div_RoundAway_P0_P4 =  Dict
_test_Rem_RoundAway_P0_P4 :: Dict (K.Rem 'K.RoundAway (P 0) (P 4) ~ P 0)
_test_Rem_RoundAway_P0_P4 =  Dict
_test_Div_RoundAway_P1_N1 :: Dict (K.Div 'K.RoundAway (P 1) (N 1) ~ N 1)
_test_Div_RoundAway_P1_N1 =  Dict
_test_Rem_RoundAway_P1_N1 :: Dict (K.Rem 'K.RoundAway (P 1) (N 1) ~ P 0)
_test_Rem_RoundAway_P1_N1 =  Dict
_test_Div_RoundAway_P1_N2 :: Dict (K.Div 'K.RoundAway (P 1) (N 2) ~ N 1)
_test_Div_RoundAway_P1_N2 =  Dict
_test_Rem_RoundAway_P1_N2 :: Dict (K.Rem 'K.RoundAway (P 1) (N 2) ~ N 1)
_test_Rem_RoundAway_P1_N2 =  Dict
_test_Div_RoundAway_P1_N3 :: Dict (K.Div 'K.RoundAway (P 1) (N 3) ~ N 1)
_test_Div_RoundAway_P1_N3 =  Dict
_test_Rem_RoundAway_P1_N3 :: Dict (K.Rem 'K.RoundAway (P 1) (N 3) ~ N 2)
_test_Rem_RoundAway_P1_N3 =  Dict
_test_Div_RoundAway_P1_N4 :: Dict (K.Div 'K.RoundAway (P 1) (N 4) ~ N 1)
_test_Div_RoundAway_P1_N4 =  Dict
_test_Rem_RoundAway_P1_N4 :: Dict (K.Rem 'K.RoundAway (P 1) (N 4) ~ N 3)
_test_Rem_RoundAway_P1_N4 =  Dict
_test_Div_RoundAway_P1_P1 :: Dict (K.Div 'K.RoundAway (P 1) (P 1) ~ P 1)
_test_Div_RoundAway_P1_P1 =  Dict
_test_Rem_RoundAway_P1_P1 :: Dict (K.Rem 'K.RoundAway (P 1) (P 1) ~ P 0)
_test_Rem_RoundAway_P1_P1 =  Dict
_test_Div_RoundAway_P1_P2 :: Dict (K.Div 'K.RoundAway (P 1) (P 2) ~ P 1)
_test_Div_RoundAway_P1_P2 =  Dict
_test_Rem_RoundAway_P1_P2 :: Dict (K.Rem 'K.RoundAway (P 1) (P 2) ~ N 1)
_test_Rem_RoundAway_P1_P2 =  Dict
_test_Div_RoundAway_P1_P3 :: Dict (K.Div 'K.RoundAway (P 1) (P 3) ~ P 1)
_test_Div_RoundAway_P1_P3 =  Dict
_test_Rem_RoundAway_P1_P3 :: Dict (K.Rem 'K.RoundAway (P 1) (P 3) ~ N 2)
_test_Rem_RoundAway_P1_P3 =  Dict
_test_Div_RoundAway_P1_P4 :: Dict (K.Div 'K.RoundAway (P 1) (P 4) ~ P 1)
_test_Div_RoundAway_P1_P4 =  Dict
_test_Rem_RoundAway_P1_P4 :: Dict (K.Rem 'K.RoundAway (P 1) (P 4) ~ N 3)
_test_Rem_RoundAway_P1_P4 =  Dict
_test_Div_RoundAway_P2_N1 :: Dict (K.Div 'K.RoundAway (P 2) (N 1) ~ N 2)
_test_Div_RoundAway_P2_N1 =  Dict
_test_Rem_RoundAway_P2_N1 :: Dict (K.Rem 'K.RoundAway (P 2) (N 1) ~ P 0)
_test_Rem_RoundAway_P2_N1 =  Dict
_test_Div_RoundAway_P2_N2 :: Dict (K.Div 'K.RoundAway (P 2) (N 2) ~ N 1)
_test_Div_RoundAway_P2_N2 =  Dict
_test_Rem_RoundAway_P2_N2 :: Dict (K.Rem 'K.RoundAway (P 2) (N 2) ~ P 0)
_test_Rem_RoundAway_P2_N2 =  Dict
_test_Div_RoundAway_P2_N3 :: Dict (K.Div 'K.RoundAway (P 2) (N 3) ~ N 1)
_test_Div_RoundAway_P2_N3 =  Dict
_test_Rem_RoundAway_P2_N3 :: Dict (K.Rem 'K.RoundAway (P 2) (N 3) ~ N 1)
_test_Rem_RoundAway_P2_N3 =  Dict
_test_Div_RoundAway_P2_N4 :: Dict (K.Div 'K.RoundAway (P 2) (N 4) ~ N 1)
_test_Div_RoundAway_P2_N4 =  Dict
_test_Rem_RoundAway_P2_N4 :: Dict (K.Rem 'K.RoundAway (P 2) (N 4) ~ N 2)
_test_Rem_RoundAway_P2_N4 =  Dict
_test_Div_RoundAway_P2_P1 :: Dict (K.Div 'K.RoundAway (P 2) (P 1) ~ P 2)
_test_Div_RoundAway_P2_P1 =  Dict
_test_Rem_RoundAway_P2_P1 :: Dict (K.Rem 'K.RoundAway (P 2) (P 1) ~ P 0)
_test_Rem_RoundAway_P2_P1 =  Dict
_test_Div_RoundAway_P2_P2 :: Dict (K.Div 'K.RoundAway (P 2) (P 2) ~ P 1)
_test_Div_RoundAway_P2_P2 =  Dict
_test_Rem_RoundAway_P2_P2 :: Dict (K.Rem 'K.RoundAway (P 2) (P 2) ~ P 0)
_test_Rem_RoundAway_P2_P2 =  Dict
_test_Div_RoundAway_P2_P3 :: Dict (K.Div 'K.RoundAway (P 2) (P 3) ~ P 1)
_test_Div_RoundAway_P2_P3 =  Dict
_test_Rem_RoundAway_P2_P3 :: Dict (K.Rem 'K.RoundAway (P 2) (P 3) ~ N 1)
_test_Rem_RoundAway_P2_P3 =  Dict
_test_Div_RoundAway_P2_P4 :: Dict (K.Div 'K.RoundAway (P 2) (P 4) ~ P 1)
_test_Div_RoundAway_P2_P4 =  Dict
_test_Rem_RoundAway_P2_P4 :: Dict (K.Rem 'K.RoundAway (P 2) (P 4) ~ N 2)
_test_Rem_RoundAway_P2_P4 =  Dict
_test_Div_RoundAway_P3_N1 :: Dict (K.Div 'K.RoundAway (P 3) (N 1) ~ N 3)
_test_Div_RoundAway_P3_N1 =  Dict
_test_Rem_RoundAway_P3_N1 :: Dict (K.Rem 'K.RoundAway (P 3) (N 1) ~ P 0)
_test_Rem_RoundAway_P3_N1 =  Dict
_test_Div_RoundAway_P3_N2 :: Dict (K.Div 'K.RoundAway (P 3) (N 2) ~ N 2)
_test_Div_RoundAway_P3_N2 =  Dict
_test_Rem_RoundAway_P3_N2 :: Dict (K.Rem 'K.RoundAway (P 3) (N 2) ~ N 1)
_test_Rem_RoundAway_P3_N2 =  Dict
_test_Div_RoundAway_P3_N3 :: Dict (K.Div 'K.RoundAway (P 3) (N 3) ~ N 1)
_test_Div_RoundAway_P3_N3 =  Dict
_test_Rem_RoundAway_P3_N3 :: Dict (K.Rem 'K.RoundAway (P 3) (N 3) ~ P 0)
_test_Rem_RoundAway_P3_N3 =  Dict
_test_Div_RoundAway_P3_N4 :: Dict (K.Div 'K.RoundAway (P 3) (N 4) ~ N 1)
_test_Div_RoundAway_P3_N4 =  Dict
_test_Rem_RoundAway_P3_N4 :: Dict (K.Rem 'K.RoundAway (P 3) (N 4) ~ N 1)
_test_Rem_RoundAway_P3_N4 =  Dict
_test_Div_RoundAway_P3_P1 :: Dict (K.Div 'K.RoundAway (P 3) (P 1) ~ P 3)
_test_Div_RoundAway_P3_P1 =  Dict
_test_Rem_RoundAway_P3_P1 :: Dict (K.Rem 'K.RoundAway (P 3) (P 1) ~ P 0)
_test_Rem_RoundAway_P3_P1 =  Dict
_test_Div_RoundAway_P3_P2 :: Dict (K.Div 'K.RoundAway (P 3) (P 2) ~ P 2)
_test_Div_RoundAway_P3_P2 =  Dict
_test_Rem_RoundAway_P3_P2 :: Dict (K.Rem 'K.RoundAway (P 3) (P 2) ~ N 1)
_test_Rem_RoundAway_P3_P2 =  Dict
_test_Div_RoundAway_P3_P3 :: Dict (K.Div 'K.RoundAway (P 3) (P 3) ~ P 1)
_test_Div_RoundAway_P3_P3 =  Dict
_test_Rem_RoundAway_P3_P3 :: Dict (K.Rem 'K.RoundAway (P 3) (P 3) ~ P 0)
_test_Rem_RoundAway_P3_P3 =  Dict
_test_Div_RoundAway_P3_P4 :: Dict (K.Div 'K.RoundAway (P 3) (P 4) ~ P 1)
_test_Div_RoundAway_P3_P4 =  Dict
_test_Rem_RoundAway_P3_P4 :: Dict (K.Rem 'K.RoundAway (P 3) (P 4) ~ N 1)
_test_Rem_RoundAway_P3_P4 =  Dict
_test_Div_RoundAway_P4_N1 :: Dict (K.Div 'K.RoundAway (P 4) (N 1) ~ N 4)
_test_Div_RoundAway_P4_N1 =  Dict
_test_Rem_RoundAway_P4_N1 :: Dict (K.Rem 'K.RoundAway (P 4) (N 1) ~ P 0)
_test_Rem_RoundAway_P4_N1 =  Dict
_test_Div_RoundAway_P4_N2 :: Dict (K.Div 'K.RoundAway (P 4) (N 2) ~ N 2)
_test_Div_RoundAway_P4_N2 =  Dict
_test_Rem_RoundAway_P4_N2 :: Dict (K.Rem 'K.RoundAway (P 4) (N 2) ~ P 0)
_test_Rem_RoundAway_P4_N2 =  Dict
_test_Div_RoundAway_P4_N3 :: Dict (K.Div 'K.RoundAway (P 4) (N 3) ~ N 2)
_test_Div_RoundAway_P4_N3 =  Dict
_test_Rem_RoundAway_P4_N3 :: Dict (K.Rem 'K.RoundAway (P 4) (N 3) ~ N 2)
_test_Rem_RoundAway_P4_N3 =  Dict
_test_Div_RoundAway_P4_N4 :: Dict (K.Div 'K.RoundAway (P 4) (N 4) ~ N 1)
_test_Div_RoundAway_P4_N4 =  Dict
_test_Rem_RoundAway_P4_N4 :: Dict (K.Rem 'K.RoundAway (P 4) (N 4) ~ P 0)
_test_Rem_RoundAway_P4_N4 =  Dict
_test_Div_RoundAway_P4_P1 :: Dict (K.Div 'K.RoundAway (P 4) (P 1) ~ P 4)
_test_Div_RoundAway_P4_P1 =  Dict
_test_Rem_RoundAway_P4_P1 :: Dict (K.Rem 'K.RoundAway (P 4) (P 1) ~ P 0)
_test_Rem_RoundAway_P4_P1 =  Dict
_test_Div_RoundAway_P4_P2 :: Dict (K.Div 'K.RoundAway (P 4) (P 2) ~ P 2)
_test_Div_RoundAway_P4_P2 =  Dict
_test_Rem_RoundAway_P4_P2 :: Dict (K.Rem 'K.RoundAway (P 4) (P 2) ~ P 0)
_test_Rem_RoundAway_P4_P2 =  Dict
_test_Div_RoundAway_P4_P3 :: Dict (K.Div 'K.RoundAway (P 4) (P 3) ~ P 2)
_test_Div_RoundAway_P4_P3 =  Dict
_test_Rem_RoundAway_P4_P3 :: Dict (K.Rem 'K.RoundAway (P 4) (P 3) ~ N 2)
_test_Rem_RoundAway_P4_P3 =  Dict
_test_Div_RoundAway_P4_P4 :: Dict (K.Div 'K.RoundAway (P 4) (P 4) ~ P 1)
_test_Div_RoundAway_P4_P4 =  Dict
_test_Rem_RoundAway_P4_P4 :: Dict (K.Rem 'K.RoundAway (P 4) (P 4) ~ P 0)
_test_Rem_RoundAway_P4_P4 =  Dict
_test_Div_RoundDown_N1_N1 :: Dict (K.Div 'K.RoundDown (N 1) (N 1) ~ P 1)
_test_Div_RoundDown_N1_N1 =  Dict
_test_Rem_RoundDown_N1_N1 :: Dict (K.Rem 'K.RoundDown (N 1) (N 1) ~ P 0)
_test_Rem_RoundDown_N1_N1 =  Dict
_test_Div_RoundDown_N1_N2 :: Dict (K.Div 'K.RoundDown (N 1) (N 2) ~ P 0)
_test_Div_RoundDown_N1_N2 =  Dict
_test_Rem_RoundDown_N1_N2 :: Dict (K.Rem 'K.RoundDown (N 1) (N 2) ~ N 1)
_test_Rem_RoundDown_N1_N2 =  Dict
_test_Div_RoundDown_N1_N3 :: Dict (K.Div 'K.RoundDown (N 1) (N 3) ~ P 0)
_test_Div_RoundDown_N1_N3 =  Dict
_test_Rem_RoundDown_N1_N3 :: Dict (K.Rem 'K.RoundDown (N 1) (N 3) ~ N 1)
_test_Rem_RoundDown_N1_N3 =  Dict
_test_Div_RoundDown_N1_N4 :: Dict (K.Div 'K.RoundDown (N 1) (N 4) ~ P 0)
_test_Div_RoundDown_N1_N4 =  Dict
_test_Rem_RoundDown_N1_N4 :: Dict (K.Rem 'K.RoundDown (N 1) (N 4) ~ N 1)
_test_Rem_RoundDown_N1_N4 =  Dict
_test_Div_RoundDown_N1_P1 :: Dict (K.Div 'K.RoundDown (N 1) (P 1) ~ N 1)
_test_Div_RoundDown_N1_P1 =  Dict
_test_Rem_RoundDown_N1_P1 :: Dict (K.Rem 'K.RoundDown (N 1) (P 1) ~ P 0)
_test_Rem_RoundDown_N1_P1 =  Dict
_test_Div_RoundDown_N1_P2 :: Dict (K.Div 'K.RoundDown (N 1) (P 2) ~ N 1)
_test_Div_RoundDown_N1_P2 =  Dict
_test_Rem_RoundDown_N1_P2 :: Dict (K.Rem 'K.RoundDown (N 1) (P 2) ~ P 1)
_test_Rem_RoundDown_N1_P2 =  Dict
_test_Div_RoundDown_N1_P3 :: Dict (K.Div 'K.RoundDown (N 1) (P 3) ~ N 1)
_test_Div_RoundDown_N1_P3 =  Dict
_test_Rem_RoundDown_N1_P3 :: Dict (K.Rem 'K.RoundDown (N 1) (P 3) ~ P 2)
_test_Rem_RoundDown_N1_P3 =  Dict
_test_Div_RoundDown_N1_P4 :: Dict (K.Div 'K.RoundDown (N 1) (P 4) ~ N 1)
_test_Div_RoundDown_N1_P4 =  Dict
_test_Rem_RoundDown_N1_P4 :: Dict (K.Rem 'K.RoundDown (N 1) (P 4) ~ P 3)
_test_Rem_RoundDown_N1_P4 =  Dict
_test_Div_RoundDown_N2_N1 :: Dict (K.Div 'K.RoundDown (N 2) (N 1) ~ P 2)
_test_Div_RoundDown_N2_N1 =  Dict
_test_Rem_RoundDown_N2_N1 :: Dict (K.Rem 'K.RoundDown (N 2) (N 1) ~ P 0)
_test_Rem_RoundDown_N2_N1 =  Dict
_test_Div_RoundDown_N2_N2 :: Dict (K.Div 'K.RoundDown (N 2) (N 2) ~ P 1)
_test_Div_RoundDown_N2_N2 =  Dict
_test_Rem_RoundDown_N2_N2 :: Dict (K.Rem 'K.RoundDown (N 2) (N 2) ~ P 0)
_test_Rem_RoundDown_N2_N2 =  Dict
_test_Div_RoundDown_N2_N3 :: Dict (K.Div 'K.RoundDown (N 2) (N 3) ~ P 0)
_test_Div_RoundDown_N2_N3 =  Dict
_test_Rem_RoundDown_N2_N3 :: Dict (K.Rem 'K.RoundDown (N 2) (N 3) ~ N 2)
_test_Rem_RoundDown_N2_N3 =  Dict
_test_Div_RoundDown_N2_N4 :: Dict (K.Div 'K.RoundDown (N 2) (N 4) ~ P 0)
_test_Div_RoundDown_N2_N4 =  Dict
_test_Rem_RoundDown_N2_N4 :: Dict (K.Rem 'K.RoundDown (N 2) (N 4) ~ N 2)
_test_Rem_RoundDown_N2_N4 =  Dict
_test_Div_RoundDown_N2_P1 :: Dict (K.Div 'K.RoundDown (N 2) (P 1) ~ N 2)
_test_Div_RoundDown_N2_P1 =  Dict
_test_Rem_RoundDown_N2_P1 :: Dict (K.Rem 'K.RoundDown (N 2) (P 1) ~ P 0)
_test_Rem_RoundDown_N2_P1 =  Dict
_test_Div_RoundDown_N2_P2 :: Dict (K.Div 'K.RoundDown (N 2) (P 2) ~ N 1)
_test_Div_RoundDown_N2_P2 =  Dict
_test_Rem_RoundDown_N2_P2 :: Dict (K.Rem 'K.RoundDown (N 2) (P 2) ~ P 0)
_test_Rem_RoundDown_N2_P2 =  Dict
_test_Div_RoundDown_N2_P3 :: Dict (K.Div 'K.RoundDown (N 2) (P 3) ~ N 1)
_test_Div_RoundDown_N2_P3 =  Dict
_test_Rem_RoundDown_N2_P3 :: Dict (K.Rem 'K.RoundDown (N 2) (P 3) ~ P 1)
_test_Rem_RoundDown_N2_P3 =  Dict
_test_Div_RoundDown_N2_P4 :: Dict (K.Div 'K.RoundDown (N 2) (P 4) ~ N 1)
_test_Div_RoundDown_N2_P4 =  Dict
_test_Rem_RoundDown_N2_P4 :: Dict (K.Rem 'K.RoundDown (N 2) (P 4) ~ P 2)
_test_Rem_RoundDown_N2_P4 =  Dict
_test_Div_RoundDown_N3_N1 :: Dict (K.Div 'K.RoundDown (N 3) (N 1) ~ P 3)
_test_Div_RoundDown_N3_N1 =  Dict
_test_Rem_RoundDown_N3_N1 :: Dict (K.Rem 'K.RoundDown (N 3) (N 1) ~ P 0)
_test_Rem_RoundDown_N3_N1 =  Dict
_test_Div_RoundDown_N3_N2 :: Dict (K.Div 'K.RoundDown (N 3) (N 2) ~ P 1)
_test_Div_RoundDown_N3_N2 =  Dict
_test_Rem_RoundDown_N3_N2 :: Dict (K.Rem 'K.RoundDown (N 3) (N 2) ~ N 1)
_test_Rem_RoundDown_N3_N2 =  Dict
_test_Div_RoundDown_N3_N3 :: Dict (K.Div 'K.RoundDown (N 3) (N 3) ~ P 1)
_test_Div_RoundDown_N3_N3 =  Dict
_test_Rem_RoundDown_N3_N3 :: Dict (K.Rem 'K.RoundDown (N 3) (N 3) ~ P 0)
_test_Rem_RoundDown_N3_N3 =  Dict
_test_Div_RoundDown_N3_N4 :: Dict (K.Div 'K.RoundDown (N 3) (N 4) ~ P 0)
_test_Div_RoundDown_N3_N4 =  Dict
_test_Rem_RoundDown_N3_N4 :: Dict (K.Rem 'K.RoundDown (N 3) (N 4) ~ N 3)
_test_Rem_RoundDown_N3_N4 =  Dict
_test_Div_RoundDown_N3_P1 :: Dict (K.Div 'K.RoundDown (N 3) (P 1) ~ N 3)
_test_Div_RoundDown_N3_P1 =  Dict
_test_Rem_RoundDown_N3_P1 :: Dict (K.Rem 'K.RoundDown (N 3) (P 1) ~ P 0)
_test_Rem_RoundDown_N3_P1 =  Dict
_test_Div_RoundDown_N3_P2 :: Dict (K.Div 'K.RoundDown (N 3) (P 2) ~ N 2)
_test_Div_RoundDown_N3_P2 =  Dict
_test_Rem_RoundDown_N3_P2 :: Dict (K.Rem 'K.RoundDown (N 3) (P 2) ~ P 1)
_test_Rem_RoundDown_N3_P2 =  Dict
_test_Div_RoundDown_N3_P3 :: Dict (K.Div 'K.RoundDown (N 3) (P 3) ~ N 1)
_test_Div_RoundDown_N3_P3 =  Dict
_test_Rem_RoundDown_N3_P3 :: Dict (K.Rem 'K.RoundDown (N 3) (P 3) ~ P 0)
_test_Rem_RoundDown_N3_P3 =  Dict
_test_Div_RoundDown_N3_P4 :: Dict (K.Div 'K.RoundDown (N 3) (P 4) ~ N 1)
_test_Div_RoundDown_N3_P4 =  Dict
_test_Rem_RoundDown_N3_P4 :: Dict (K.Rem 'K.RoundDown (N 3) (P 4) ~ P 1)
_test_Rem_RoundDown_N3_P4 =  Dict
_test_Div_RoundDown_N4_N1 :: Dict (K.Div 'K.RoundDown (N 4) (N 1) ~ P 4)
_test_Div_RoundDown_N4_N1 =  Dict
_test_Rem_RoundDown_N4_N1 :: Dict (K.Rem 'K.RoundDown (N 4) (N 1) ~ P 0)
_test_Rem_RoundDown_N4_N1 =  Dict
_test_Div_RoundDown_N4_N2 :: Dict (K.Div 'K.RoundDown (N 4) (N 2) ~ P 2)
_test_Div_RoundDown_N4_N2 =  Dict
_test_Rem_RoundDown_N4_N2 :: Dict (K.Rem 'K.RoundDown (N 4) (N 2) ~ P 0)
_test_Rem_RoundDown_N4_N2 =  Dict
_test_Div_RoundDown_N4_N3 :: Dict (K.Div 'K.RoundDown (N 4) (N 3) ~ P 1)
_test_Div_RoundDown_N4_N3 =  Dict
_test_Rem_RoundDown_N4_N3 :: Dict (K.Rem 'K.RoundDown (N 4) (N 3) ~ N 1)
_test_Rem_RoundDown_N4_N3 =  Dict
_test_Div_RoundDown_N4_N4 :: Dict (K.Div 'K.RoundDown (N 4) (N 4) ~ P 1)
_test_Div_RoundDown_N4_N4 =  Dict
_test_Rem_RoundDown_N4_N4 :: Dict (K.Rem 'K.RoundDown (N 4) (N 4) ~ P 0)
_test_Rem_RoundDown_N4_N4 =  Dict
_test_Div_RoundDown_N4_P1 :: Dict (K.Div 'K.RoundDown (N 4) (P 1) ~ N 4)
_test_Div_RoundDown_N4_P1 =  Dict
_test_Rem_RoundDown_N4_P1 :: Dict (K.Rem 'K.RoundDown (N 4) (P 1) ~ P 0)
_test_Rem_RoundDown_N4_P1 =  Dict
_test_Div_RoundDown_N4_P2 :: Dict (K.Div 'K.RoundDown (N 4) (P 2) ~ N 2)
_test_Div_RoundDown_N4_P2 =  Dict
_test_Rem_RoundDown_N4_P2 :: Dict (K.Rem 'K.RoundDown (N 4) (P 2) ~ P 0)
_test_Rem_RoundDown_N4_P2 =  Dict
_test_Div_RoundDown_N4_P3 :: Dict (K.Div 'K.RoundDown (N 4) (P 3) ~ N 2)
_test_Div_RoundDown_N4_P3 =  Dict
_test_Rem_RoundDown_N4_P3 :: Dict (K.Rem 'K.RoundDown (N 4) (P 3) ~ P 2)
_test_Rem_RoundDown_N4_P3 =  Dict
_test_Div_RoundDown_N4_P4 :: Dict (K.Div 'K.RoundDown (N 4) (P 4) ~ N 1)
_test_Div_RoundDown_N4_P4 =  Dict
_test_Rem_RoundDown_N4_P4 :: Dict (K.Rem 'K.RoundDown (N 4) (P 4) ~ P 0)
_test_Rem_RoundDown_N4_P4 =  Dict
_test_Div_RoundDown_P0_N1 :: Dict (K.Div 'K.RoundDown (P 0) (N 1) ~ P 0)
_test_Div_RoundDown_P0_N1 =  Dict
_test_Rem_RoundDown_P0_N1 :: Dict (K.Rem 'K.RoundDown (P 0) (N 1) ~ P 0)
_test_Rem_RoundDown_P0_N1 =  Dict
_test_Div_RoundDown_P0_N2 :: Dict (K.Div 'K.RoundDown (P 0) (N 2) ~ P 0)
_test_Div_RoundDown_P0_N2 =  Dict
_test_Rem_RoundDown_P0_N2 :: Dict (K.Rem 'K.RoundDown (P 0) (N 2) ~ P 0)
_test_Rem_RoundDown_P0_N2 =  Dict
_test_Div_RoundDown_P0_N3 :: Dict (K.Div 'K.RoundDown (P 0) (N 3) ~ P 0)
_test_Div_RoundDown_P0_N3 =  Dict
_test_Rem_RoundDown_P0_N3 :: Dict (K.Rem 'K.RoundDown (P 0) (N 3) ~ P 0)
_test_Rem_RoundDown_P0_N3 =  Dict
_test_Div_RoundDown_P0_N4 :: Dict (K.Div 'K.RoundDown (P 0) (N 4) ~ P 0)
_test_Div_RoundDown_P0_N4 =  Dict
_test_Rem_RoundDown_P0_N4 :: Dict (K.Rem 'K.RoundDown (P 0) (N 4) ~ P 0)
_test_Rem_RoundDown_P0_N4 =  Dict
_test_Div_RoundDown_P0_P1 :: Dict (K.Div 'K.RoundDown (P 0) (P 1) ~ P 0)
_test_Div_RoundDown_P0_P1 =  Dict
_test_Rem_RoundDown_P0_P1 :: Dict (K.Rem 'K.RoundDown (P 0) (P 1) ~ P 0)
_test_Rem_RoundDown_P0_P1 =  Dict
_test_Div_RoundDown_P0_P2 :: Dict (K.Div 'K.RoundDown (P 0) (P 2) ~ P 0)
_test_Div_RoundDown_P0_P2 =  Dict
_test_Rem_RoundDown_P0_P2 :: Dict (K.Rem 'K.RoundDown (P 0) (P 2) ~ P 0)
_test_Rem_RoundDown_P0_P2 =  Dict
_test_Div_RoundDown_P0_P3 :: Dict (K.Div 'K.RoundDown (P 0) (P 3) ~ P 0)
_test_Div_RoundDown_P0_P3 =  Dict
_test_Rem_RoundDown_P0_P3 :: Dict (K.Rem 'K.RoundDown (P 0) (P 3) ~ P 0)
_test_Rem_RoundDown_P0_P3 =  Dict
_test_Div_RoundDown_P0_P4 :: Dict (K.Div 'K.RoundDown (P 0) (P 4) ~ P 0)
_test_Div_RoundDown_P0_P4 =  Dict
_test_Rem_RoundDown_P0_P4 :: Dict (K.Rem 'K.RoundDown (P 0) (P 4) ~ P 0)
_test_Rem_RoundDown_P0_P4 =  Dict
_test_Div_RoundDown_P1_N1 :: Dict (K.Div 'K.RoundDown (P 1) (N 1) ~ N 1)
_test_Div_RoundDown_P1_N1 =  Dict
_test_Rem_RoundDown_P1_N1 :: Dict (K.Rem 'K.RoundDown (P 1) (N 1) ~ P 0)
_test_Rem_RoundDown_P1_N1 =  Dict
_test_Div_RoundDown_P1_N2 :: Dict (K.Div 'K.RoundDown (P 1) (N 2) ~ N 1)
_test_Div_RoundDown_P1_N2 =  Dict
_test_Rem_RoundDown_P1_N2 :: Dict (K.Rem 'K.RoundDown (P 1) (N 2) ~ N 1)
_test_Rem_RoundDown_P1_N2 =  Dict
_test_Div_RoundDown_P1_N3 :: Dict (K.Div 'K.RoundDown (P 1) (N 3) ~ N 1)
_test_Div_RoundDown_P1_N3 =  Dict
_test_Rem_RoundDown_P1_N3 :: Dict (K.Rem 'K.RoundDown (P 1) (N 3) ~ N 2)
_test_Rem_RoundDown_P1_N3 =  Dict
_test_Div_RoundDown_P1_N4 :: Dict (K.Div 'K.RoundDown (P 1) (N 4) ~ N 1)
_test_Div_RoundDown_P1_N4 =  Dict
_test_Rem_RoundDown_P1_N4 :: Dict (K.Rem 'K.RoundDown (P 1) (N 4) ~ N 3)
_test_Rem_RoundDown_P1_N4 =  Dict
_test_Div_RoundDown_P1_P1 :: Dict (K.Div 'K.RoundDown (P 1) (P 1) ~ P 1)
_test_Div_RoundDown_P1_P1 =  Dict
_test_Rem_RoundDown_P1_P1 :: Dict (K.Rem 'K.RoundDown (P 1) (P 1) ~ P 0)
_test_Rem_RoundDown_P1_P1 =  Dict
_test_Div_RoundDown_P1_P2 :: Dict (K.Div 'K.RoundDown (P 1) (P 2) ~ P 0)
_test_Div_RoundDown_P1_P2 =  Dict
_test_Rem_RoundDown_P1_P2 :: Dict (K.Rem 'K.RoundDown (P 1) (P 2) ~ P 1)
_test_Rem_RoundDown_P1_P2 =  Dict
_test_Div_RoundDown_P1_P3 :: Dict (K.Div 'K.RoundDown (P 1) (P 3) ~ P 0)
_test_Div_RoundDown_P1_P3 =  Dict
_test_Rem_RoundDown_P1_P3 :: Dict (K.Rem 'K.RoundDown (P 1) (P 3) ~ P 1)
_test_Rem_RoundDown_P1_P3 =  Dict
_test_Div_RoundDown_P1_P4 :: Dict (K.Div 'K.RoundDown (P 1) (P 4) ~ P 0)
_test_Div_RoundDown_P1_P4 =  Dict
_test_Rem_RoundDown_P1_P4 :: Dict (K.Rem 'K.RoundDown (P 1) (P 4) ~ P 1)
_test_Rem_RoundDown_P1_P4 =  Dict
_test_Div_RoundDown_P2_N1 :: Dict (K.Div 'K.RoundDown (P 2) (N 1) ~ N 2)
_test_Div_RoundDown_P2_N1 =  Dict
_test_Rem_RoundDown_P2_N1 :: Dict (K.Rem 'K.RoundDown (P 2) (N 1) ~ P 0)
_test_Rem_RoundDown_P2_N1 =  Dict
_test_Div_RoundDown_P2_N2 :: Dict (K.Div 'K.RoundDown (P 2) (N 2) ~ N 1)
_test_Div_RoundDown_P2_N2 =  Dict
_test_Rem_RoundDown_P2_N2 :: Dict (K.Rem 'K.RoundDown (P 2) (N 2) ~ P 0)
_test_Rem_RoundDown_P2_N2 =  Dict
_test_Div_RoundDown_P2_N3 :: Dict (K.Div 'K.RoundDown (P 2) (N 3) ~ N 1)
_test_Div_RoundDown_P2_N3 =  Dict
_test_Rem_RoundDown_P2_N3 :: Dict (K.Rem 'K.RoundDown (P 2) (N 3) ~ N 1)
_test_Rem_RoundDown_P2_N3 =  Dict
_test_Div_RoundDown_P2_N4 :: Dict (K.Div 'K.RoundDown (P 2) (N 4) ~ N 1)
_test_Div_RoundDown_P2_N4 =  Dict
_test_Rem_RoundDown_P2_N4 :: Dict (K.Rem 'K.RoundDown (P 2) (N 4) ~ N 2)
_test_Rem_RoundDown_P2_N4 =  Dict
_test_Div_RoundDown_P2_P1 :: Dict (K.Div 'K.RoundDown (P 2) (P 1) ~ P 2)
_test_Div_RoundDown_P2_P1 =  Dict
_test_Rem_RoundDown_P2_P1 :: Dict (K.Rem 'K.RoundDown (P 2) (P 1) ~ P 0)
_test_Rem_RoundDown_P2_P1 =  Dict
_test_Div_RoundDown_P2_P2 :: Dict (K.Div 'K.RoundDown (P 2) (P 2) ~ P 1)
_test_Div_RoundDown_P2_P2 =  Dict
_test_Rem_RoundDown_P2_P2 :: Dict (K.Rem 'K.RoundDown (P 2) (P 2) ~ P 0)
_test_Rem_RoundDown_P2_P2 =  Dict
_test_Div_RoundDown_P2_P3 :: Dict (K.Div 'K.RoundDown (P 2) (P 3) ~ P 0)
_test_Div_RoundDown_P2_P3 =  Dict
_test_Rem_RoundDown_P2_P3 :: Dict (K.Rem 'K.RoundDown (P 2) (P 3) ~ P 2)
_test_Rem_RoundDown_P2_P3 =  Dict
_test_Div_RoundDown_P2_P4 :: Dict (K.Div 'K.RoundDown (P 2) (P 4) ~ P 0)
_test_Div_RoundDown_P2_P4 =  Dict
_test_Rem_RoundDown_P2_P4 :: Dict (K.Rem 'K.RoundDown (P 2) (P 4) ~ P 2)
_test_Rem_RoundDown_P2_P4 =  Dict
_test_Div_RoundDown_P3_N1 :: Dict (K.Div 'K.RoundDown (P 3) (N 1) ~ N 3)
_test_Div_RoundDown_P3_N1 =  Dict
_test_Rem_RoundDown_P3_N1 :: Dict (K.Rem 'K.RoundDown (P 3) (N 1) ~ P 0)
_test_Rem_RoundDown_P3_N1 =  Dict
_test_Div_RoundDown_P3_N2 :: Dict (K.Div 'K.RoundDown (P 3) (N 2) ~ N 2)
_test_Div_RoundDown_P3_N2 =  Dict
_test_Rem_RoundDown_P3_N2 :: Dict (K.Rem 'K.RoundDown (P 3) (N 2) ~ N 1)
_test_Rem_RoundDown_P3_N2 =  Dict
_test_Div_RoundDown_P3_N3 :: Dict (K.Div 'K.RoundDown (P 3) (N 3) ~ N 1)
_test_Div_RoundDown_P3_N3 =  Dict
_test_Rem_RoundDown_P3_N3 :: Dict (K.Rem 'K.RoundDown (P 3) (N 3) ~ P 0)
_test_Rem_RoundDown_P3_N3 =  Dict
_test_Div_RoundDown_P3_N4 :: Dict (K.Div 'K.RoundDown (P 3) (N 4) ~ N 1)
_test_Div_RoundDown_P3_N4 =  Dict
_test_Rem_RoundDown_P3_N4 :: Dict (K.Rem 'K.RoundDown (P 3) (N 4) ~ N 1)
_test_Rem_RoundDown_P3_N4 =  Dict
_test_Div_RoundDown_P3_P1 :: Dict (K.Div 'K.RoundDown (P 3) (P 1) ~ P 3)
_test_Div_RoundDown_P3_P1 =  Dict
_test_Rem_RoundDown_P3_P1 :: Dict (K.Rem 'K.RoundDown (P 3) (P 1) ~ P 0)
_test_Rem_RoundDown_P3_P1 =  Dict
_test_Div_RoundDown_P3_P2 :: Dict (K.Div 'K.RoundDown (P 3) (P 2) ~ P 1)
_test_Div_RoundDown_P3_P2 =  Dict
_test_Rem_RoundDown_P3_P2 :: Dict (K.Rem 'K.RoundDown (P 3) (P 2) ~ P 1)
_test_Rem_RoundDown_P3_P2 =  Dict
_test_Div_RoundDown_P3_P3 :: Dict (K.Div 'K.RoundDown (P 3) (P 3) ~ P 1)
_test_Div_RoundDown_P3_P3 =  Dict
_test_Rem_RoundDown_P3_P3 :: Dict (K.Rem 'K.RoundDown (P 3) (P 3) ~ P 0)
_test_Rem_RoundDown_P3_P3 =  Dict
_test_Div_RoundDown_P3_P4 :: Dict (K.Div 'K.RoundDown (P 3) (P 4) ~ P 0)
_test_Div_RoundDown_P3_P4 =  Dict
_test_Rem_RoundDown_P3_P4 :: Dict (K.Rem 'K.RoundDown (P 3) (P 4) ~ P 3)
_test_Rem_RoundDown_P3_P4 =  Dict
_test_Div_RoundDown_P4_N1 :: Dict (K.Div 'K.RoundDown (P 4) (N 1) ~ N 4)
_test_Div_RoundDown_P4_N1 =  Dict
_test_Rem_RoundDown_P4_N1 :: Dict (K.Rem 'K.RoundDown (P 4) (N 1) ~ P 0)
_test_Rem_RoundDown_P4_N1 =  Dict
_test_Div_RoundDown_P4_N2 :: Dict (K.Div 'K.RoundDown (P 4) (N 2) ~ N 2)
_test_Div_RoundDown_P4_N2 =  Dict
_test_Rem_RoundDown_P4_N2 :: Dict (K.Rem 'K.RoundDown (P 4) (N 2) ~ P 0)
_test_Rem_RoundDown_P4_N2 =  Dict
_test_Div_RoundDown_P4_N3 :: Dict (K.Div 'K.RoundDown (P 4) (N 3) ~ N 2)
_test_Div_RoundDown_P4_N3 =  Dict
_test_Rem_RoundDown_P4_N3 :: Dict (K.Rem 'K.RoundDown (P 4) (N 3) ~ N 2)
_test_Rem_RoundDown_P4_N3 =  Dict
_test_Div_RoundDown_P4_N4 :: Dict (K.Div 'K.RoundDown (P 4) (N 4) ~ N 1)
_test_Div_RoundDown_P4_N4 =  Dict
_test_Rem_RoundDown_P4_N4 :: Dict (K.Rem 'K.RoundDown (P 4) (N 4) ~ P 0)
_test_Rem_RoundDown_P4_N4 =  Dict
_test_Div_RoundDown_P4_P1 :: Dict (K.Div 'K.RoundDown (P 4) (P 1) ~ P 4)
_test_Div_RoundDown_P4_P1 =  Dict
_test_Rem_RoundDown_P4_P1 :: Dict (K.Rem 'K.RoundDown (P 4) (P 1) ~ P 0)
_test_Rem_RoundDown_P4_P1 =  Dict
_test_Div_RoundDown_P4_P2 :: Dict (K.Div 'K.RoundDown (P 4) (P 2) ~ P 2)
_test_Div_RoundDown_P4_P2 =  Dict
_test_Rem_RoundDown_P4_P2 :: Dict (K.Rem 'K.RoundDown (P 4) (P 2) ~ P 0)
_test_Rem_RoundDown_P4_P2 =  Dict
_test_Div_RoundDown_P4_P3 :: Dict (K.Div 'K.RoundDown (P 4) (P 3) ~ P 1)
_test_Div_RoundDown_P4_P3 =  Dict
_test_Rem_RoundDown_P4_P3 :: Dict (K.Rem 'K.RoundDown (P 4) (P 3) ~ P 1)
_test_Rem_RoundDown_P4_P3 =  Dict
_test_Div_RoundDown_P4_P4 :: Dict (K.Div 'K.RoundDown (P 4) (P 4) ~ P 1)
_test_Div_RoundDown_P4_P4 =  Dict
_test_Rem_RoundDown_P4_P4 :: Dict (K.Rem 'K.RoundDown (P 4) (P 4) ~ P 0)
_test_Rem_RoundDown_P4_P4 =  Dict
_test_Div_RoundHalfAway_N1_N1 :: Dict (K.Div 'K.RoundHalfAway (N 1) (N 1) ~ P 1)
_test_Div_RoundHalfAway_N1_N1 =  Dict
_test_Rem_RoundHalfAway_N1_N1 :: Dict (K.Rem 'K.RoundHalfAway (N 1) (N 1) ~ P 0)
_test_Rem_RoundHalfAway_N1_N1 =  Dict
_test_Div_RoundHalfAway_N1_N2 :: Dict (K.Div 'K.RoundHalfAway (N 1) (N 2) ~ P 1)
_test_Div_RoundHalfAway_N1_N2 =  Dict
_test_Rem_RoundHalfAway_N1_N2 :: Dict (K.Rem 'K.RoundHalfAway (N 1) (N 2) ~ P 1)
_test_Rem_RoundHalfAway_N1_N2 =  Dict
_test_Div_RoundHalfAway_N1_N3 :: Dict (K.Div 'K.RoundHalfAway (N 1) (N 3) ~ P 0)
_test_Div_RoundHalfAway_N1_N3 =  Dict
_test_Rem_RoundHalfAway_N1_N3 :: Dict (K.Rem 'K.RoundHalfAway (N 1) (N 3) ~ N 1)
_test_Rem_RoundHalfAway_N1_N3 =  Dict
_test_Div_RoundHalfAway_N1_N4 :: Dict (K.Div 'K.RoundHalfAway (N 1) (N 4) ~ P 0)
_test_Div_RoundHalfAway_N1_N4 =  Dict
_test_Rem_RoundHalfAway_N1_N4 :: Dict (K.Rem 'K.RoundHalfAway (N 1) (N 4) ~ N 1)
_test_Rem_RoundHalfAway_N1_N4 =  Dict
_test_Div_RoundHalfAway_N1_P1 :: Dict (K.Div 'K.RoundHalfAway (N 1) (P 1) ~ N 1)
_test_Div_RoundHalfAway_N1_P1 =  Dict
_test_Rem_RoundHalfAway_N1_P1 :: Dict (K.Rem 'K.RoundHalfAway (N 1) (P 1) ~ P 0)
_test_Rem_RoundHalfAway_N1_P1 =  Dict
_test_Div_RoundHalfAway_N1_P2 :: Dict (K.Div 'K.RoundHalfAway (N 1) (P 2) ~ N 1)
_test_Div_RoundHalfAway_N1_P2 =  Dict
_test_Rem_RoundHalfAway_N1_P2 :: Dict (K.Rem 'K.RoundHalfAway (N 1) (P 2) ~ P 1)
_test_Rem_RoundHalfAway_N1_P2 =  Dict
_test_Div_RoundHalfAway_N1_P3 :: Dict (K.Div 'K.RoundHalfAway (N 1) (P 3) ~ P 0)
_test_Div_RoundHalfAway_N1_P3 =  Dict
_test_Rem_RoundHalfAway_N1_P3 :: Dict (K.Rem 'K.RoundHalfAway (N 1) (P 3) ~ N 1)
_test_Rem_RoundHalfAway_N1_P3 =  Dict
_test_Div_RoundHalfAway_N1_P4 :: Dict (K.Div 'K.RoundHalfAway (N 1) (P 4) ~ P 0)
_test_Div_RoundHalfAway_N1_P4 =  Dict
_test_Rem_RoundHalfAway_N1_P4 :: Dict (K.Rem 'K.RoundHalfAway (N 1) (P 4) ~ N 1)
_test_Rem_RoundHalfAway_N1_P4 =  Dict
_test_Div_RoundHalfAway_N2_N1 :: Dict (K.Div 'K.RoundHalfAway (N 2) (N 1) ~ P 2)
_test_Div_RoundHalfAway_N2_N1 =  Dict
_test_Rem_RoundHalfAway_N2_N1 :: Dict (K.Rem 'K.RoundHalfAway (N 2) (N 1) ~ P 0)
_test_Rem_RoundHalfAway_N2_N1 =  Dict
_test_Div_RoundHalfAway_N2_N2 :: Dict (K.Div 'K.RoundHalfAway (N 2) (N 2) ~ P 1)
_test_Div_RoundHalfAway_N2_N2 =  Dict
_test_Rem_RoundHalfAway_N2_N2 :: Dict (K.Rem 'K.RoundHalfAway (N 2) (N 2) ~ P 0)
_test_Rem_RoundHalfAway_N2_N2 =  Dict
_test_Div_RoundHalfAway_N2_N3 :: Dict (K.Div 'K.RoundHalfAway (N 2) (N 3) ~ P 1)
_test_Div_RoundHalfAway_N2_N3 =  Dict
_test_Rem_RoundHalfAway_N2_N3 :: Dict (K.Rem 'K.RoundHalfAway (N 2) (N 3) ~ P 1)
_test_Rem_RoundHalfAway_N2_N3 =  Dict
_test_Div_RoundHalfAway_N2_N4 :: Dict (K.Div 'K.RoundHalfAway (N 2) (N 4) ~ P 1)
_test_Div_RoundHalfAway_N2_N4 =  Dict
_test_Rem_RoundHalfAway_N2_N4 :: Dict (K.Rem 'K.RoundHalfAway (N 2) (N 4) ~ P 2)
_test_Rem_RoundHalfAway_N2_N4 =  Dict
_test_Div_RoundHalfAway_N2_P1 :: Dict (K.Div 'K.RoundHalfAway (N 2) (P 1) ~ N 2)
_test_Div_RoundHalfAway_N2_P1 =  Dict
_test_Rem_RoundHalfAway_N2_P1 :: Dict (K.Rem 'K.RoundHalfAway (N 2) (P 1) ~ P 0)
_test_Rem_RoundHalfAway_N2_P1 =  Dict
_test_Div_RoundHalfAway_N2_P2 :: Dict (K.Div 'K.RoundHalfAway (N 2) (P 2) ~ N 1)
_test_Div_RoundHalfAway_N2_P2 =  Dict
_test_Rem_RoundHalfAway_N2_P2 :: Dict (K.Rem 'K.RoundHalfAway (N 2) (P 2) ~ P 0)
_test_Rem_RoundHalfAway_N2_P2 =  Dict
_test_Div_RoundHalfAway_N2_P3 :: Dict (K.Div 'K.RoundHalfAway (N 2) (P 3) ~ N 1)
_test_Div_RoundHalfAway_N2_P3 =  Dict
_test_Rem_RoundHalfAway_N2_P3 :: Dict (K.Rem 'K.RoundHalfAway (N 2) (P 3) ~ P 1)
_test_Rem_RoundHalfAway_N2_P3 =  Dict
_test_Div_RoundHalfAway_N2_P4 :: Dict (K.Div 'K.RoundHalfAway (N 2) (P 4) ~ N 1)
_test_Div_RoundHalfAway_N2_P4 =  Dict
_test_Rem_RoundHalfAway_N2_P4 :: Dict (K.Rem 'K.RoundHalfAway (N 2) (P 4) ~ P 2)
_test_Rem_RoundHalfAway_N2_P4 =  Dict
_test_Div_RoundHalfAway_N3_N1 :: Dict (K.Div 'K.RoundHalfAway (N 3) (N 1) ~ P 3)
_test_Div_RoundHalfAway_N3_N1 =  Dict
_test_Rem_RoundHalfAway_N3_N1 :: Dict (K.Rem 'K.RoundHalfAway (N 3) (N 1) ~ P 0)
_test_Rem_RoundHalfAway_N3_N1 =  Dict
_test_Div_RoundHalfAway_N3_N2 :: Dict (K.Div 'K.RoundHalfAway (N 3) (N 2) ~ P 2)
_test_Div_RoundHalfAway_N3_N2 =  Dict
_test_Rem_RoundHalfAway_N3_N2 :: Dict (K.Rem 'K.RoundHalfAway (N 3) (N 2) ~ P 1)
_test_Rem_RoundHalfAway_N3_N2 =  Dict
_test_Div_RoundHalfAway_N3_N3 :: Dict (K.Div 'K.RoundHalfAway (N 3) (N 3) ~ P 1)
_test_Div_RoundHalfAway_N3_N3 =  Dict
_test_Rem_RoundHalfAway_N3_N3 :: Dict (K.Rem 'K.RoundHalfAway (N 3) (N 3) ~ P 0)
_test_Rem_RoundHalfAway_N3_N3 =  Dict
_test_Div_RoundHalfAway_N3_N4 :: Dict (K.Div 'K.RoundHalfAway (N 3) (N 4) ~ P 1)
_test_Div_RoundHalfAway_N3_N4 =  Dict
_test_Rem_RoundHalfAway_N3_N4 :: Dict (K.Rem 'K.RoundHalfAway (N 3) (N 4) ~ P 1)
_test_Rem_RoundHalfAway_N3_N4 =  Dict
_test_Div_RoundHalfAway_N3_P1 :: Dict (K.Div 'K.RoundHalfAway (N 3) (P 1) ~ N 3)
_test_Div_RoundHalfAway_N3_P1 =  Dict
_test_Rem_RoundHalfAway_N3_P1 :: Dict (K.Rem 'K.RoundHalfAway (N 3) (P 1) ~ P 0)
_test_Rem_RoundHalfAway_N3_P1 =  Dict
_test_Div_RoundHalfAway_N3_P2 :: Dict (K.Div 'K.RoundHalfAway (N 3) (P 2) ~ N 2)
_test_Div_RoundHalfAway_N3_P2 =  Dict
_test_Rem_RoundHalfAway_N3_P2 :: Dict (K.Rem 'K.RoundHalfAway (N 3) (P 2) ~ P 1)
_test_Rem_RoundHalfAway_N3_P2 =  Dict
_test_Div_RoundHalfAway_N3_P3 :: Dict (K.Div 'K.RoundHalfAway (N 3) (P 3) ~ N 1)
_test_Div_RoundHalfAway_N3_P3 =  Dict
_test_Rem_RoundHalfAway_N3_P3 :: Dict (K.Rem 'K.RoundHalfAway (N 3) (P 3) ~ P 0)
_test_Rem_RoundHalfAway_N3_P3 =  Dict
_test_Div_RoundHalfAway_N3_P4 :: Dict (K.Div 'K.RoundHalfAway (N 3) (P 4) ~ N 1)
_test_Div_RoundHalfAway_N3_P4 =  Dict
_test_Rem_RoundHalfAway_N3_P4 :: Dict (K.Rem 'K.RoundHalfAway (N 3) (P 4) ~ P 1)
_test_Rem_RoundHalfAway_N3_P4 =  Dict
_test_Div_RoundHalfAway_N4_N1 :: Dict (K.Div 'K.RoundHalfAway (N 4) (N 1) ~ P 4)
_test_Div_RoundHalfAway_N4_N1 =  Dict
_test_Rem_RoundHalfAway_N4_N1 :: Dict (K.Rem 'K.RoundHalfAway (N 4) (N 1) ~ P 0)
_test_Rem_RoundHalfAway_N4_N1 =  Dict
_test_Div_RoundHalfAway_N4_N2 :: Dict (K.Div 'K.RoundHalfAway (N 4) (N 2) ~ P 2)
_test_Div_RoundHalfAway_N4_N2 =  Dict
_test_Rem_RoundHalfAway_N4_N2 :: Dict (K.Rem 'K.RoundHalfAway (N 4) (N 2) ~ P 0)
_test_Rem_RoundHalfAway_N4_N2 =  Dict
_test_Div_RoundHalfAway_N4_N3 :: Dict (K.Div 'K.RoundHalfAway (N 4) (N 3) ~ P 1)
_test_Div_RoundHalfAway_N4_N3 =  Dict
_test_Rem_RoundHalfAway_N4_N3 :: Dict (K.Rem 'K.RoundHalfAway (N 4) (N 3) ~ N 1)
_test_Rem_RoundHalfAway_N4_N3 =  Dict
_test_Div_RoundHalfAway_N4_N4 :: Dict (K.Div 'K.RoundHalfAway (N 4) (N 4) ~ P 1)
_test_Div_RoundHalfAway_N4_N4 =  Dict
_test_Rem_RoundHalfAway_N4_N4 :: Dict (K.Rem 'K.RoundHalfAway (N 4) (N 4) ~ P 0)
_test_Rem_RoundHalfAway_N4_N4 =  Dict
_test_Div_RoundHalfAway_N4_P1 :: Dict (K.Div 'K.RoundHalfAway (N 4) (P 1) ~ N 4)
_test_Div_RoundHalfAway_N4_P1 =  Dict
_test_Rem_RoundHalfAway_N4_P1 :: Dict (K.Rem 'K.RoundHalfAway (N 4) (P 1) ~ P 0)
_test_Rem_RoundHalfAway_N4_P1 =  Dict
_test_Div_RoundHalfAway_N4_P2 :: Dict (K.Div 'K.RoundHalfAway (N 4) (P 2) ~ N 2)
_test_Div_RoundHalfAway_N4_P2 =  Dict
_test_Rem_RoundHalfAway_N4_P2 :: Dict (K.Rem 'K.RoundHalfAway (N 4) (P 2) ~ P 0)
_test_Rem_RoundHalfAway_N4_P2 =  Dict
_test_Div_RoundHalfAway_N4_P3 :: Dict (K.Div 'K.RoundHalfAway (N 4) (P 3) ~ N 1)
_test_Div_RoundHalfAway_N4_P3 =  Dict
_test_Rem_RoundHalfAway_N4_P3 :: Dict (K.Rem 'K.RoundHalfAway (N 4) (P 3) ~ N 1)
_test_Rem_RoundHalfAway_N4_P3 =  Dict
_test_Div_RoundHalfAway_N4_P4 :: Dict (K.Div 'K.RoundHalfAway (N 4) (P 4) ~ N 1)
_test_Div_RoundHalfAway_N4_P4 =  Dict
_test_Rem_RoundHalfAway_N4_P4 :: Dict (K.Rem 'K.RoundHalfAway (N 4) (P 4) ~ P 0)
_test_Rem_RoundHalfAway_N4_P4 =  Dict
_test_Div_RoundHalfAway_P0_N1 :: Dict (K.Div 'K.RoundHalfAway (P 0) (N 1) ~ P 0)
_test_Div_RoundHalfAway_P0_N1 =  Dict
_test_Rem_RoundHalfAway_P0_N1 :: Dict (K.Rem 'K.RoundHalfAway (P 0) (N 1) ~ P 0)
_test_Rem_RoundHalfAway_P0_N1 =  Dict
_test_Div_RoundHalfAway_P0_N2 :: Dict (K.Div 'K.RoundHalfAway (P 0) (N 2) ~ P 0)
_test_Div_RoundHalfAway_P0_N2 =  Dict
_test_Rem_RoundHalfAway_P0_N2 :: Dict (K.Rem 'K.RoundHalfAway (P 0) (N 2) ~ P 0)
_test_Rem_RoundHalfAway_P0_N2 =  Dict
_test_Div_RoundHalfAway_P0_N3 :: Dict (K.Div 'K.RoundHalfAway (P 0) (N 3) ~ P 0)
_test_Div_RoundHalfAway_P0_N3 =  Dict
_test_Rem_RoundHalfAway_P0_N3 :: Dict (K.Rem 'K.RoundHalfAway (P 0) (N 3) ~ P 0)
_test_Rem_RoundHalfAway_P0_N3 =  Dict
_test_Div_RoundHalfAway_P0_N4 :: Dict (K.Div 'K.RoundHalfAway (P 0) (N 4) ~ P 0)
_test_Div_RoundHalfAway_P0_N4 =  Dict
_test_Rem_RoundHalfAway_P0_N4 :: Dict (K.Rem 'K.RoundHalfAway (P 0) (N 4) ~ P 0)
_test_Rem_RoundHalfAway_P0_N4 =  Dict
_test_Div_RoundHalfAway_P0_P1 :: Dict (K.Div 'K.RoundHalfAway (P 0) (P 1) ~ P 0)
_test_Div_RoundHalfAway_P0_P1 =  Dict
_test_Rem_RoundHalfAway_P0_P1 :: Dict (K.Rem 'K.RoundHalfAway (P 0) (P 1) ~ P 0)
_test_Rem_RoundHalfAway_P0_P1 =  Dict
_test_Div_RoundHalfAway_P0_P2 :: Dict (K.Div 'K.RoundHalfAway (P 0) (P 2) ~ P 0)
_test_Div_RoundHalfAway_P0_P2 =  Dict
_test_Rem_RoundHalfAway_P0_P2 :: Dict (K.Rem 'K.RoundHalfAway (P 0) (P 2) ~ P 0)
_test_Rem_RoundHalfAway_P0_P2 =  Dict
_test_Div_RoundHalfAway_P0_P3 :: Dict (K.Div 'K.RoundHalfAway (P 0) (P 3) ~ P 0)
_test_Div_RoundHalfAway_P0_P3 =  Dict
_test_Rem_RoundHalfAway_P0_P3 :: Dict (K.Rem 'K.RoundHalfAway (P 0) (P 3) ~ P 0)
_test_Rem_RoundHalfAway_P0_P3 =  Dict
_test_Div_RoundHalfAway_P0_P4 :: Dict (K.Div 'K.RoundHalfAway (P 0) (P 4) ~ P 0)
_test_Div_RoundHalfAway_P0_P4 =  Dict
_test_Rem_RoundHalfAway_P0_P4 :: Dict (K.Rem 'K.RoundHalfAway (P 0) (P 4) ~ P 0)
_test_Rem_RoundHalfAway_P0_P4 =  Dict
_test_Div_RoundHalfAway_P1_N1 :: Dict (K.Div 'K.RoundHalfAway (P 1) (N 1) ~ N 1)
_test_Div_RoundHalfAway_P1_N1 =  Dict
_test_Rem_RoundHalfAway_P1_N1 :: Dict (K.Rem 'K.RoundHalfAway (P 1) (N 1) ~ P 0)
_test_Rem_RoundHalfAway_P1_N1 =  Dict
_test_Div_RoundHalfAway_P1_N2 :: Dict (K.Div 'K.RoundHalfAway (P 1) (N 2) ~ N 1)
_test_Div_RoundHalfAway_P1_N2 =  Dict
_test_Rem_RoundHalfAway_P1_N2 :: Dict (K.Rem 'K.RoundHalfAway (P 1) (N 2) ~ N 1)
_test_Rem_RoundHalfAway_P1_N2 =  Dict
_test_Div_RoundHalfAway_P1_N3 :: Dict (K.Div 'K.RoundHalfAway (P 1) (N 3) ~ P 0)
_test_Div_RoundHalfAway_P1_N3 =  Dict
_test_Rem_RoundHalfAway_P1_N3 :: Dict (K.Rem 'K.RoundHalfAway (P 1) (N 3) ~ P 1)
_test_Rem_RoundHalfAway_P1_N3 =  Dict
_test_Div_RoundHalfAway_P1_N4 :: Dict (K.Div 'K.RoundHalfAway (P 1) (N 4) ~ P 0)
_test_Div_RoundHalfAway_P1_N4 =  Dict
_test_Rem_RoundHalfAway_P1_N4 :: Dict (K.Rem 'K.RoundHalfAway (P 1) (N 4) ~ P 1)
_test_Rem_RoundHalfAway_P1_N4 =  Dict
_test_Div_RoundHalfAway_P1_P1 :: Dict (K.Div 'K.RoundHalfAway (P 1) (P 1) ~ P 1)
_test_Div_RoundHalfAway_P1_P1 =  Dict
_test_Rem_RoundHalfAway_P1_P1 :: Dict (K.Rem 'K.RoundHalfAway (P 1) (P 1) ~ P 0)
_test_Rem_RoundHalfAway_P1_P1 =  Dict
_test_Div_RoundHalfAway_P1_P2 :: Dict (K.Div 'K.RoundHalfAway (P 1) (P 2) ~ P 1)
_test_Div_RoundHalfAway_P1_P2 =  Dict
_test_Rem_RoundHalfAway_P1_P2 :: Dict (K.Rem 'K.RoundHalfAway (P 1) (P 2) ~ N 1)
_test_Rem_RoundHalfAway_P1_P2 =  Dict
_test_Div_RoundHalfAway_P1_P3 :: Dict (K.Div 'K.RoundHalfAway (P 1) (P 3) ~ P 0)
_test_Div_RoundHalfAway_P1_P3 =  Dict
_test_Rem_RoundHalfAway_P1_P3 :: Dict (K.Rem 'K.RoundHalfAway (P 1) (P 3) ~ P 1)
_test_Rem_RoundHalfAway_P1_P3 =  Dict
_test_Div_RoundHalfAway_P1_P4 :: Dict (K.Div 'K.RoundHalfAway (P 1) (P 4) ~ P 0)
_test_Div_RoundHalfAway_P1_P4 =  Dict
_test_Rem_RoundHalfAway_P1_P4 :: Dict (K.Rem 'K.RoundHalfAway (P 1) (P 4) ~ P 1)
_test_Rem_RoundHalfAway_P1_P4 =  Dict
_test_Div_RoundHalfAway_P2_N1 :: Dict (K.Div 'K.RoundHalfAway (P 2) (N 1) ~ N 2)
_test_Div_RoundHalfAway_P2_N1 =  Dict
_test_Rem_RoundHalfAway_P2_N1 :: Dict (K.Rem 'K.RoundHalfAway (P 2) (N 1) ~ P 0)
_test_Rem_RoundHalfAway_P2_N1 =  Dict
_test_Div_RoundHalfAway_P2_N2 :: Dict (K.Div 'K.RoundHalfAway (P 2) (N 2) ~ N 1)
_test_Div_RoundHalfAway_P2_N2 =  Dict
_test_Rem_RoundHalfAway_P2_N2 :: Dict (K.Rem 'K.RoundHalfAway (P 2) (N 2) ~ P 0)
_test_Rem_RoundHalfAway_P2_N2 =  Dict
_test_Div_RoundHalfAway_P2_N3 :: Dict (K.Div 'K.RoundHalfAway (P 2) (N 3) ~ N 1)
_test_Div_RoundHalfAway_P2_N3 =  Dict
_test_Rem_RoundHalfAway_P2_N3 :: Dict (K.Rem 'K.RoundHalfAway (P 2) (N 3) ~ N 1)
_test_Rem_RoundHalfAway_P2_N3 =  Dict
_test_Div_RoundHalfAway_P2_N4 :: Dict (K.Div 'K.RoundHalfAway (P 2) (N 4) ~ N 1)
_test_Div_RoundHalfAway_P2_N4 =  Dict
_test_Rem_RoundHalfAway_P2_N4 :: Dict (K.Rem 'K.RoundHalfAway (P 2) (N 4) ~ N 2)
_test_Rem_RoundHalfAway_P2_N4 =  Dict
_test_Div_RoundHalfAway_P2_P1 :: Dict (K.Div 'K.RoundHalfAway (P 2) (P 1) ~ P 2)
_test_Div_RoundHalfAway_P2_P1 =  Dict
_test_Rem_RoundHalfAway_P2_P1 :: Dict (K.Rem 'K.RoundHalfAway (P 2) (P 1) ~ P 0)
_test_Rem_RoundHalfAway_P2_P1 =  Dict
_test_Div_RoundHalfAway_P2_P2 :: Dict (K.Div 'K.RoundHalfAway (P 2) (P 2) ~ P 1)
_test_Div_RoundHalfAway_P2_P2 =  Dict
_test_Rem_RoundHalfAway_P2_P2 :: Dict (K.Rem 'K.RoundHalfAway (P 2) (P 2) ~ P 0)
_test_Rem_RoundHalfAway_P2_P2 =  Dict
_test_Div_RoundHalfAway_P2_P3 :: Dict (K.Div 'K.RoundHalfAway (P 2) (P 3) ~ P 1)
_test_Div_RoundHalfAway_P2_P3 =  Dict
_test_Rem_RoundHalfAway_P2_P3 :: Dict (K.Rem 'K.RoundHalfAway (P 2) (P 3) ~ N 1)
_test_Rem_RoundHalfAway_P2_P3 =  Dict
_test_Div_RoundHalfAway_P2_P4 :: Dict (K.Div 'K.RoundHalfAway (P 2) (P 4) ~ P 1)
_test_Div_RoundHalfAway_P2_P4 =  Dict
_test_Rem_RoundHalfAway_P2_P4 :: Dict (K.Rem 'K.RoundHalfAway (P 2) (P 4) ~ N 2)
_test_Rem_RoundHalfAway_P2_P4 =  Dict
_test_Div_RoundHalfAway_P3_N1 :: Dict (K.Div 'K.RoundHalfAway (P 3) (N 1) ~ N 3)
_test_Div_RoundHalfAway_P3_N1 =  Dict
_test_Rem_RoundHalfAway_P3_N1 :: Dict (K.Rem 'K.RoundHalfAway (P 3) (N 1) ~ P 0)
_test_Rem_RoundHalfAway_P3_N1 =  Dict
_test_Div_RoundHalfAway_P3_N2 :: Dict (K.Div 'K.RoundHalfAway (P 3) (N 2) ~ N 2)
_test_Div_RoundHalfAway_P3_N2 =  Dict
_test_Rem_RoundHalfAway_P3_N2 :: Dict (K.Rem 'K.RoundHalfAway (P 3) (N 2) ~ N 1)
_test_Rem_RoundHalfAway_P3_N2 =  Dict
_test_Div_RoundHalfAway_P3_N3 :: Dict (K.Div 'K.RoundHalfAway (P 3) (N 3) ~ N 1)
_test_Div_RoundHalfAway_P3_N3 =  Dict
_test_Rem_RoundHalfAway_P3_N3 :: Dict (K.Rem 'K.RoundHalfAway (P 3) (N 3) ~ P 0)
_test_Rem_RoundHalfAway_P3_N3 =  Dict
_test_Div_RoundHalfAway_P3_N4 :: Dict (K.Div 'K.RoundHalfAway (P 3) (N 4) ~ N 1)
_test_Div_RoundHalfAway_P3_N4 =  Dict
_test_Rem_RoundHalfAway_P3_N4 :: Dict (K.Rem 'K.RoundHalfAway (P 3) (N 4) ~ N 1)
_test_Rem_RoundHalfAway_P3_N4 =  Dict
_test_Div_RoundHalfAway_P3_P1 :: Dict (K.Div 'K.RoundHalfAway (P 3) (P 1) ~ P 3)
_test_Div_RoundHalfAway_P3_P1 =  Dict
_test_Rem_RoundHalfAway_P3_P1 :: Dict (K.Rem 'K.RoundHalfAway (P 3) (P 1) ~ P 0)
_test_Rem_RoundHalfAway_P3_P1 =  Dict
_test_Div_RoundHalfAway_P3_P2 :: Dict (K.Div 'K.RoundHalfAway (P 3) (P 2) ~ P 2)
_test_Div_RoundHalfAway_P3_P2 =  Dict
_test_Rem_RoundHalfAway_P3_P2 :: Dict (K.Rem 'K.RoundHalfAway (P 3) (P 2) ~ N 1)
_test_Rem_RoundHalfAway_P3_P2 =  Dict
_test_Div_RoundHalfAway_P3_P3 :: Dict (K.Div 'K.RoundHalfAway (P 3) (P 3) ~ P 1)
_test_Div_RoundHalfAway_P3_P3 =  Dict
_test_Rem_RoundHalfAway_P3_P3 :: Dict (K.Rem 'K.RoundHalfAway (P 3) (P 3) ~ P 0)
_test_Rem_RoundHalfAway_P3_P3 =  Dict
_test_Div_RoundHalfAway_P3_P4 :: Dict (K.Div 'K.RoundHalfAway (P 3) (P 4) ~ P 1)
_test_Div_RoundHalfAway_P3_P4 =  Dict
_test_Rem_RoundHalfAway_P3_P4 :: Dict (K.Rem 'K.RoundHalfAway (P 3) (P 4) ~ N 1)
_test_Rem_RoundHalfAway_P3_P4 =  Dict
_test_Div_RoundHalfAway_P4_N1 :: Dict (K.Div 'K.RoundHalfAway (P 4) (N 1) ~ N 4)
_test_Div_RoundHalfAway_P4_N1 =  Dict
_test_Rem_RoundHalfAway_P4_N1 :: Dict (K.Rem 'K.RoundHalfAway (P 4) (N 1) ~ P 0)
_test_Rem_RoundHalfAway_P4_N1 =  Dict
_test_Div_RoundHalfAway_P4_N2 :: Dict (K.Div 'K.RoundHalfAway (P 4) (N 2) ~ N 2)
_test_Div_RoundHalfAway_P4_N2 =  Dict
_test_Rem_RoundHalfAway_P4_N2 :: Dict (K.Rem 'K.RoundHalfAway (P 4) (N 2) ~ P 0)
_test_Rem_RoundHalfAway_P4_N2 =  Dict
_test_Div_RoundHalfAway_P4_N3 :: Dict (K.Div 'K.RoundHalfAway (P 4) (N 3) ~ N 1)
_test_Div_RoundHalfAway_P4_N3 =  Dict
_test_Rem_RoundHalfAway_P4_N3 :: Dict (K.Rem 'K.RoundHalfAway (P 4) (N 3) ~ P 1)
_test_Rem_RoundHalfAway_P4_N3 =  Dict
_test_Div_RoundHalfAway_P4_N4 :: Dict (K.Div 'K.RoundHalfAway (P 4) (N 4) ~ N 1)
_test_Div_RoundHalfAway_P4_N4 =  Dict
_test_Rem_RoundHalfAway_P4_N4 :: Dict (K.Rem 'K.RoundHalfAway (P 4) (N 4) ~ P 0)
_test_Rem_RoundHalfAway_P4_N4 =  Dict
_test_Div_RoundHalfAway_P4_P1 :: Dict (K.Div 'K.RoundHalfAway (P 4) (P 1) ~ P 4)
_test_Div_RoundHalfAway_P4_P1 =  Dict
_test_Rem_RoundHalfAway_P4_P1 :: Dict (K.Rem 'K.RoundHalfAway (P 4) (P 1) ~ P 0)
_test_Rem_RoundHalfAway_P4_P1 =  Dict
_test_Div_RoundHalfAway_P4_P2 :: Dict (K.Div 'K.RoundHalfAway (P 4) (P 2) ~ P 2)
_test_Div_RoundHalfAway_P4_P2 =  Dict
_test_Rem_RoundHalfAway_P4_P2 :: Dict (K.Rem 'K.RoundHalfAway (P 4) (P 2) ~ P 0)
_test_Rem_RoundHalfAway_P4_P2 =  Dict
_test_Div_RoundHalfAway_P4_P3 :: Dict (K.Div 'K.RoundHalfAway (P 4) (P 3) ~ P 1)
_test_Div_RoundHalfAway_P4_P3 =  Dict
_test_Rem_RoundHalfAway_P4_P3 :: Dict (K.Rem 'K.RoundHalfAway (P 4) (P 3) ~ P 1)
_test_Rem_RoundHalfAway_P4_P3 =  Dict
_test_Div_RoundHalfAway_P4_P4 :: Dict (K.Div 'K.RoundHalfAway (P 4) (P 4) ~ P 1)
_test_Div_RoundHalfAway_P4_P4 =  Dict
_test_Rem_RoundHalfAway_P4_P4 :: Dict (K.Rem 'K.RoundHalfAway (P 4) (P 4) ~ P 0)
_test_Rem_RoundHalfAway_P4_P4 =  Dict
_test_Div_RoundHalfDown_N1_N1 :: Dict (K.Div 'K.RoundHalfDown (N 1) (N 1) ~ P 1)
_test_Div_RoundHalfDown_N1_N1 =  Dict
_test_Rem_RoundHalfDown_N1_N1 :: Dict (K.Rem 'K.RoundHalfDown (N 1) (N 1) ~ P 0)
_test_Rem_RoundHalfDown_N1_N1 =  Dict
_test_Div_RoundHalfDown_N1_N2 :: Dict (K.Div 'K.RoundHalfDown (N 1) (N 2) ~ P 0)
_test_Div_RoundHalfDown_N1_N2 =  Dict
_test_Rem_RoundHalfDown_N1_N2 :: Dict (K.Rem 'K.RoundHalfDown (N 1) (N 2) ~ N 1)
_test_Rem_RoundHalfDown_N1_N2 =  Dict
_test_Div_RoundHalfDown_N1_N3 :: Dict (K.Div 'K.RoundHalfDown (N 1) (N 3) ~ P 0)
_test_Div_RoundHalfDown_N1_N3 =  Dict
_test_Rem_RoundHalfDown_N1_N3 :: Dict (K.Rem 'K.RoundHalfDown (N 1) (N 3) ~ N 1)
_test_Rem_RoundHalfDown_N1_N3 =  Dict
_test_Div_RoundHalfDown_N1_N4 :: Dict (K.Div 'K.RoundHalfDown (N 1) (N 4) ~ P 0)
_test_Div_RoundHalfDown_N1_N4 =  Dict
_test_Rem_RoundHalfDown_N1_N4 :: Dict (K.Rem 'K.RoundHalfDown (N 1) (N 4) ~ N 1)
_test_Rem_RoundHalfDown_N1_N4 =  Dict
_test_Div_RoundHalfDown_N1_P1 :: Dict (K.Div 'K.RoundHalfDown (N 1) (P 1) ~ N 1)
_test_Div_RoundHalfDown_N1_P1 =  Dict
_test_Rem_RoundHalfDown_N1_P1 :: Dict (K.Rem 'K.RoundHalfDown (N 1) (P 1) ~ P 0)
_test_Rem_RoundHalfDown_N1_P1 =  Dict
_test_Div_RoundHalfDown_N1_P2 :: Dict (K.Div 'K.RoundHalfDown (N 1) (P 2) ~ N 1)
_test_Div_RoundHalfDown_N1_P2 =  Dict
_test_Rem_RoundHalfDown_N1_P2 :: Dict (K.Rem 'K.RoundHalfDown (N 1) (P 2) ~ P 1)
_test_Rem_RoundHalfDown_N1_P2 =  Dict
_test_Div_RoundHalfDown_N1_P3 :: Dict (K.Div 'K.RoundHalfDown (N 1) (P 3) ~ P 0)
_test_Div_RoundHalfDown_N1_P3 =  Dict
_test_Rem_RoundHalfDown_N1_P3 :: Dict (K.Rem 'K.RoundHalfDown (N 1) (P 3) ~ N 1)
_test_Rem_RoundHalfDown_N1_P3 =  Dict
_test_Div_RoundHalfDown_N1_P4 :: Dict (K.Div 'K.RoundHalfDown (N 1) (P 4) ~ P 0)
_test_Div_RoundHalfDown_N1_P4 =  Dict
_test_Rem_RoundHalfDown_N1_P4 :: Dict (K.Rem 'K.RoundHalfDown (N 1) (P 4) ~ N 1)
_test_Rem_RoundHalfDown_N1_P4 =  Dict
_test_Div_RoundHalfDown_N2_N1 :: Dict (K.Div 'K.RoundHalfDown (N 2) (N 1) ~ P 2)
_test_Div_RoundHalfDown_N2_N1 =  Dict
_test_Rem_RoundHalfDown_N2_N1 :: Dict (K.Rem 'K.RoundHalfDown (N 2) (N 1) ~ P 0)
_test_Rem_RoundHalfDown_N2_N1 =  Dict
_test_Div_RoundHalfDown_N2_N2 :: Dict (K.Div 'K.RoundHalfDown (N 2) (N 2) ~ P 1)
_test_Div_RoundHalfDown_N2_N2 =  Dict
_test_Rem_RoundHalfDown_N2_N2 :: Dict (K.Rem 'K.RoundHalfDown (N 2) (N 2) ~ P 0)
_test_Rem_RoundHalfDown_N2_N2 =  Dict
_test_Div_RoundHalfDown_N2_N3 :: Dict (K.Div 'K.RoundHalfDown (N 2) (N 3) ~ P 1)
_test_Div_RoundHalfDown_N2_N3 =  Dict
_test_Rem_RoundHalfDown_N2_N3 :: Dict (K.Rem 'K.RoundHalfDown (N 2) (N 3) ~ P 1)
_test_Rem_RoundHalfDown_N2_N3 =  Dict
_test_Div_RoundHalfDown_N2_N4 :: Dict (K.Div 'K.RoundHalfDown (N 2) (N 4) ~ P 0)
_test_Div_RoundHalfDown_N2_N4 =  Dict
_test_Rem_RoundHalfDown_N2_N4 :: Dict (K.Rem 'K.RoundHalfDown (N 2) (N 4) ~ N 2)
_test_Rem_RoundHalfDown_N2_N4 =  Dict
_test_Div_RoundHalfDown_N2_P1 :: Dict (K.Div 'K.RoundHalfDown (N 2) (P 1) ~ N 2)
_test_Div_RoundHalfDown_N2_P1 =  Dict
_test_Rem_RoundHalfDown_N2_P1 :: Dict (K.Rem 'K.RoundHalfDown (N 2) (P 1) ~ P 0)
_test_Rem_RoundHalfDown_N2_P1 =  Dict
_test_Div_RoundHalfDown_N2_P2 :: Dict (K.Div 'K.RoundHalfDown (N 2) (P 2) ~ N 1)
_test_Div_RoundHalfDown_N2_P2 =  Dict
_test_Rem_RoundHalfDown_N2_P2 :: Dict (K.Rem 'K.RoundHalfDown (N 2) (P 2) ~ P 0)
_test_Rem_RoundHalfDown_N2_P2 =  Dict
_test_Div_RoundHalfDown_N2_P3 :: Dict (K.Div 'K.RoundHalfDown (N 2) (P 3) ~ N 1)
_test_Div_RoundHalfDown_N2_P3 =  Dict
_test_Rem_RoundHalfDown_N2_P3 :: Dict (K.Rem 'K.RoundHalfDown (N 2) (P 3) ~ P 1)
_test_Rem_RoundHalfDown_N2_P3 =  Dict
_test_Div_RoundHalfDown_N2_P4 :: Dict (K.Div 'K.RoundHalfDown (N 2) (P 4) ~ N 1)
_test_Div_RoundHalfDown_N2_P4 =  Dict
_test_Rem_RoundHalfDown_N2_P4 :: Dict (K.Rem 'K.RoundHalfDown (N 2) (P 4) ~ P 2)
_test_Rem_RoundHalfDown_N2_P4 =  Dict
_test_Div_RoundHalfDown_N3_N1 :: Dict (K.Div 'K.RoundHalfDown (N 3) (N 1) ~ P 3)
_test_Div_RoundHalfDown_N3_N1 =  Dict
_test_Rem_RoundHalfDown_N3_N1 :: Dict (K.Rem 'K.RoundHalfDown (N 3) (N 1) ~ P 0)
_test_Rem_RoundHalfDown_N3_N1 =  Dict
_test_Div_RoundHalfDown_N3_N2 :: Dict (K.Div 'K.RoundHalfDown (N 3) (N 2) ~ P 1)
_test_Div_RoundHalfDown_N3_N2 =  Dict
_test_Rem_RoundHalfDown_N3_N2 :: Dict (K.Rem 'K.RoundHalfDown (N 3) (N 2) ~ N 1)
_test_Rem_RoundHalfDown_N3_N2 =  Dict
_test_Div_RoundHalfDown_N3_N3 :: Dict (K.Div 'K.RoundHalfDown (N 3) (N 3) ~ P 1)
_test_Div_RoundHalfDown_N3_N3 =  Dict
_test_Rem_RoundHalfDown_N3_N3 :: Dict (K.Rem 'K.RoundHalfDown (N 3) (N 3) ~ P 0)
_test_Rem_RoundHalfDown_N3_N3 =  Dict
_test_Div_RoundHalfDown_N3_N4 :: Dict (K.Div 'K.RoundHalfDown (N 3) (N 4) ~ P 1)
_test_Div_RoundHalfDown_N3_N4 =  Dict
_test_Rem_RoundHalfDown_N3_N4 :: Dict (K.Rem 'K.RoundHalfDown (N 3) (N 4) ~ P 1)
_test_Rem_RoundHalfDown_N3_N4 =  Dict
_test_Div_RoundHalfDown_N3_P1 :: Dict (K.Div 'K.RoundHalfDown (N 3) (P 1) ~ N 3)
_test_Div_RoundHalfDown_N3_P1 =  Dict
_test_Rem_RoundHalfDown_N3_P1 :: Dict (K.Rem 'K.RoundHalfDown (N 3) (P 1) ~ P 0)
_test_Rem_RoundHalfDown_N3_P1 =  Dict
_test_Div_RoundHalfDown_N3_P2 :: Dict (K.Div 'K.RoundHalfDown (N 3) (P 2) ~ N 2)
_test_Div_RoundHalfDown_N3_P2 =  Dict
_test_Rem_RoundHalfDown_N3_P2 :: Dict (K.Rem 'K.RoundHalfDown (N 3) (P 2) ~ P 1)
_test_Rem_RoundHalfDown_N3_P2 =  Dict
_test_Div_RoundHalfDown_N3_P3 :: Dict (K.Div 'K.RoundHalfDown (N 3) (P 3) ~ N 1)
_test_Div_RoundHalfDown_N3_P3 =  Dict
_test_Rem_RoundHalfDown_N3_P3 :: Dict (K.Rem 'K.RoundHalfDown (N 3) (P 3) ~ P 0)
_test_Rem_RoundHalfDown_N3_P3 =  Dict
_test_Div_RoundHalfDown_N3_P4 :: Dict (K.Div 'K.RoundHalfDown (N 3) (P 4) ~ N 1)
_test_Div_RoundHalfDown_N3_P4 =  Dict
_test_Rem_RoundHalfDown_N3_P4 :: Dict (K.Rem 'K.RoundHalfDown (N 3) (P 4) ~ P 1)
_test_Rem_RoundHalfDown_N3_P4 =  Dict
_test_Div_RoundHalfDown_N4_N1 :: Dict (K.Div 'K.RoundHalfDown (N 4) (N 1) ~ P 4)
_test_Div_RoundHalfDown_N4_N1 =  Dict
_test_Rem_RoundHalfDown_N4_N1 :: Dict (K.Rem 'K.RoundHalfDown (N 4) (N 1) ~ P 0)
_test_Rem_RoundHalfDown_N4_N1 =  Dict
_test_Div_RoundHalfDown_N4_N2 :: Dict (K.Div 'K.RoundHalfDown (N 4) (N 2) ~ P 2)
_test_Div_RoundHalfDown_N4_N2 =  Dict
_test_Rem_RoundHalfDown_N4_N2 :: Dict (K.Rem 'K.RoundHalfDown (N 4) (N 2) ~ P 0)
_test_Rem_RoundHalfDown_N4_N2 =  Dict
_test_Div_RoundHalfDown_N4_N3 :: Dict (K.Div 'K.RoundHalfDown (N 4) (N 3) ~ P 1)
_test_Div_RoundHalfDown_N4_N3 =  Dict
_test_Rem_RoundHalfDown_N4_N3 :: Dict (K.Rem 'K.RoundHalfDown (N 4) (N 3) ~ N 1)
_test_Rem_RoundHalfDown_N4_N3 =  Dict
_test_Div_RoundHalfDown_N4_N4 :: Dict (K.Div 'K.RoundHalfDown (N 4) (N 4) ~ P 1)
_test_Div_RoundHalfDown_N4_N4 =  Dict
_test_Rem_RoundHalfDown_N4_N4 :: Dict (K.Rem 'K.RoundHalfDown (N 4) (N 4) ~ P 0)
_test_Rem_RoundHalfDown_N4_N4 =  Dict
_test_Div_RoundHalfDown_N4_P1 :: Dict (K.Div 'K.RoundHalfDown (N 4) (P 1) ~ N 4)
_test_Div_RoundHalfDown_N4_P1 =  Dict
_test_Rem_RoundHalfDown_N4_P1 :: Dict (K.Rem 'K.RoundHalfDown (N 4) (P 1) ~ P 0)
_test_Rem_RoundHalfDown_N4_P1 =  Dict
_test_Div_RoundHalfDown_N4_P2 :: Dict (K.Div 'K.RoundHalfDown (N 4) (P 2) ~ N 2)
_test_Div_RoundHalfDown_N4_P2 =  Dict
_test_Rem_RoundHalfDown_N4_P2 :: Dict (K.Rem 'K.RoundHalfDown (N 4) (P 2) ~ P 0)
_test_Rem_RoundHalfDown_N4_P2 =  Dict
_test_Div_RoundHalfDown_N4_P3 :: Dict (K.Div 'K.RoundHalfDown (N 4) (P 3) ~ N 1)
_test_Div_RoundHalfDown_N4_P3 =  Dict
_test_Rem_RoundHalfDown_N4_P3 :: Dict (K.Rem 'K.RoundHalfDown (N 4) (P 3) ~ N 1)
_test_Rem_RoundHalfDown_N4_P3 =  Dict
_test_Div_RoundHalfDown_N4_P4 :: Dict (K.Div 'K.RoundHalfDown (N 4) (P 4) ~ N 1)
_test_Div_RoundHalfDown_N4_P4 =  Dict
_test_Rem_RoundHalfDown_N4_P4 :: Dict (K.Rem 'K.RoundHalfDown (N 4) (P 4) ~ P 0)
_test_Rem_RoundHalfDown_N4_P4 =  Dict
_test_Div_RoundHalfDown_P0_N1 :: Dict (K.Div 'K.RoundHalfDown (P 0) (N 1) ~ P 0)
_test_Div_RoundHalfDown_P0_N1 =  Dict
_test_Rem_RoundHalfDown_P0_N1 :: Dict (K.Rem 'K.RoundHalfDown (P 0) (N 1) ~ P 0)
_test_Rem_RoundHalfDown_P0_N1 =  Dict
_test_Div_RoundHalfDown_P0_N2 :: Dict (K.Div 'K.RoundHalfDown (P 0) (N 2) ~ P 0)
_test_Div_RoundHalfDown_P0_N2 =  Dict
_test_Rem_RoundHalfDown_P0_N2 :: Dict (K.Rem 'K.RoundHalfDown (P 0) (N 2) ~ P 0)
_test_Rem_RoundHalfDown_P0_N2 =  Dict
_test_Div_RoundHalfDown_P0_N3 :: Dict (K.Div 'K.RoundHalfDown (P 0) (N 3) ~ P 0)
_test_Div_RoundHalfDown_P0_N3 =  Dict
_test_Rem_RoundHalfDown_P0_N3 :: Dict (K.Rem 'K.RoundHalfDown (P 0) (N 3) ~ P 0)
_test_Rem_RoundHalfDown_P0_N3 =  Dict
_test_Div_RoundHalfDown_P0_N4 :: Dict (K.Div 'K.RoundHalfDown (P 0) (N 4) ~ P 0)
_test_Div_RoundHalfDown_P0_N4 =  Dict
_test_Rem_RoundHalfDown_P0_N4 :: Dict (K.Rem 'K.RoundHalfDown (P 0) (N 4) ~ P 0)
_test_Rem_RoundHalfDown_P0_N4 =  Dict
_test_Div_RoundHalfDown_P0_P1 :: Dict (K.Div 'K.RoundHalfDown (P 0) (P 1) ~ P 0)
_test_Div_RoundHalfDown_P0_P1 =  Dict
_test_Rem_RoundHalfDown_P0_P1 :: Dict (K.Rem 'K.RoundHalfDown (P 0) (P 1) ~ P 0)
_test_Rem_RoundHalfDown_P0_P1 =  Dict
_test_Div_RoundHalfDown_P0_P2 :: Dict (K.Div 'K.RoundHalfDown (P 0) (P 2) ~ P 0)
_test_Div_RoundHalfDown_P0_P2 =  Dict
_test_Rem_RoundHalfDown_P0_P2 :: Dict (K.Rem 'K.RoundHalfDown (P 0) (P 2) ~ P 0)
_test_Rem_RoundHalfDown_P0_P2 =  Dict
_test_Div_RoundHalfDown_P0_P3 :: Dict (K.Div 'K.RoundHalfDown (P 0) (P 3) ~ P 0)
_test_Div_RoundHalfDown_P0_P3 =  Dict
_test_Rem_RoundHalfDown_P0_P3 :: Dict (K.Rem 'K.RoundHalfDown (P 0) (P 3) ~ P 0)
_test_Rem_RoundHalfDown_P0_P3 =  Dict
_test_Div_RoundHalfDown_P0_P4 :: Dict (K.Div 'K.RoundHalfDown (P 0) (P 4) ~ P 0)
_test_Div_RoundHalfDown_P0_P4 =  Dict
_test_Rem_RoundHalfDown_P0_P4 :: Dict (K.Rem 'K.RoundHalfDown (P 0) (P 4) ~ P 0)
_test_Rem_RoundHalfDown_P0_P4 =  Dict
_test_Div_RoundHalfDown_P1_N1 :: Dict (K.Div 'K.RoundHalfDown (P 1) (N 1) ~ N 1)
_test_Div_RoundHalfDown_P1_N1 =  Dict
_test_Rem_RoundHalfDown_P1_N1 :: Dict (K.Rem 'K.RoundHalfDown (P 1) (N 1) ~ P 0)
_test_Rem_RoundHalfDown_P1_N1 =  Dict
_test_Div_RoundHalfDown_P1_N2 :: Dict (K.Div 'K.RoundHalfDown (P 1) (N 2) ~ N 1)
_test_Div_RoundHalfDown_P1_N2 =  Dict
_test_Rem_RoundHalfDown_P1_N2 :: Dict (K.Rem 'K.RoundHalfDown (P 1) (N 2) ~ N 1)
_test_Rem_RoundHalfDown_P1_N2 =  Dict
_test_Div_RoundHalfDown_P1_N3 :: Dict (K.Div 'K.RoundHalfDown (P 1) (N 3) ~ P 0)
_test_Div_RoundHalfDown_P1_N3 =  Dict
_test_Rem_RoundHalfDown_P1_N3 :: Dict (K.Rem 'K.RoundHalfDown (P 1) (N 3) ~ P 1)
_test_Rem_RoundHalfDown_P1_N3 =  Dict
_test_Div_RoundHalfDown_P1_N4 :: Dict (K.Div 'K.RoundHalfDown (P 1) (N 4) ~ P 0)
_test_Div_RoundHalfDown_P1_N4 =  Dict
_test_Rem_RoundHalfDown_P1_N4 :: Dict (K.Rem 'K.RoundHalfDown (P 1) (N 4) ~ P 1)
_test_Rem_RoundHalfDown_P1_N4 =  Dict
_test_Div_RoundHalfDown_P1_P1 :: Dict (K.Div 'K.RoundHalfDown (P 1) (P 1) ~ P 1)
_test_Div_RoundHalfDown_P1_P1 =  Dict
_test_Rem_RoundHalfDown_P1_P1 :: Dict (K.Rem 'K.RoundHalfDown (P 1) (P 1) ~ P 0)
_test_Rem_RoundHalfDown_P1_P1 =  Dict
_test_Div_RoundHalfDown_P1_P2 :: Dict (K.Div 'K.RoundHalfDown (P 1) (P 2) ~ P 0)
_test_Div_RoundHalfDown_P1_P2 =  Dict
_test_Rem_RoundHalfDown_P1_P2 :: Dict (K.Rem 'K.RoundHalfDown (P 1) (P 2) ~ P 1)
_test_Rem_RoundHalfDown_P1_P2 =  Dict
_test_Div_RoundHalfDown_P1_P3 :: Dict (K.Div 'K.RoundHalfDown (P 1) (P 3) ~ P 0)
_test_Div_RoundHalfDown_P1_P3 =  Dict
_test_Rem_RoundHalfDown_P1_P3 :: Dict (K.Rem 'K.RoundHalfDown (P 1) (P 3) ~ P 1)
_test_Rem_RoundHalfDown_P1_P3 =  Dict
_test_Div_RoundHalfDown_P1_P4 :: Dict (K.Div 'K.RoundHalfDown (P 1) (P 4) ~ P 0)
_test_Div_RoundHalfDown_P1_P4 =  Dict
_test_Rem_RoundHalfDown_P1_P4 :: Dict (K.Rem 'K.RoundHalfDown (P 1) (P 4) ~ P 1)
_test_Rem_RoundHalfDown_P1_P4 =  Dict
_test_Div_RoundHalfDown_P2_N1 :: Dict (K.Div 'K.RoundHalfDown (P 2) (N 1) ~ N 2)
_test_Div_RoundHalfDown_P2_N1 =  Dict
_test_Rem_RoundHalfDown_P2_N1 :: Dict (K.Rem 'K.RoundHalfDown (P 2) (N 1) ~ P 0)
_test_Rem_RoundHalfDown_P2_N1 =  Dict
_test_Div_RoundHalfDown_P2_N2 :: Dict (K.Div 'K.RoundHalfDown (P 2) (N 2) ~ N 1)
_test_Div_RoundHalfDown_P2_N2 =  Dict
_test_Rem_RoundHalfDown_P2_N2 :: Dict (K.Rem 'K.RoundHalfDown (P 2) (N 2) ~ P 0)
_test_Rem_RoundHalfDown_P2_N2 =  Dict
_test_Div_RoundHalfDown_P2_N3 :: Dict (K.Div 'K.RoundHalfDown (P 2) (N 3) ~ N 1)
_test_Div_RoundHalfDown_P2_N3 =  Dict
_test_Rem_RoundHalfDown_P2_N3 :: Dict (K.Rem 'K.RoundHalfDown (P 2) (N 3) ~ N 1)
_test_Rem_RoundHalfDown_P2_N3 =  Dict
_test_Div_RoundHalfDown_P2_N4 :: Dict (K.Div 'K.RoundHalfDown (P 2) (N 4) ~ N 1)
_test_Div_RoundHalfDown_P2_N4 =  Dict
_test_Rem_RoundHalfDown_P2_N4 :: Dict (K.Rem 'K.RoundHalfDown (P 2) (N 4) ~ N 2)
_test_Rem_RoundHalfDown_P2_N4 =  Dict
_test_Div_RoundHalfDown_P2_P1 :: Dict (K.Div 'K.RoundHalfDown (P 2) (P 1) ~ P 2)
_test_Div_RoundHalfDown_P2_P1 =  Dict
_test_Rem_RoundHalfDown_P2_P1 :: Dict (K.Rem 'K.RoundHalfDown (P 2) (P 1) ~ P 0)
_test_Rem_RoundHalfDown_P2_P1 =  Dict
_test_Div_RoundHalfDown_P2_P2 :: Dict (K.Div 'K.RoundHalfDown (P 2) (P 2) ~ P 1)
_test_Div_RoundHalfDown_P2_P2 =  Dict
_test_Rem_RoundHalfDown_P2_P2 :: Dict (K.Rem 'K.RoundHalfDown (P 2) (P 2) ~ P 0)
_test_Rem_RoundHalfDown_P2_P2 =  Dict
_test_Div_RoundHalfDown_P2_P3 :: Dict (K.Div 'K.RoundHalfDown (P 2) (P 3) ~ P 1)
_test_Div_RoundHalfDown_P2_P3 =  Dict
_test_Rem_RoundHalfDown_P2_P3 :: Dict (K.Rem 'K.RoundHalfDown (P 2) (P 3) ~ N 1)
_test_Rem_RoundHalfDown_P2_P3 =  Dict
_test_Div_RoundHalfDown_P2_P4 :: Dict (K.Div 'K.RoundHalfDown (P 2) (P 4) ~ P 0)
_test_Div_RoundHalfDown_P2_P4 =  Dict
_test_Rem_RoundHalfDown_P2_P4 :: Dict (K.Rem 'K.RoundHalfDown (P 2) (P 4) ~ P 2)
_test_Rem_RoundHalfDown_P2_P4 =  Dict
_test_Div_RoundHalfDown_P3_N1 :: Dict (K.Div 'K.RoundHalfDown (P 3) (N 1) ~ N 3)
_test_Div_RoundHalfDown_P3_N1 =  Dict
_test_Rem_RoundHalfDown_P3_N1 :: Dict (K.Rem 'K.RoundHalfDown (P 3) (N 1) ~ P 0)
_test_Rem_RoundHalfDown_P3_N1 =  Dict
_test_Div_RoundHalfDown_P3_N2 :: Dict (K.Div 'K.RoundHalfDown (P 3) (N 2) ~ N 2)
_test_Div_RoundHalfDown_P3_N2 =  Dict
_test_Rem_RoundHalfDown_P3_N2 :: Dict (K.Rem 'K.RoundHalfDown (P 3) (N 2) ~ N 1)
_test_Rem_RoundHalfDown_P3_N2 =  Dict
_test_Div_RoundHalfDown_P3_N3 :: Dict (K.Div 'K.RoundHalfDown (P 3) (N 3) ~ N 1)
_test_Div_RoundHalfDown_P3_N3 =  Dict
_test_Rem_RoundHalfDown_P3_N3 :: Dict (K.Rem 'K.RoundHalfDown (P 3) (N 3) ~ P 0)
_test_Rem_RoundHalfDown_P3_N3 =  Dict
_test_Div_RoundHalfDown_P3_N4 :: Dict (K.Div 'K.RoundHalfDown (P 3) (N 4) ~ N 1)
_test_Div_RoundHalfDown_P3_N4 =  Dict
_test_Rem_RoundHalfDown_P3_N4 :: Dict (K.Rem 'K.RoundHalfDown (P 3) (N 4) ~ N 1)
_test_Rem_RoundHalfDown_P3_N4 =  Dict
_test_Div_RoundHalfDown_P3_P1 :: Dict (K.Div 'K.RoundHalfDown (P 3) (P 1) ~ P 3)
_test_Div_RoundHalfDown_P3_P1 =  Dict
_test_Rem_RoundHalfDown_P3_P1 :: Dict (K.Rem 'K.RoundHalfDown (P 3) (P 1) ~ P 0)
_test_Rem_RoundHalfDown_P3_P1 =  Dict
_test_Div_RoundHalfDown_P3_P2 :: Dict (K.Div 'K.RoundHalfDown (P 3) (P 2) ~ P 1)
_test_Div_RoundHalfDown_P3_P2 =  Dict
_test_Rem_RoundHalfDown_P3_P2 :: Dict (K.Rem 'K.RoundHalfDown (P 3) (P 2) ~ P 1)
_test_Rem_RoundHalfDown_P3_P2 =  Dict
_test_Div_RoundHalfDown_P3_P3 :: Dict (K.Div 'K.RoundHalfDown (P 3) (P 3) ~ P 1)
_test_Div_RoundHalfDown_P3_P3 =  Dict
_test_Rem_RoundHalfDown_P3_P3 :: Dict (K.Rem 'K.RoundHalfDown (P 3) (P 3) ~ P 0)
_test_Rem_RoundHalfDown_P3_P3 =  Dict
_test_Div_RoundHalfDown_P3_P4 :: Dict (K.Div 'K.RoundHalfDown (P 3) (P 4) ~ P 1)
_test_Div_RoundHalfDown_P3_P4 =  Dict
_test_Rem_RoundHalfDown_P3_P4 :: Dict (K.Rem 'K.RoundHalfDown (P 3) (P 4) ~ N 1)
_test_Rem_RoundHalfDown_P3_P4 =  Dict
_test_Div_RoundHalfDown_P4_N1 :: Dict (K.Div 'K.RoundHalfDown (P 4) (N 1) ~ N 4)
_test_Div_RoundHalfDown_P4_N1 =  Dict
_test_Rem_RoundHalfDown_P4_N1 :: Dict (K.Rem 'K.RoundHalfDown (P 4) (N 1) ~ P 0)
_test_Rem_RoundHalfDown_P4_N1 =  Dict
_test_Div_RoundHalfDown_P4_N2 :: Dict (K.Div 'K.RoundHalfDown (P 4) (N 2) ~ N 2)
_test_Div_RoundHalfDown_P4_N2 =  Dict
_test_Rem_RoundHalfDown_P4_N2 :: Dict (K.Rem 'K.RoundHalfDown (P 4) (N 2) ~ P 0)
_test_Rem_RoundHalfDown_P4_N2 =  Dict
_test_Div_RoundHalfDown_P4_N3 :: Dict (K.Div 'K.RoundHalfDown (P 4) (N 3) ~ N 1)
_test_Div_RoundHalfDown_P4_N3 =  Dict
_test_Rem_RoundHalfDown_P4_N3 :: Dict (K.Rem 'K.RoundHalfDown (P 4) (N 3) ~ P 1)
_test_Rem_RoundHalfDown_P4_N3 =  Dict
_test_Div_RoundHalfDown_P4_N4 :: Dict (K.Div 'K.RoundHalfDown (P 4) (N 4) ~ N 1)
_test_Div_RoundHalfDown_P4_N4 =  Dict
_test_Rem_RoundHalfDown_P4_N4 :: Dict (K.Rem 'K.RoundHalfDown (P 4) (N 4) ~ P 0)
_test_Rem_RoundHalfDown_P4_N4 =  Dict
_test_Div_RoundHalfDown_P4_P1 :: Dict (K.Div 'K.RoundHalfDown (P 4) (P 1) ~ P 4)
_test_Div_RoundHalfDown_P4_P1 =  Dict
_test_Rem_RoundHalfDown_P4_P1 :: Dict (K.Rem 'K.RoundHalfDown (P 4) (P 1) ~ P 0)
_test_Rem_RoundHalfDown_P4_P1 =  Dict
_test_Div_RoundHalfDown_P4_P2 :: Dict (K.Div 'K.RoundHalfDown (P 4) (P 2) ~ P 2)
_test_Div_RoundHalfDown_P4_P2 =  Dict
_test_Rem_RoundHalfDown_P4_P2 :: Dict (K.Rem 'K.RoundHalfDown (P 4) (P 2) ~ P 0)
_test_Rem_RoundHalfDown_P4_P2 =  Dict
_test_Div_RoundHalfDown_P4_P3 :: Dict (K.Div 'K.RoundHalfDown (P 4) (P 3) ~ P 1)
_test_Div_RoundHalfDown_P4_P3 =  Dict
_test_Rem_RoundHalfDown_P4_P3 :: Dict (K.Rem 'K.RoundHalfDown (P 4) (P 3) ~ P 1)
_test_Rem_RoundHalfDown_P4_P3 =  Dict
_test_Div_RoundHalfDown_P4_P4 :: Dict (K.Div 'K.RoundHalfDown (P 4) (P 4) ~ P 1)
_test_Div_RoundHalfDown_P4_P4 =  Dict
_test_Rem_RoundHalfDown_P4_P4 :: Dict (K.Rem 'K.RoundHalfDown (P 4) (P 4) ~ P 0)
_test_Rem_RoundHalfDown_P4_P4 =  Dict
_test_Div_RoundHalfEven_N1_N1 :: Dict (K.Div 'K.RoundHalfEven (N 1) (N 1) ~ P 1)
_test_Div_RoundHalfEven_N1_N1 =  Dict
_test_Rem_RoundHalfEven_N1_N1 :: Dict (K.Rem 'K.RoundHalfEven (N 1) (N 1) ~ P 0)
_test_Rem_RoundHalfEven_N1_N1 =  Dict
_test_Div_RoundHalfEven_N1_N2 :: Dict (K.Div 'K.RoundHalfEven (N 1) (N 2) ~ P 0)
_test_Div_RoundHalfEven_N1_N2 =  Dict
_test_Rem_RoundHalfEven_N1_N2 :: Dict (K.Rem 'K.RoundHalfEven (N 1) (N 2) ~ N 1)
_test_Rem_RoundHalfEven_N1_N2 =  Dict
_test_Div_RoundHalfEven_N1_N3 :: Dict (K.Div 'K.RoundHalfEven (N 1) (N 3) ~ P 0)
_test_Div_RoundHalfEven_N1_N3 =  Dict
_test_Rem_RoundHalfEven_N1_N3 :: Dict (K.Rem 'K.RoundHalfEven (N 1) (N 3) ~ N 1)
_test_Rem_RoundHalfEven_N1_N3 =  Dict
_test_Div_RoundHalfEven_N1_N4 :: Dict (K.Div 'K.RoundHalfEven (N 1) (N 4) ~ P 0)
_test_Div_RoundHalfEven_N1_N4 =  Dict
_test_Rem_RoundHalfEven_N1_N4 :: Dict (K.Rem 'K.RoundHalfEven (N 1) (N 4) ~ N 1)
_test_Rem_RoundHalfEven_N1_N4 =  Dict
_test_Div_RoundHalfEven_N1_P1 :: Dict (K.Div 'K.RoundHalfEven (N 1) (P 1) ~ N 1)
_test_Div_RoundHalfEven_N1_P1 =  Dict
_test_Rem_RoundHalfEven_N1_P1 :: Dict (K.Rem 'K.RoundHalfEven (N 1) (P 1) ~ P 0)
_test_Rem_RoundHalfEven_N1_P1 =  Dict
_test_Div_RoundHalfEven_N1_P2 :: Dict (K.Div 'K.RoundHalfEven (N 1) (P 2) ~ P 0)
_test_Div_RoundHalfEven_N1_P2 =  Dict
_test_Rem_RoundHalfEven_N1_P2 :: Dict (K.Rem 'K.RoundHalfEven (N 1) (P 2) ~ N 1)
_test_Rem_RoundHalfEven_N1_P2 =  Dict
_test_Div_RoundHalfEven_N1_P3 :: Dict (K.Div 'K.RoundHalfEven (N 1) (P 3) ~ P 0)
_test_Div_RoundHalfEven_N1_P3 =  Dict
_test_Rem_RoundHalfEven_N1_P3 :: Dict (K.Rem 'K.RoundHalfEven (N 1) (P 3) ~ N 1)
_test_Rem_RoundHalfEven_N1_P3 =  Dict
_test_Div_RoundHalfEven_N1_P4 :: Dict (K.Div 'K.RoundHalfEven (N 1) (P 4) ~ P 0)
_test_Div_RoundHalfEven_N1_P4 =  Dict
_test_Rem_RoundHalfEven_N1_P4 :: Dict (K.Rem 'K.RoundHalfEven (N 1) (P 4) ~ N 1)
_test_Rem_RoundHalfEven_N1_P4 =  Dict
_test_Div_RoundHalfEven_N2_N1 :: Dict (K.Div 'K.RoundHalfEven (N 2) (N 1) ~ P 2)
_test_Div_RoundHalfEven_N2_N1 =  Dict
_test_Rem_RoundHalfEven_N2_N1 :: Dict (K.Rem 'K.RoundHalfEven (N 2) (N 1) ~ P 0)
_test_Rem_RoundHalfEven_N2_N1 =  Dict
_test_Div_RoundHalfEven_N2_N2 :: Dict (K.Div 'K.RoundHalfEven (N 2) (N 2) ~ P 1)
_test_Div_RoundHalfEven_N2_N2 =  Dict
_test_Rem_RoundHalfEven_N2_N2 :: Dict (K.Rem 'K.RoundHalfEven (N 2) (N 2) ~ P 0)
_test_Rem_RoundHalfEven_N2_N2 =  Dict
_test_Div_RoundHalfEven_N2_N3 :: Dict (K.Div 'K.RoundHalfEven (N 2) (N 3) ~ P 1)
_test_Div_RoundHalfEven_N2_N3 =  Dict
_test_Rem_RoundHalfEven_N2_N3 :: Dict (K.Rem 'K.RoundHalfEven (N 2) (N 3) ~ P 1)
_test_Rem_RoundHalfEven_N2_N3 =  Dict
_test_Div_RoundHalfEven_N2_N4 :: Dict (K.Div 'K.RoundHalfEven (N 2) (N 4) ~ P 0)
_test_Div_RoundHalfEven_N2_N4 =  Dict
_test_Rem_RoundHalfEven_N2_N4 :: Dict (K.Rem 'K.RoundHalfEven (N 2) (N 4) ~ N 2)
_test_Rem_RoundHalfEven_N2_N4 =  Dict
_test_Div_RoundHalfEven_N2_P1 :: Dict (K.Div 'K.RoundHalfEven (N 2) (P 1) ~ N 2)
_test_Div_RoundHalfEven_N2_P1 =  Dict
_test_Rem_RoundHalfEven_N2_P1 :: Dict (K.Rem 'K.RoundHalfEven (N 2) (P 1) ~ P 0)
_test_Rem_RoundHalfEven_N2_P1 =  Dict
_test_Div_RoundHalfEven_N2_P2 :: Dict (K.Div 'K.RoundHalfEven (N 2) (P 2) ~ N 1)
_test_Div_RoundHalfEven_N2_P2 =  Dict
_test_Rem_RoundHalfEven_N2_P2 :: Dict (K.Rem 'K.RoundHalfEven (N 2) (P 2) ~ P 0)
_test_Rem_RoundHalfEven_N2_P2 =  Dict
_test_Div_RoundHalfEven_N2_P3 :: Dict (K.Div 'K.RoundHalfEven (N 2) (P 3) ~ N 1)
_test_Div_RoundHalfEven_N2_P3 =  Dict
_test_Rem_RoundHalfEven_N2_P3 :: Dict (K.Rem 'K.RoundHalfEven (N 2) (P 3) ~ P 1)
_test_Rem_RoundHalfEven_N2_P3 =  Dict
_test_Div_RoundHalfEven_N2_P4 :: Dict (K.Div 'K.RoundHalfEven (N 2) (P 4) ~ P 0)
_test_Div_RoundHalfEven_N2_P4 =  Dict
_test_Rem_RoundHalfEven_N2_P4 :: Dict (K.Rem 'K.RoundHalfEven (N 2) (P 4) ~ N 2)
_test_Rem_RoundHalfEven_N2_P4 =  Dict
_test_Div_RoundHalfEven_N3_N1 :: Dict (K.Div 'K.RoundHalfEven (N 3) (N 1) ~ P 3)
_test_Div_RoundHalfEven_N3_N1 =  Dict
_test_Rem_RoundHalfEven_N3_N1 :: Dict (K.Rem 'K.RoundHalfEven (N 3) (N 1) ~ P 0)
_test_Rem_RoundHalfEven_N3_N1 =  Dict
_test_Div_RoundHalfEven_N3_N2 :: Dict (K.Div 'K.RoundHalfEven (N 3) (N 2) ~ P 2)
_test_Div_RoundHalfEven_N3_N2 =  Dict
_test_Rem_RoundHalfEven_N3_N2 :: Dict (K.Rem 'K.RoundHalfEven (N 3) (N 2) ~ P 1)
_test_Rem_RoundHalfEven_N3_N2 =  Dict
_test_Div_RoundHalfEven_N3_N3 :: Dict (K.Div 'K.RoundHalfEven (N 3) (N 3) ~ P 1)
_test_Div_RoundHalfEven_N3_N3 =  Dict
_test_Rem_RoundHalfEven_N3_N3 :: Dict (K.Rem 'K.RoundHalfEven (N 3) (N 3) ~ P 0)
_test_Rem_RoundHalfEven_N3_N3 =  Dict
_test_Div_RoundHalfEven_N3_N4 :: Dict (K.Div 'K.RoundHalfEven (N 3) (N 4) ~ P 1)
_test_Div_RoundHalfEven_N3_N4 =  Dict
_test_Rem_RoundHalfEven_N3_N4 :: Dict (K.Rem 'K.RoundHalfEven (N 3) (N 4) ~ P 1)
_test_Rem_RoundHalfEven_N3_N4 =  Dict
_test_Div_RoundHalfEven_N3_P1 :: Dict (K.Div 'K.RoundHalfEven (N 3) (P 1) ~ N 3)
_test_Div_RoundHalfEven_N3_P1 =  Dict
_test_Rem_RoundHalfEven_N3_P1 :: Dict (K.Rem 'K.RoundHalfEven (N 3) (P 1) ~ P 0)
_test_Rem_RoundHalfEven_N3_P1 =  Dict
_test_Div_RoundHalfEven_N3_P2 :: Dict (K.Div 'K.RoundHalfEven (N 3) (P 2) ~ N 2)
_test_Div_RoundHalfEven_N3_P2 =  Dict
_test_Rem_RoundHalfEven_N3_P2 :: Dict (K.Rem 'K.RoundHalfEven (N 3) (P 2) ~ P 1)
_test_Rem_RoundHalfEven_N3_P2 =  Dict
_test_Div_RoundHalfEven_N3_P3 :: Dict (K.Div 'K.RoundHalfEven (N 3) (P 3) ~ N 1)
_test_Div_RoundHalfEven_N3_P3 =  Dict
_test_Rem_RoundHalfEven_N3_P3 :: Dict (K.Rem 'K.RoundHalfEven (N 3) (P 3) ~ P 0)
_test_Rem_RoundHalfEven_N3_P3 =  Dict
_test_Div_RoundHalfEven_N3_P4 :: Dict (K.Div 'K.RoundHalfEven (N 3) (P 4) ~ N 1)
_test_Div_RoundHalfEven_N3_P4 =  Dict
_test_Rem_RoundHalfEven_N3_P4 :: Dict (K.Rem 'K.RoundHalfEven (N 3) (P 4) ~ P 1)
_test_Rem_RoundHalfEven_N3_P4 =  Dict
_test_Div_RoundHalfEven_N4_N1 :: Dict (K.Div 'K.RoundHalfEven (N 4) (N 1) ~ P 4)
_test_Div_RoundHalfEven_N4_N1 =  Dict
_test_Rem_RoundHalfEven_N4_N1 :: Dict (K.Rem 'K.RoundHalfEven (N 4) (N 1) ~ P 0)
_test_Rem_RoundHalfEven_N4_N1 =  Dict
_test_Div_RoundHalfEven_N4_N2 :: Dict (K.Div 'K.RoundHalfEven (N 4) (N 2) ~ P 2)
_test_Div_RoundHalfEven_N4_N2 =  Dict
_test_Rem_RoundHalfEven_N4_N2 :: Dict (K.Rem 'K.RoundHalfEven (N 4) (N 2) ~ P 0)
_test_Rem_RoundHalfEven_N4_N2 =  Dict
_test_Div_RoundHalfEven_N4_N3 :: Dict (K.Div 'K.RoundHalfEven (N 4) (N 3) ~ P 1)
_test_Div_RoundHalfEven_N4_N3 =  Dict
_test_Rem_RoundHalfEven_N4_N3 :: Dict (K.Rem 'K.RoundHalfEven (N 4) (N 3) ~ N 1)
_test_Rem_RoundHalfEven_N4_N3 =  Dict
_test_Div_RoundHalfEven_N4_N4 :: Dict (K.Div 'K.RoundHalfEven (N 4) (N 4) ~ P 1)
_test_Div_RoundHalfEven_N4_N4 =  Dict
_test_Rem_RoundHalfEven_N4_N4 :: Dict (K.Rem 'K.RoundHalfEven (N 4) (N 4) ~ P 0)
_test_Rem_RoundHalfEven_N4_N4 =  Dict
_test_Div_RoundHalfEven_N4_P1 :: Dict (K.Div 'K.RoundHalfEven (N 4) (P 1) ~ N 4)
_test_Div_RoundHalfEven_N4_P1 =  Dict
_test_Rem_RoundHalfEven_N4_P1 :: Dict (K.Rem 'K.RoundHalfEven (N 4) (P 1) ~ P 0)
_test_Rem_RoundHalfEven_N4_P1 =  Dict
_test_Div_RoundHalfEven_N4_P2 :: Dict (K.Div 'K.RoundHalfEven (N 4) (P 2) ~ N 2)
_test_Div_RoundHalfEven_N4_P2 =  Dict
_test_Rem_RoundHalfEven_N4_P2 :: Dict (K.Rem 'K.RoundHalfEven (N 4) (P 2) ~ P 0)
_test_Rem_RoundHalfEven_N4_P2 =  Dict
_test_Div_RoundHalfEven_N4_P3 :: Dict (K.Div 'K.RoundHalfEven (N 4) (P 3) ~ N 1)
_test_Div_RoundHalfEven_N4_P3 =  Dict
_test_Rem_RoundHalfEven_N4_P3 :: Dict (K.Rem 'K.RoundHalfEven (N 4) (P 3) ~ N 1)
_test_Rem_RoundHalfEven_N4_P3 =  Dict
_test_Div_RoundHalfEven_N4_P4 :: Dict (K.Div 'K.RoundHalfEven (N 4) (P 4) ~ N 1)
_test_Div_RoundHalfEven_N4_P4 =  Dict
_test_Rem_RoundHalfEven_N4_P4 :: Dict (K.Rem 'K.RoundHalfEven (N 4) (P 4) ~ P 0)
_test_Rem_RoundHalfEven_N4_P4 =  Dict
_test_Div_RoundHalfEven_P0_N1 :: Dict (K.Div 'K.RoundHalfEven (P 0) (N 1) ~ P 0)
_test_Div_RoundHalfEven_P0_N1 =  Dict
_test_Rem_RoundHalfEven_P0_N1 :: Dict (K.Rem 'K.RoundHalfEven (P 0) (N 1) ~ P 0)
_test_Rem_RoundHalfEven_P0_N1 =  Dict
_test_Div_RoundHalfEven_P0_N2 :: Dict (K.Div 'K.RoundHalfEven (P 0) (N 2) ~ P 0)
_test_Div_RoundHalfEven_P0_N2 =  Dict
_test_Rem_RoundHalfEven_P0_N2 :: Dict (K.Rem 'K.RoundHalfEven (P 0) (N 2) ~ P 0)
_test_Rem_RoundHalfEven_P0_N2 =  Dict
_test_Div_RoundHalfEven_P0_N3 :: Dict (K.Div 'K.RoundHalfEven (P 0) (N 3) ~ P 0)
_test_Div_RoundHalfEven_P0_N3 =  Dict
_test_Rem_RoundHalfEven_P0_N3 :: Dict (K.Rem 'K.RoundHalfEven (P 0) (N 3) ~ P 0)
_test_Rem_RoundHalfEven_P0_N3 =  Dict
_test_Div_RoundHalfEven_P0_N4 :: Dict (K.Div 'K.RoundHalfEven (P 0) (N 4) ~ P 0)
_test_Div_RoundHalfEven_P0_N4 =  Dict
_test_Rem_RoundHalfEven_P0_N4 :: Dict (K.Rem 'K.RoundHalfEven (P 0) (N 4) ~ P 0)
_test_Rem_RoundHalfEven_P0_N4 =  Dict
_test_Div_RoundHalfEven_P0_P1 :: Dict (K.Div 'K.RoundHalfEven (P 0) (P 1) ~ P 0)
_test_Div_RoundHalfEven_P0_P1 =  Dict
_test_Rem_RoundHalfEven_P0_P1 :: Dict (K.Rem 'K.RoundHalfEven (P 0) (P 1) ~ P 0)
_test_Rem_RoundHalfEven_P0_P1 =  Dict
_test_Div_RoundHalfEven_P0_P2 :: Dict (K.Div 'K.RoundHalfEven (P 0) (P 2) ~ P 0)
_test_Div_RoundHalfEven_P0_P2 =  Dict
_test_Rem_RoundHalfEven_P0_P2 :: Dict (K.Rem 'K.RoundHalfEven (P 0) (P 2) ~ P 0)
_test_Rem_RoundHalfEven_P0_P2 =  Dict
_test_Div_RoundHalfEven_P0_P3 :: Dict (K.Div 'K.RoundHalfEven (P 0) (P 3) ~ P 0)
_test_Div_RoundHalfEven_P0_P3 =  Dict
_test_Rem_RoundHalfEven_P0_P3 :: Dict (K.Rem 'K.RoundHalfEven (P 0) (P 3) ~ P 0)
_test_Rem_RoundHalfEven_P0_P3 =  Dict
_test_Div_RoundHalfEven_P0_P4 :: Dict (K.Div 'K.RoundHalfEven (P 0) (P 4) ~ P 0)
_test_Div_RoundHalfEven_P0_P4 =  Dict
_test_Rem_RoundHalfEven_P0_P4 :: Dict (K.Rem 'K.RoundHalfEven (P 0) (P 4) ~ P 0)
_test_Rem_RoundHalfEven_P0_P4 =  Dict
_test_Div_RoundHalfEven_P1_N1 :: Dict (K.Div 'K.RoundHalfEven (P 1) (N 1) ~ N 1)
_test_Div_RoundHalfEven_P1_N1 =  Dict
_test_Rem_RoundHalfEven_P1_N1 :: Dict (K.Rem 'K.RoundHalfEven (P 1) (N 1) ~ P 0)
_test_Rem_RoundHalfEven_P1_N1 =  Dict
_test_Div_RoundHalfEven_P1_N2 :: Dict (K.Div 'K.RoundHalfEven (P 1) (N 2) ~ P 0)
_test_Div_RoundHalfEven_P1_N2 =  Dict
_test_Rem_RoundHalfEven_P1_N2 :: Dict (K.Rem 'K.RoundHalfEven (P 1) (N 2) ~ P 1)
_test_Rem_RoundHalfEven_P1_N2 =  Dict
_test_Div_RoundHalfEven_P1_N3 :: Dict (K.Div 'K.RoundHalfEven (P 1) (N 3) ~ P 0)
_test_Div_RoundHalfEven_P1_N3 =  Dict
_test_Rem_RoundHalfEven_P1_N3 :: Dict (K.Rem 'K.RoundHalfEven (P 1) (N 3) ~ P 1)
_test_Rem_RoundHalfEven_P1_N3 =  Dict
_test_Div_RoundHalfEven_P1_N4 :: Dict (K.Div 'K.RoundHalfEven (P 1) (N 4) ~ P 0)
_test_Div_RoundHalfEven_P1_N4 =  Dict
_test_Rem_RoundHalfEven_P1_N4 :: Dict (K.Rem 'K.RoundHalfEven (P 1) (N 4) ~ P 1)
_test_Rem_RoundHalfEven_P1_N4 =  Dict
_test_Div_RoundHalfEven_P1_P1 :: Dict (K.Div 'K.RoundHalfEven (P 1) (P 1) ~ P 1)
_test_Div_RoundHalfEven_P1_P1 =  Dict
_test_Rem_RoundHalfEven_P1_P1 :: Dict (K.Rem 'K.RoundHalfEven (P 1) (P 1) ~ P 0)
_test_Rem_RoundHalfEven_P1_P1 =  Dict
_test_Div_RoundHalfEven_P1_P2 :: Dict (K.Div 'K.RoundHalfEven (P 1) (P 2) ~ P 0)
_test_Div_RoundHalfEven_P1_P2 =  Dict
_test_Rem_RoundHalfEven_P1_P2 :: Dict (K.Rem 'K.RoundHalfEven (P 1) (P 2) ~ P 1)
_test_Rem_RoundHalfEven_P1_P2 =  Dict
_test_Div_RoundHalfEven_P1_P3 :: Dict (K.Div 'K.RoundHalfEven (P 1) (P 3) ~ P 0)
_test_Div_RoundHalfEven_P1_P3 =  Dict
_test_Rem_RoundHalfEven_P1_P3 :: Dict (K.Rem 'K.RoundHalfEven (P 1) (P 3) ~ P 1)
_test_Rem_RoundHalfEven_P1_P3 =  Dict
_test_Div_RoundHalfEven_P1_P4 :: Dict (K.Div 'K.RoundHalfEven (P 1) (P 4) ~ P 0)
_test_Div_RoundHalfEven_P1_P4 =  Dict
_test_Rem_RoundHalfEven_P1_P4 :: Dict (K.Rem 'K.RoundHalfEven (P 1) (P 4) ~ P 1)
_test_Rem_RoundHalfEven_P1_P4 =  Dict
_test_Div_RoundHalfEven_P2_N1 :: Dict (K.Div 'K.RoundHalfEven (P 2) (N 1) ~ N 2)
_test_Div_RoundHalfEven_P2_N1 =  Dict
_test_Rem_RoundHalfEven_P2_N1 :: Dict (K.Rem 'K.RoundHalfEven (P 2) (N 1) ~ P 0)
_test_Rem_RoundHalfEven_P2_N1 =  Dict
_test_Div_RoundHalfEven_P2_N2 :: Dict (K.Div 'K.RoundHalfEven (P 2) (N 2) ~ N 1)
_test_Div_RoundHalfEven_P2_N2 =  Dict
_test_Rem_RoundHalfEven_P2_N2 :: Dict (K.Rem 'K.RoundHalfEven (P 2) (N 2) ~ P 0)
_test_Rem_RoundHalfEven_P2_N2 =  Dict
_test_Div_RoundHalfEven_P2_N3 :: Dict (K.Div 'K.RoundHalfEven (P 2) (N 3) ~ N 1)
_test_Div_RoundHalfEven_P2_N3 =  Dict
_test_Rem_RoundHalfEven_P2_N3 :: Dict (K.Rem 'K.RoundHalfEven (P 2) (N 3) ~ N 1)
_test_Rem_RoundHalfEven_P2_N3 =  Dict
_test_Div_RoundHalfEven_P2_N4 :: Dict (K.Div 'K.RoundHalfEven (P 2) (N 4) ~ P 0)
_test_Div_RoundHalfEven_P2_N4 =  Dict
_test_Rem_RoundHalfEven_P2_N4 :: Dict (K.Rem 'K.RoundHalfEven (P 2) (N 4) ~ P 2)
_test_Rem_RoundHalfEven_P2_N4 =  Dict
_test_Div_RoundHalfEven_P2_P1 :: Dict (K.Div 'K.RoundHalfEven (P 2) (P 1) ~ P 2)
_test_Div_RoundHalfEven_P2_P1 =  Dict
_test_Rem_RoundHalfEven_P2_P1 :: Dict (K.Rem 'K.RoundHalfEven (P 2) (P 1) ~ P 0)
_test_Rem_RoundHalfEven_P2_P1 =  Dict
_test_Div_RoundHalfEven_P2_P2 :: Dict (K.Div 'K.RoundHalfEven (P 2) (P 2) ~ P 1)
_test_Div_RoundHalfEven_P2_P2 =  Dict
_test_Rem_RoundHalfEven_P2_P2 :: Dict (K.Rem 'K.RoundHalfEven (P 2) (P 2) ~ P 0)
_test_Rem_RoundHalfEven_P2_P2 =  Dict
_test_Div_RoundHalfEven_P2_P3 :: Dict (K.Div 'K.RoundHalfEven (P 2) (P 3) ~ P 1)
_test_Div_RoundHalfEven_P2_P3 =  Dict
_test_Rem_RoundHalfEven_P2_P3 :: Dict (K.Rem 'K.RoundHalfEven (P 2) (P 3) ~ N 1)
_test_Rem_RoundHalfEven_P2_P3 =  Dict
_test_Div_RoundHalfEven_P2_P4 :: Dict (K.Div 'K.RoundHalfEven (P 2) (P 4) ~ P 0)
_test_Div_RoundHalfEven_P2_P4 =  Dict
_test_Rem_RoundHalfEven_P2_P4 :: Dict (K.Rem 'K.RoundHalfEven (P 2) (P 4) ~ P 2)
_test_Rem_RoundHalfEven_P2_P4 =  Dict
_test_Div_RoundHalfEven_P3_N1 :: Dict (K.Div 'K.RoundHalfEven (P 3) (N 1) ~ N 3)
_test_Div_RoundHalfEven_P3_N1 =  Dict
_test_Rem_RoundHalfEven_P3_N1 :: Dict (K.Rem 'K.RoundHalfEven (P 3) (N 1) ~ P 0)
_test_Rem_RoundHalfEven_P3_N1 =  Dict
_test_Div_RoundHalfEven_P3_N2 :: Dict (K.Div 'K.RoundHalfEven (P 3) (N 2) ~ N 2)
_test_Div_RoundHalfEven_P3_N2 =  Dict
_test_Rem_RoundHalfEven_P3_N2 :: Dict (K.Rem 'K.RoundHalfEven (P 3) (N 2) ~ N 1)
_test_Rem_RoundHalfEven_P3_N2 =  Dict
_test_Div_RoundHalfEven_P3_N3 :: Dict (K.Div 'K.RoundHalfEven (P 3) (N 3) ~ N 1)
_test_Div_RoundHalfEven_P3_N3 =  Dict
_test_Rem_RoundHalfEven_P3_N3 :: Dict (K.Rem 'K.RoundHalfEven (P 3) (N 3) ~ P 0)
_test_Rem_RoundHalfEven_P3_N3 =  Dict
_test_Div_RoundHalfEven_P3_N4 :: Dict (K.Div 'K.RoundHalfEven (P 3) (N 4) ~ N 1)
_test_Div_RoundHalfEven_P3_N4 =  Dict
_test_Rem_RoundHalfEven_P3_N4 :: Dict (K.Rem 'K.RoundHalfEven (P 3) (N 4) ~ N 1)
_test_Rem_RoundHalfEven_P3_N4 =  Dict
_test_Div_RoundHalfEven_P3_P1 :: Dict (K.Div 'K.RoundHalfEven (P 3) (P 1) ~ P 3)
_test_Div_RoundHalfEven_P3_P1 =  Dict
_test_Rem_RoundHalfEven_P3_P1 :: Dict (K.Rem 'K.RoundHalfEven (P 3) (P 1) ~ P 0)
_test_Rem_RoundHalfEven_P3_P1 =  Dict
_test_Div_RoundHalfEven_P3_P2 :: Dict (K.Div 'K.RoundHalfEven (P 3) (P 2) ~ P 2)
_test_Div_RoundHalfEven_P3_P2 =  Dict
_test_Rem_RoundHalfEven_P3_P2 :: Dict (K.Rem 'K.RoundHalfEven (P 3) (P 2) ~ N 1)
_test_Rem_RoundHalfEven_P3_P2 =  Dict
_test_Div_RoundHalfEven_P3_P3 :: Dict (K.Div 'K.RoundHalfEven (P 3) (P 3) ~ P 1)
_test_Div_RoundHalfEven_P3_P3 =  Dict
_test_Rem_RoundHalfEven_P3_P3 :: Dict (K.Rem 'K.RoundHalfEven (P 3) (P 3) ~ P 0)
_test_Rem_RoundHalfEven_P3_P3 =  Dict
_test_Div_RoundHalfEven_P3_P4 :: Dict (K.Div 'K.RoundHalfEven (P 3) (P 4) ~ P 1)
_test_Div_RoundHalfEven_P3_P4 =  Dict
_test_Rem_RoundHalfEven_P3_P4 :: Dict (K.Rem 'K.RoundHalfEven (P 3) (P 4) ~ N 1)
_test_Rem_RoundHalfEven_P3_P4 =  Dict
_test_Div_RoundHalfEven_P4_N1 :: Dict (K.Div 'K.RoundHalfEven (P 4) (N 1) ~ N 4)
_test_Div_RoundHalfEven_P4_N1 =  Dict
_test_Rem_RoundHalfEven_P4_N1 :: Dict (K.Rem 'K.RoundHalfEven (P 4) (N 1) ~ P 0)
_test_Rem_RoundHalfEven_P4_N1 =  Dict
_test_Div_RoundHalfEven_P4_N2 :: Dict (K.Div 'K.RoundHalfEven (P 4) (N 2) ~ N 2)
_test_Div_RoundHalfEven_P4_N2 =  Dict
_test_Rem_RoundHalfEven_P4_N2 :: Dict (K.Rem 'K.RoundHalfEven (P 4) (N 2) ~ P 0)
_test_Rem_RoundHalfEven_P4_N2 =  Dict
_test_Div_RoundHalfEven_P4_N3 :: Dict (K.Div 'K.RoundHalfEven (P 4) (N 3) ~ N 1)
_test_Div_RoundHalfEven_P4_N3 =  Dict
_test_Rem_RoundHalfEven_P4_N3 :: Dict (K.Rem 'K.RoundHalfEven (P 4) (N 3) ~ P 1)
_test_Rem_RoundHalfEven_P4_N3 =  Dict
_test_Div_RoundHalfEven_P4_N4 :: Dict (K.Div 'K.RoundHalfEven (P 4) (N 4) ~ N 1)
_test_Div_RoundHalfEven_P4_N4 =  Dict
_test_Rem_RoundHalfEven_P4_N4 :: Dict (K.Rem 'K.RoundHalfEven (P 4) (N 4) ~ P 0)
_test_Rem_RoundHalfEven_P4_N4 =  Dict
_test_Div_RoundHalfEven_P4_P1 :: Dict (K.Div 'K.RoundHalfEven (P 4) (P 1) ~ P 4)
_test_Div_RoundHalfEven_P4_P1 =  Dict
_test_Rem_RoundHalfEven_P4_P1 :: Dict (K.Rem 'K.RoundHalfEven (P 4) (P 1) ~ P 0)
_test_Rem_RoundHalfEven_P4_P1 =  Dict
_test_Div_RoundHalfEven_P4_P2 :: Dict (K.Div 'K.RoundHalfEven (P 4) (P 2) ~ P 2)
_test_Div_RoundHalfEven_P4_P2 =  Dict
_test_Rem_RoundHalfEven_P4_P2 :: Dict (K.Rem 'K.RoundHalfEven (P 4) (P 2) ~ P 0)
_test_Rem_RoundHalfEven_P4_P2 =  Dict
_test_Div_RoundHalfEven_P4_P3 :: Dict (K.Div 'K.RoundHalfEven (P 4) (P 3) ~ P 1)
_test_Div_RoundHalfEven_P4_P3 =  Dict
_test_Rem_RoundHalfEven_P4_P3 :: Dict (K.Rem 'K.RoundHalfEven (P 4) (P 3) ~ P 1)
_test_Rem_RoundHalfEven_P4_P3 =  Dict
_test_Div_RoundHalfEven_P4_P4 :: Dict (K.Div 'K.RoundHalfEven (P 4) (P 4) ~ P 1)
_test_Div_RoundHalfEven_P4_P4 =  Dict
_test_Rem_RoundHalfEven_P4_P4 :: Dict (K.Rem 'K.RoundHalfEven (P 4) (P 4) ~ P 0)
_test_Rem_RoundHalfEven_P4_P4 =  Dict
_test_Div_RoundHalfOdd_N1_N1 :: Dict (K.Div 'K.RoundHalfOdd (N 1) (N 1) ~ P 1)
_test_Div_RoundHalfOdd_N1_N1 =  Dict
_test_Rem_RoundHalfOdd_N1_N1 :: Dict (K.Rem 'K.RoundHalfOdd (N 1) (N 1) ~ P 0)
_test_Rem_RoundHalfOdd_N1_N1 =  Dict
_test_Div_RoundHalfOdd_N1_N2 :: Dict (K.Div 'K.RoundHalfOdd (N 1) (N 2) ~ P 1)
_test_Div_RoundHalfOdd_N1_N2 =  Dict
_test_Rem_RoundHalfOdd_N1_N2 :: Dict (K.Rem 'K.RoundHalfOdd (N 1) (N 2) ~ P 1)
_test_Rem_RoundHalfOdd_N1_N2 =  Dict
_test_Div_RoundHalfOdd_N1_N3 :: Dict (K.Div 'K.RoundHalfOdd (N 1) (N 3) ~ P 0)
_test_Div_RoundHalfOdd_N1_N3 =  Dict
_test_Rem_RoundHalfOdd_N1_N3 :: Dict (K.Rem 'K.RoundHalfOdd (N 1) (N 3) ~ N 1)
_test_Rem_RoundHalfOdd_N1_N3 =  Dict
_test_Div_RoundHalfOdd_N1_N4 :: Dict (K.Div 'K.RoundHalfOdd (N 1) (N 4) ~ P 0)
_test_Div_RoundHalfOdd_N1_N4 =  Dict
_test_Rem_RoundHalfOdd_N1_N4 :: Dict (K.Rem 'K.RoundHalfOdd (N 1) (N 4) ~ N 1)
_test_Rem_RoundHalfOdd_N1_N4 =  Dict
_test_Div_RoundHalfOdd_N1_P1 :: Dict (K.Div 'K.RoundHalfOdd (N 1) (P 1) ~ N 1)
_test_Div_RoundHalfOdd_N1_P1 =  Dict
_test_Rem_RoundHalfOdd_N1_P1 :: Dict (K.Rem 'K.RoundHalfOdd (N 1) (P 1) ~ P 0)
_test_Rem_RoundHalfOdd_N1_P1 =  Dict
_test_Div_RoundHalfOdd_N1_P2 :: Dict (K.Div 'K.RoundHalfOdd (N 1) (P 2) ~ N 1)
_test_Div_RoundHalfOdd_N1_P2 =  Dict
_test_Rem_RoundHalfOdd_N1_P2 :: Dict (K.Rem 'K.RoundHalfOdd (N 1) (P 2) ~ P 1)
_test_Rem_RoundHalfOdd_N1_P2 =  Dict
_test_Div_RoundHalfOdd_N1_P3 :: Dict (K.Div 'K.RoundHalfOdd (N 1) (P 3) ~ P 0)
_test_Div_RoundHalfOdd_N1_P3 =  Dict
_test_Rem_RoundHalfOdd_N1_P3 :: Dict (K.Rem 'K.RoundHalfOdd (N 1) (P 3) ~ N 1)
_test_Rem_RoundHalfOdd_N1_P3 =  Dict
_test_Div_RoundHalfOdd_N1_P4 :: Dict (K.Div 'K.RoundHalfOdd (N 1) (P 4) ~ P 0)
_test_Div_RoundHalfOdd_N1_P4 =  Dict
_test_Rem_RoundHalfOdd_N1_P4 :: Dict (K.Rem 'K.RoundHalfOdd (N 1) (P 4) ~ N 1)
_test_Rem_RoundHalfOdd_N1_P4 =  Dict
_test_Div_RoundHalfOdd_N2_N1 :: Dict (K.Div 'K.RoundHalfOdd (N 2) (N 1) ~ P 2)
_test_Div_RoundHalfOdd_N2_N1 =  Dict
_test_Rem_RoundHalfOdd_N2_N1 :: Dict (K.Rem 'K.RoundHalfOdd (N 2) (N 1) ~ P 0)
_test_Rem_RoundHalfOdd_N2_N1 =  Dict
_test_Div_RoundHalfOdd_N2_N2 :: Dict (K.Div 'K.RoundHalfOdd (N 2) (N 2) ~ P 1)
_test_Div_RoundHalfOdd_N2_N2 =  Dict
_test_Rem_RoundHalfOdd_N2_N2 :: Dict (K.Rem 'K.RoundHalfOdd (N 2) (N 2) ~ P 0)
_test_Rem_RoundHalfOdd_N2_N2 =  Dict
_test_Div_RoundHalfOdd_N2_N3 :: Dict (K.Div 'K.RoundHalfOdd (N 2) (N 3) ~ P 1)
_test_Div_RoundHalfOdd_N2_N3 =  Dict
_test_Rem_RoundHalfOdd_N2_N3 :: Dict (K.Rem 'K.RoundHalfOdd (N 2) (N 3) ~ P 1)
_test_Rem_RoundHalfOdd_N2_N3 =  Dict
_test_Div_RoundHalfOdd_N2_N4 :: Dict (K.Div 'K.RoundHalfOdd (N 2) (N 4) ~ P 1)
_test_Div_RoundHalfOdd_N2_N4 =  Dict
_test_Rem_RoundHalfOdd_N2_N4 :: Dict (K.Rem 'K.RoundHalfOdd (N 2) (N 4) ~ P 2)
_test_Rem_RoundHalfOdd_N2_N4 =  Dict
_test_Div_RoundHalfOdd_N2_P1 :: Dict (K.Div 'K.RoundHalfOdd (N 2) (P 1) ~ N 2)
_test_Div_RoundHalfOdd_N2_P1 =  Dict
_test_Rem_RoundHalfOdd_N2_P1 :: Dict (K.Rem 'K.RoundHalfOdd (N 2) (P 1) ~ P 0)
_test_Rem_RoundHalfOdd_N2_P1 =  Dict
_test_Div_RoundHalfOdd_N2_P2 :: Dict (K.Div 'K.RoundHalfOdd (N 2) (P 2) ~ N 1)
_test_Div_RoundHalfOdd_N2_P2 =  Dict
_test_Rem_RoundHalfOdd_N2_P2 :: Dict (K.Rem 'K.RoundHalfOdd (N 2) (P 2) ~ P 0)
_test_Rem_RoundHalfOdd_N2_P2 =  Dict
_test_Div_RoundHalfOdd_N2_P3 :: Dict (K.Div 'K.RoundHalfOdd (N 2) (P 3) ~ N 1)
_test_Div_RoundHalfOdd_N2_P3 =  Dict
_test_Rem_RoundHalfOdd_N2_P3 :: Dict (K.Rem 'K.RoundHalfOdd (N 2) (P 3) ~ P 1)
_test_Rem_RoundHalfOdd_N2_P3 =  Dict
_test_Div_RoundHalfOdd_N2_P4 :: Dict (K.Div 'K.RoundHalfOdd (N 2) (P 4) ~ N 1)
_test_Div_RoundHalfOdd_N2_P4 =  Dict
_test_Rem_RoundHalfOdd_N2_P4 :: Dict (K.Rem 'K.RoundHalfOdd (N 2) (P 4) ~ P 2)
_test_Rem_RoundHalfOdd_N2_P4 =  Dict
_test_Div_RoundHalfOdd_N3_N1 :: Dict (K.Div 'K.RoundHalfOdd (N 3) (N 1) ~ P 3)
_test_Div_RoundHalfOdd_N3_N1 =  Dict
_test_Rem_RoundHalfOdd_N3_N1 :: Dict (K.Rem 'K.RoundHalfOdd (N 3) (N 1) ~ P 0)
_test_Rem_RoundHalfOdd_N3_N1 =  Dict
_test_Div_RoundHalfOdd_N3_N2 :: Dict (K.Div 'K.RoundHalfOdd (N 3) (N 2) ~ P 1)
_test_Div_RoundHalfOdd_N3_N2 =  Dict
_test_Rem_RoundHalfOdd_N3_N2 :: Dict (K.Rem 'K.RoundHalfOdd (N 3) (N 2) ~ N 1)
_test_Rem_RoundHalfOdd_N3_N2 =  Dict
_test_Div_RoundHalfOdd_N3_N3 :: Dict (K.Div 'K.RoundHalfOdd (N 3) (N 3) ~ P 1)
_test_Div_RoundHalfOdd_N3_N3 =  Dict
_test_Rem_RoundHalfOdd_N3_N3 :: Dict (K.Rem 'K.RoundHalfOdd (N 3) (N 3) ~ P 0)
_test_Rem_RoundHalfOdd_N3_N3 =  Dict
_test_Div_RoundHalfOdd_N3_N4 :: Dict (K.Div 'K.RoundHalfOdd (N 3) (N 4) ~ P 1)
_test_Div_RoundHalfOdd_N3_N4 =  Dict
_test_Rem_RoundHalfOdd_N3_N4 :: Dict (K.Rem 'K.RoundHalfOdd (N 3) (N 4) ~ P 1)
_test_Rem_RoundHalfOdd_N3_N4 =  Dict
_test_Div_RoundHalfOdd_N3_P1 :: Dict (K.Div 'K.RoundHalfOdd (N 3) (P 1) ~ N 3)
_test_Div_RoundHalfOdd_N3_P1 =  Dict
_test_Rem_RoundHalfOdd_N3_P1 :: Dict (K.Rem 'K.RoundHalfOdd (N 3) (P 1) ~ P 0)
_test_Rem_RoundHalfOdd_N3_P1 =  Dict
_test_Div_RoundHalfOdd_N3_P2 :: Dict (K.Div 'K.RoundHalfOdd (N 3) (P 2) ~ N 1)
_test_Div_RoundHalfOdd_N3_P2 =  Dict
_test_Rem_RoundHalfOdd_N3_P2 :: Dict (K.Rem 'K.RoundHalfOdd (N 3) (P 2) ~ N 1)
_test_Rem_RoundHalfOdd_N3_P2 =  Dict
_test_Div_RoundHalfOdd_N3_P3 :: Dict (K.Div 'K.RoundHalfOdd (N 3) (P 3) ~ N 1)
_test_Div_RoundHalfOdd_N3_P3 =  Dict
_test_Rem_RoundHalfOdd_N3_P3 :: Dict (K.Rem 'K.RoundHalfOdd (N 3) (P 3) ~ P 0)
_test_Rem_RoundHalfOdd_N3_P3 =  Dict
_test_Div_RoundHalfOdd_N3_P4 :: Dict (K.Div 'K.RoundHalfOdd (N 3) (P 4) ~ N 1)
_test_Div_RoundHalfOdd_N3_P4 =  Dict
_test_Rem_RoundHalfOdd_N3_P4 :: Dict (K.Rem 'K.RoundHalfOdd (N 3) (P 4) ~ P 1)
_test_Rem_RoundHalfOdd_N3_P4 =  Dict
_test_Div_RoundHalfOdd_N4_N1 :: Dict (K.Div 'K.RoundHalfOdd (N 4) (N 1) ~ P 4)
_test_Div_RoundHalfOdd_N4_N1 =  Dict
_test_Rem_RoundHalfOdd_N4_N1 :: Dict (K.Rem 'K.RoundHalfOdd (N 4) (N 1) ~ P 0)
_test_Rem_RoundHalfOdd_N4_N1 =  Dict
_test_Div_RoundHalfOdd_N4_N2 :: Dict (K.Div 'K.RoundHalfOdd (N 4) (N 2) ~ P 2)
_test_Div_RoundHalfOdd_N4_N2 =  Dict
_test_Rem_RoundHalfOdd_N4_N2 :: Dict (K.Rem 'K.RoundHalfOdd (N 4) (N 2) ~ P 0)
_test_Rem_RoundHalfOdd_N4_N2 =  Dict
_test_Div_RoundHalfOdd_N4_N3 :: Dict (K.Div 'K.RoundHalfOdd (N 4) (N 3) ~ P 1)
_test_Div_RoundHalfOdd_N4_N3 =  Dict
_test_Rem_RoundHalfOdd_N4_N3 :: Dict (K.Rem 'K.RoundHalfOdd (N 4) (N 3) ~ N 1)
_test_Rem_RoundHalfOdd_N4_N3 =  Dict
_test_Div_RoundHalfOdd_N4_N4 :: Dict (K.Div 'K.RoundHalfOdd (N 4) (N 4) ~ P 1)
_test_Div_RoundHalfOdd_N4_N4 =  Dict
_test_Rem_RoundHalfOdd_N4_N4 :: Dict (K.Rem 'K.RoundHalfOdd (N 4) (N 4) ~ P 0)
_test_Rem_RoundHalfOdd_N4_N4 =  Dict
_test_Div_RoundHalfOdd_N4_P1 :: Dict (K.Div 'K.RoundHalfOdd (N 4) (P 1) ~ N 4)
_test_Div_RoundHalfOdd_N4_P1 =  Dict
_test_Rem_RoundHalfOdd_N4_P1 :: Dict (K.Rem 'K.RoundHalfOdd (N 4) (P 1) ~ P 0)
_test_Rem_RoundHalfOdd_N4_P1 =  Dict
_test_Div_RoundHalfOdd_N4_P2 :: Dict (K.Div 'K.RoundHalfOdd (N 4) (P 2) ~ N 2)
_test_Div_RoundHalfOdd_N4_P2 =  Dict
_test_Rem_RoundHalfOdd_N4_P2 :: Dict (K.Rem 'K.RoundHalfOdd (N 4) (P 2) ~ P 0)
_test_Rem_RoundHalfOdd_N4_P2 =  Dict
_test_Div_RoundHalfOdd_N4_P3 :: Dict (K.Div 'K.RoundHalfOdd (N 4) (P 3) ~ N 1)
_test_Div_RoundHalfOdd_N4_P3 =  Dict
_test_Rem_RoundHalfOdd_N4_P3 :: Dict (K.Rem 'K.RoundHalfOdd (N 4) (P 3) ~ N 1)
_test_Rem_RoundHalfOdd_N4_P3 =  Dict
_test_Div_RoundHalfOdd_N4_P4 :: Dict (K.Div 'K.RoundHalfOdd (N 4) (P 4) ~ N 1)
_test_Div_RoundHalfOdd_N4_P4 =  Dict
_test_Rem_RoundHalfOdd_N4_P4 :: Dict (K.Rem 'K.RoundHalfOdd (N 4) (P 4) ~ P 0)
_test_Rem_RoundHalfOdd_N4_P4 =  Dict
_test_Div_RoundHalfOdd_P0_N1 :: Dict (K.Div 'K.RoundHalfOdd (P 0) (N 1) ~ P 0)
_test_Div_RoundHalfOdd_P0_N1 =  Dict
_test_Rem_RoundHalfOdd_P0_N1 :: Dict (K.Rem 'K.RoundHalfOdd (P 0) (N 1) ~ P 0)
_test_Rem_RoundHalfOdd_P0_N1 =  Dict
_test_Div_RoundHalfOdd_P0_N2 :: Dict (K.Div 'K.RoundHalfOdd (P 0) (N 2) ~ P 0)
_test_Div_RoundHalfOdd_P0_N2 =  Dict
_test_Rem_RoundHalfOdd_P0_N2 :: Dict (K.Rem 'K.RoundHalfOdd (P 0) (N 2) ~ P 0)
_test_Rem_RoundHalfOdd_P0_N2 =  Dict
_test_Div_RoundHalfOdd_P0_N3 :: Dict (K.Div 'K.RoundHalfOdd (P 0) (N 3) ~ P 0)
_test_Div_RoundHalfOdd_P0_N3 =  Dict
_test_Rem_RoundHalfOdd_P0_N3 :: Dict (K.Rem 'K.RoundHalfOdd (P 0) (N 3) ~ P 0)
_test_Rem_RoundHalfOdd_P0_N3 =  Dict
_test_Div_RoundHalfOdd_P0_N4 :: Dict (K.Div 'K.RoundHalfOdd (P 0) (N 4) ~ P 0)
_test_Div_RoundHalfOdd_P0_N4 =  Dict
_test_Rem_RoundHalfOdd_P0_N4 :: Dict (K.Rem 'K.RoundHalfOdd (P 0) (N 4) ~ P 0)
_test_Rem_RoundHalfOdd_P0_N4 =  Dict
_test_Div_RoundHalfOdd_P0_P1 :: Dict (K.Div 'K.RoundHalfOdd (P 0) (P 1) ~ P 0)
_test_Div_RoundHalfOdd_P0_P1 =  Dict
_test_Rem_RoundHalfOdd_P0_P1 :: Dict (K.Rem 'K.RoundHalfOdd (P 0) (P 1) ~ P 0)
_test_Rem_RoundHalfOdd_P0_P1 =  Dict
_test_Div_RoundHalfOdd_P0_P2 :: Dict (K.Div 'K.RoundHalfOdd (P 0) (P 2) ~ P 0)
_test_Div_RoundHalfOdd_P0_P2 =  Dict
_test_Rem_RoundHalfOdd_P0_P2 :: Dict (K.Rem 'K.RoundHalfOdd (P 0) (P 2) ~ P 0)
_test_Rem_RoundHalfOdd_P0_P2 =  Dict
_test_Div_RoundHalfOdd_P0_P3 :: Dict (K.Div 'K.RoundHalfOdd (P 0) (P 3) ~ P 0)
_test_Div_RoundHalfOdd_P0_P3 =  Dict
_test_Rem_RoundHalfOdd_P0_P3 :: Dict (K.Rem 'K.RoundHalfOdd (P 0) (P 3) ~ P 0)
_test_Rem_RoundHalfOdd_P0_P3 =  Dict
_test_Div_RoundHalfOdd_P0_P4 :: Dict (K.Div 'K.RoundHalfOdd (P 0) (P 4) ~ P 0)
_test_Div_RoundHalfOdd_P0_P4 =  Dict
_test_Rem_RoundHalfOdd_P0_P4 :: Dict (K.Rem 'K.RoundHalfOdd (P 0) (P 4) ~ P 0)
_test_Rem_RoundHalfOdd_P0_P4 =  Dict
_test_Div_RoundHalfOdd_P1_N1 :: Dict (K.Div 'K.RoundHalfOdd (P 1) (N 1) ~ N 1)
_test_Div_RoundHalfOdd_P1_N1 =  Dict
_test_Rem_RoundHalfOdd_P1_N1 :: Dict (K.Rem 'K.RoundHalfOdd (P 1) (N 1) ~ P 0)
_test_Rem_RoundHalfOdd_P1_N1 =  Dict
_test_Div_RoundHalfOdd_P1_N2 :: Dict (K.Div 'K.RoundHalfOdd (P 1) (N 2) ~ N 1)
_test_Div_RoundHalfOdd_P1_N2 =  Dict
_test_Rem_RoundHalfOdd_P1_N2 :: Dict (K.Rem 'K.RoundHalfOdd (P 1) (N 2) ~ N 1)
_test_Rem_RoundHalfOdd_P1_N2 =  Dict
_test_Div_RoundHalfOdd_P1_N3 :: Dict (K.Div 'K.RoundHalfOdd (P 1) (N 3) ~ P 0)
_test_Div_RoundHalfOdd_P1_N3 =  Dict
_test_Rem_RoundHalfOdd_P1_N3 :: Dict (K.Rem 'K.RoundHalfOdd (P 1) (N 3) ~ P 1)
_test_Rem_RoundHalfOdd_P1_N3 =  Dict
_test_Div_RoundHalfOdd_P1_N4 :: Dict (K.Div 'K.RoundHalfOdd (P 1) (N 4) ~ P 0)
_test_Div_RoundHalfOdd_P1_N4 =  Dict
_test_Rem_RoundHalfOdd_P1_N4 :: Dict (K.Rem 'K.RoundHalfOdd (P 1) (N 4) ~ P 1)
_test_Rem_RoundHalfOdd_P1_N4 =  Dict
_test_Div_RoundHalfOdd_P1_P1 :: Dict (K.Div 'K.RoundHalfOdd (P 1) (P 1) ~ P 1)
_test_Div_RoundHalfOdd_P1_P1 =  Dict
_test_Rem_RoundHalfOdd_P1_P1 :: Dict (K.Rem 'K.RoundHalfOdd (P 1) (P 1) ~ P 0)
_test_Rem_RoundHalfOdd_P1_P1 =  Dict
_test_Div_RoundHalfOdd_P1_P2 :: Dict (K.Div 'K.RoundHalfOdd (P 1) (P 2) ~ P 1)
_test_Div_RoundHalfOdd_P1_P2 =  Dict
_test_Rem_RoundHalfOdd_P1_P2 :: Dict (K.Rem 'K.RoundHalfOdd (P 1) (P 2) ~ N 1)
_test_Rem_RoundHalfOdd_P1_P2 =  Dict
_test_Div_RoundHalfOdd_P1_P3 :: Dict (K.Div 'K.RoundHalfOdd (P 1) (P 3) ~ P 0)
_test_Div_RoundHalfOdd_P1_P3 =  Dict
_test_Rem_RoundHalfOdd_P1_P3 :: Dict (K.Rem 'K.RoundHalfOdd (P 1) (P 3) ~ P 1)
_test_Rem_RoundHalfOdd_P1_P3 =  Dict
_test_Div_RoundHalfOdd_P1_P4 :: Dict (K.Div 'K.RoundHalfOdd (P 1) (P 4) ~ P 0)
_test_Div_RoundHalfOdd_P1_P4 =  Dict
_test_Rem_RoundHalfOdd_P1_P4 :: Dict (K.Rem 'K.RoundHalfOdd (P 1) (P 4) ~ P 1)
_test_Rem_RoundHalfOdd_P1_P4 =  Dict
_test_Div_RoundHalfOdd_P2_N1 :: Dict (K.Div 'K.RoundHalfOdd (P 2) (N 1) ~ N 2)
_test_Div_RoundHalfOdd_P2_N1 =  Dict
_test_Rem_RoundHalfOdd_P2_N1 :: Dict (K.Rem 'K.RoundHalfOdd (P 2) (N 1) ~ P 0)
_test_Rem_RoundHalfOdd_P2_N1 =  Dict
_test_Div_RoundHalfOdd_P2_N2 :: Dict (K.Div 'K.RoundHalfOdd (P 2) (N 2) ~ N 1)
_test_Div_RoundHalfOdd_P2_N2 =  Dict
_test_Rem_RoundHalfOdd_P2_N2 :: Dict (K.Rem 'K.RoundHalfOdd (P 2) (N 2) ~ P 0)
_test_Rem_RoundHalfOdd_P2_N2 =  Dict
_test_Div_RoundHalfOdd_P2_N3 :: Dict (K.Div 'K.RoundHalfOdd (P 2) (N 3) ~ N 1)
_test_Div_RoundHalfOdd_P2_N3 =  Dict
_test_Rem_RoundHalfOdd_P2_N3 :: Dict (K.Rem 'K.RoundHalfOdd (P 2) (N 3) ~ N 1)
_test_Rem_RoundHalfOdd_P2_N3 =  Dict
_test_Div_RoundHalfOdd_P2_N4 :: Dict (K.Div 'K.RoundHalfOdd (P 2) (N 4) ~ N 1)
_test_Div_RoundHalfOdd_P2_N4 =  Dict
_test_Rem_RoundHalfOdd_P2_N4 :: Dict (K.Rem 'K.RoundHalfOdd (P 2) (N 4) ~ N 2)
_test_Rem_RoundHalfOdd_P2_N4 =  Dict
_test_Div_RoundHalfOdd_P2_P1 :: Dict (K.Div 'K.RoundHalfOdd (P 2) (P 1) ~ P 2)
_test_Div_RoundHalfOdd_P2_P1 =  Dict
_test_Rem_RoundHalfOdd_P2_P1 :: Dict (K.Rem 'K.RoundHalfOdd (P 2) (P 1) ~ P 0)
_test_Rem_RoundHalfOdd_P2_P1 =  Dict
_test_Div_RoundHalfOdd_P2_P2 :: Dict (K.Div 'K.RoundHalfOdd (P 2) (P 2) ~ P 1)
_test_Div_RoundHalfOdd_P2_P2 =  Dict
_test_Rem_RoundHalfOdd_P2_P2 :: Dict (K.Rem 'K.RoundHalfOdd (P 2) (P 2) ~ P 0)
_test_Rem_RoundHalfOdd_P2_P2 =  Dict
_test_Div_RoundHalfOdd_P2_P3 :: Dict (K.Div 'K.RoundHalfOdd (P 2) (P 3) ~ P 1)
_test_Div_RoundHalfOdd_P2_P3 =  Dict
_test_Rem_RoundHalfOdd_P2_P3 :: Dict (K.Rem 'K.RoundHalfOdd (P 2) (P 3) ~ N 1)
_test_Rem_RoundHalfOdd_P2_P3 =  Dict
_test_Div_RoundHalfOdd_P2_P4 :: Dict (K.Div 'K.RoundHalfOdd (P 2) (P 4) ~ P 1)
_test_Div_RoundHalfOdd_P2_P4 =  Dict
_test_Rem_RoundHalfOdd_P2_P4 :: Dict (K.Rem 'K.RoundHalfOdd (P 2) (P 4) ~ N 2)
_test_Rem_RoundHalfOdd_P2_P4 =  Dict
_test_Div_RoundHalfOdd_P3_N1 :: Dict (K.Div 'K.RoundHalfOdd (P 3) (N 1) ~ N 3)
_test_Div_RoundHalfOdd_P3_N1 =  Dict
_test_Rem_RoundHalfOdd_P3_N1 :: Dict (K.Rem 'K.RoundHalfOdd (P 3) (N 1) ~ P 0)
_test_Rem_RoundHalfOdd_P3_N1 =  Dict
_test_Div_RoundHalfOdd_P3_N2 :: Dict (K.Div 'K.RoundHalfOdd (P 3) (N 2) ~ N 1)
_test_Div_RoundHalfOdd_P3_N2 =  Dict
_test_Rem_RoundHalfOdd_P3_N2 :: Dict (K.Rem 'K.RoundHalfOdd (P 3) (N 2) ~ P 1)
_test_Rem_RoundHalfOdd_P3_N2 =  Dict
_test_Div_RoundHalfOdd_P3_N3 :: Dict (K.Div 'K.RoundHalfOdd (P 3) (N 3) ~ N 1)
_test_Div_RoundHalfOdd_P3_N3 =  Dict
_test_Rem_RoundHalfOdd_P3_N3 :: Dict (K.Rem 'K.RoundHalfOdd (P 3) (N 3) ~ P 0)
_test_Rem_RoundHalfOdd_P3_N3 =  Dict
_test_Div_RoundHalfOdd_P3_N4 :: Dict (K.Div 'K.RoundHalfOdd (P 3) (N 4) ~ N 1)
_test_Div_RoundHalfOdd_P3_N4 =  Dict
_test_Rem_RoundHalfOdd_P3_N4 :: Dict (K.Rem 'K.RoundHalfOdd (P 3) (N 4) ~ N 1)
_test_Rem_RoundHalfOdd_P3_N4 =  Dict
_test_Div_RoundHalfOdd_P3_P1 :: Dict (K.Div 'K.RoundHalfOdd (P 3) (P 1) ~ P 3)
_test_Div_RoundHalfOdd_P3_P1 =  Dict
_test_Rem_RoundHalfOdd_P3_P1 :: Dict (K.Rem 'K.RoundHalfOdd (P 3) (P 1) ~ P 0)
_test_Rem_RoundHalfOdd_P3_P1 =  Dict
_test_Div_RoundHalfOdd_P3_P2 :: Dict (K.Div 'K.RoundHalfOdd (P 3) (P 2) ~ P 1)
_test_Div_RoundHalfOdd_P3_P2 =  Dict
_test_Rem_RoundHalfOdd_P3_P2 :: Dict (K.Rem 'K.RoundHalfOdd (P 3) (P 2) ~ P 1)
_test_Rem_RoundHalfOdd_P3_P2 =  Dict
_test_Div_RoundHalfOdd_P3_P3 :: Dict (K.Div 'K.RoundHalfOdd (P 3) (P 3) ~ P 1)
_test_Div_RoundHalfOdd_P3_P3 =  Dict
_test_Rem_RoundHalfOdd_P3_P3 :: Dict (K.Rem 'K.RoundHalfOdd (P 3) (P 3) ~ P 0)
_test_Rem_RoundHalfOdd_P3_P3 =  Dict
_test_Div_RoundHalfOdd_P3_P4 :: Dict (K.Div 'K.RoundHalfOdd (P 3) (P 4) ~ P 1)
_test_Div_RoundHalfOdd_P3_P4 =  Dict
_test_Rem_RoundHalfOdd_P3_P4 :: Dict (K.Rem 'K.RoundHalfOdd (P 3) (P 4) ~ N 1)
_test_Rem_RoundHalfOdd_P3_P4 =  Dict
_test_Div_RoundHalfOdd_P4_N1 :: Dict (K.Div 'K.RoundHalfOdd (P 4) (N 1) ~ N 4)
_test_Div_RoundHalfOdd_P4_N1 =  Dict
_test_Rem_RoundHalfOdd_P4_N1 :: Dict (K.Rem 'K.RoundHalfOdd (P 4) (N 1) ~ P 0)
_test_Rem_RoundHalfOdd_P4_N1 =  Dict
_test_Div_RoundHalfOdd_P4_N2 :: Dict (K.Div 'K.RoundHalfOdd (P 4) (N 2) ~ N 2)
_test_Div_RoundHalfOdd_P4_N2 =  Dict
_test_Rem_RoundHalfOdd_P4_N2 :: Dict (K.Rem 'K.RoundHalfOdd (P 4) (N 2) ~ P 0)
_test_Rem_RoundHalfOdd_P4_N2 =  Dict
_test_Div_RoundHalfOdd_P4_N3 :: Dict (K.Div 'K.RoundHalfOdd (P 4) (N 3) ~ N 1)
_test_Div_RoundHalfOdd_P4_N3 =  Dict
_test_Rem_RoundHalfOdd_P4_N3 :: Dict (K.Rem 'K.RoundHalfOdd (P 4) (N 3) ~ P 1)
_test_Rem_RoundHalfOdd_P4_N3 =  Dict
_test_Div_RoundHalfOdd_P4_N4 :: Dict (K.Div 'K.RoundHalfOdd (P 4) (N 4) ~ N 1)
_test_Div_RoundHalfOdd_P4_N4 =  Dict
_test_Rem_RoundHalfOdd_P4_N4 :: Dict (K.Rem 'K.RoundHalfOdd (P 4) (N 4) ~ P 0)
_test_Rem_RoundHalfOdd_P4_N4 =  Dict
_test_Div_RoundHalfOdd_P4_P1 :: Dict (K.Div 'K.RoundHalfOdd (P 4) (P 1) ~ P 4)
_test_Div_RoundHalfOdd_P4_P1 =  Dict
_test_Rem_RoundHalfOdd_P4_P1 :: Dict (K.Rem 'K.RoundHalfOdd (P 4) (P 1) ~ P 0)
_test_Rem_RoundHalfOdd_P4_P1 =  Dict
_test_Div_RoundHalfOdd_P4_P2 :: Dict (K.Div 'K.RoundHalfOdd (P 4) (P 2) ~ P 2)
_test_Div_RoundHalfOdd_P4_P2 =  Dict
_test_Rem_RoundHalfOdd_P4_P2 :: Dict (K.Rem 'K.RoundHalfOdd (P 4) (P 2) ~ P 0)
_test_Rem_RoundHalfOdd_P4_P2 =  Dict
_test_Div_RoundHalfOdd_P4_P3 :: Dict (K.Div 'K.RoundHalfOdd (P 4) (P 3) ~ P 1)
_test_Div_RoundHalfOdd_P4_P3 =  Dict
_test_Rem_RoundHalfOdd_P4_P3 :: Dict (K.Rem 'K.RoundHalfOdd (P 4) (P 3) ~ P 1)
_test_Rem_RoundHalfOdd_P4_P3 =  Dict
_test_Div_RoundHalfOdd_P4_P4 :: Dict (K.Div 'K.RoundHalfOdd (P 4) (P 4) ~ P 1)
_test_Div_RoundHalfOdd_P4_P4 =  Dict
_test_Rem_RoundHalfOdd_P4_P4 :: Dict (K.Rem 'K.RoundHalfOdd (P 4) (P 4) ~ P 0)
_test_Rem_RoundHalfOdd_P4_P4 =  Dict
_test_Div_RoundHalfUp_N1_N1 :: Dict (K.Div 'K.RoundHalfUp (N 1) (N 1) ~ P 1)
_test_Div_RoundHalfUp_N1_N1 =  Dict
_test_Rem_RoundHalfUp_N1_N1 :: Dict (K.Rem 'K.RoundHalfUp (N 1) (N 1) ~ P 0)
_test_Rem_RoundHalfUp_N1_N1 =  Dict
_test_Div_RoundHalfUp_N1_N2 :: Dict (K.Div 'K.RoundHalfUp (N 1) (N 2) ~ P 1)
_test_Div_RoundHalfUp_N1_N2 =  Dict
_test_Rem_RoundHalfUp_N1_N2 :: Dict (K.Rem 'K.RoundHalfUp (N 1) (N 2) ~ P 1)
_test_Rem_RoundHalfUp_N1_N2 =  Dict
_test_Div_RoundHalfUp_N1_N3 :: Dict (K.Div 'K.RoundHalfUp (N 1) (N 3) ~ P 0)
_test_Div_RoundHalfUp_N1_N3 =  Dict
_test_Rem_RoundHalfUp_N1_N3 :: Dict (K.Rem 'K.RoundHalfUp (N 1) (N 3) ~ N 1)
_test_Rem_RoundHalfUp_N1_N3 =  Dict
_test_Div_RoundHalfUp_N1_N4 :: Dict (K.Div 'K.RoundHalfUp (N 1) (N 4) ~ P 0)
_test_Div_RoundHalfUp_N1_N4 =  Dict
_test_Rem_RoundHalfUp_N1_N4 :: Dict (K.Rem 'K.RoundHalfUp (N 1) (N 4) ~ N 1)
_test_Rem_RoundHalfUp_N1_N4 =  Dict
_test_Div_RoundHalfUp_N1_P1 :: Dict (K.Div 'K.RoundHalfUp (N 1) (P 1) ~ N 1)
_test_Div_RoundHalfUp_N1_P1 =  Dict
_test_Rem_RoundHalfUp_N1_P1 :: Dict (K.Rem 'K.RoundHalfUp (N 1) (P 1) ~ P 0)
_test_Rem_RoundHalfUp_N1_P1 =  Dict
_test_Div_RoundHalfUp_N1_P2 :: Dict (K.Div 'K.RoundHalfUp (N 1) (P 2) ~ P 0)
_test_Div_RoundHalfUp_N1_P2 =  Dict
_test_Rem_RoundHalfUp_N1_P2 :: Dict (K.Rem 'K.RoundHalfUp (N 1) (P 2) ~ N 1)
_test_Rem_RoundHalfUp_N1_P2 =  Dict
_test_Div_RoundHalfUp_N1_P3 :: Dict (K.Div 'K.RoundHalfUp (N 1) (P 3) ~ P 0)
_test_Div_RoundHalfUp_N1_P3 =  Dict
_test_Rem_RoundHalfUp_N1_P3 :: Dict (K.Rem 'K.RoundHalfUp (N 1) (P 3) ~ N 1)
_test_Rem_RoundHalfUp_N1_P3 =  Dict
_test_Div_RoundHalfUp_N1_P4 :: Dict (K.Div 'K.RoundHalfUp (N 1) (P 4) ~ P 0)
_test_Div_RoundHalfUp_N1_P4 =  Dict
_test_Rem_RoundHalfUp_N1_P4 :: Dict (K.Rem 'K.RoundHalfUp (N 1) (P 4) ~ N 1)
_test_Rem_RoundHalfUp_N1_P4 =  Dict
_test_Div_RoundHalfUp_N2_N1 :: Dict (K.Div 'K.RoundHalfUp (N 2) (N 1) ~ P 2)
_test_Div_RoundHalfUp_N2_N1 =  Dict
_test_Rem_RoundHalfUp_N2_N1 :: Dict (K.Rem 'K.RoundHalfUp (N 2) (N 1) ~ P 0)
_test_Rem_RoundHalfUp_N2_N1 =  Dict
_test_Div_RoundHalfUp_N2_N2 :: Dict (K.Div 'K.RoundHalfUp (N 2) (N 2) ~ P 1)
_test_Div_RoundHalfUp_N2_N2 =  Dict
_test_Rem_RoundHalfUp_N2_N2 :: Dict (K.Rem 'K.RoundHalfUp (N 2) (N 2) ~ P 0)
_test_Rem_RoundHalfUp_N2_N2 =  Dict
_test_Div_RoundHalfUp_N2_N3 :: Dict (K.Div 'K.RoundHalfUp (N 2) (N 3) ~ P 1)
_test_Div_RoundHalfUp_N2_N3 =  Dict
_test_Rem_RoundHalfUp_N2_N3 :: Dict (K.Rem 'K.RoundHalfUp (N 2) (N 3) ~ P 1)
_test_Rem_RoundHalfUp_N2_N3 =  Dict
_test_Div_RoundHalfUp_N2_N4 :: Dict (K.Div 'K.RoundHalfUp (N 2) (N 4) ~ P 1)
_test_Div_RoundHalfUp_N2_N4 =  Dict
_test_Rem_RoundHalfUp_N2_N4 :: Dict (K.Rem 'K.RoundHalfUp (N 2) (N 4) ~ P 2)
_test_Rem_RoundHalfUp_N2_N4 =  Dict
_test_Div_RoundHalfUp_N2_P1 :: Dict (K.Div 'K.RoundHalfUp (N 2) (P 1) ~ N 2)
_test_Div_RoundHalfUp_N2_P1 =  Dict
_test_Rem_RoundHalfUp_N2_P1 :: Dict (K.Rem 'K.RoundHalfUp (N 2) (P 1) ~ P 0)
_test_Rem_RoundHalfUp_N2_P1 =  Dict
_test_Div_RoundHalfUp_N2_P2 :: Dict (K.Div 'K.RoundHalfUp (N 2) (P 2) ~ N 1)
_test_Div_RoundHalfUp_N2_P2 =  Dict
_test_Rem_RoundHalfUp_N2_P2 :: Dict (K.Rem 'K.RoundHalfUp (N 2) (P 2) ~ P 0)
_test_Rem_RoundHalfUp_N2_P2 =  Dict
_test_Div_RoundHalfUp_N2_P3 :: Dict (K.Div 'K.RoundHalfUp (N 2) (P 3) ~ N 1)
_test_Div_RoundHalfUp_N2_P3 =  Dict
_test_Rem_RoundHalfUp_N2_P3 :: Dict (K.Rem 'K.RoundHalfUp (N 2) (P 3) ~ P 1)
_test_Rem_RoundHalfUp_N2_P3 =  Dict
_test_Div_RoundHalfUp_N2_P4 :: Dict (K.Div 'K.RoundHalfUp (N 2) (P 4) ~ P 0)
_test_Div_RoundHalfUp_N2_P4 =  Dict
_test_Rem_RoundHalfUp_N2_P4 :: Dict (K.Rem 'K.RoundHalfUp (N 2) (P 4) ~ N 2)
_test_Rem_RoundHalfUp_N2_P4 =  Dict
_test_Div_RoundHalfUp_N3_N1 :: Dict (K.Div 'K.RoundHalfUp (N 3) (N 1) ~ P 3)
_test_Div_RoundHalfUp_N3_N1 =  Dict
_test_Rem_RoundHalfUp_N3_N1 :: Dict (K.Rem 'K.RoundHalfUp (N 3) (N 1) ~ P 0)
_test_Rem_RoundHalfUp_N3_N1 =  Dict
_test_Div_RoundHalfUp_N3_N2 :: Dict (K.Div 'K.RoundHalfUp (N 3) (N 2) ~ P 2)
_test_Div_RoundHalfUp_N3_N2 =  Dict
_test_Rem_RoundHalfUp_N3_N2 :: Dict (K.Rem 'K.RoundHalfUp (N 3) (N 2) ~ P 1)
_test_Rem_RoundHalfUp_N3_N2 =  Dict
_test_Div_RoundHalfUp_N3_N3 :: Dict (K.Div 'K.RoundHalfUp (N 3) (N 3) ~ P 1)
_test_Div_RoundHalfUp_N3_N3 =  Dict
_test_Rem_RoundHalfUp_N3_N3 :: Dict (K.Rem 'K.RoundHalfUp (N 3) (N 3) ~ P 0)
_test_Rem_RoundHalfUp_N3_N3 =  Dict
_test_Div_RoundHalfUp_N3_N4 :: Dict (K.Div 'K.RoundHalfUp (N 3) (N 4) ~ P 1)
_test_Div_RoundHalfUp_N3_N4 =  Dict
_test_Rem_RoundHalfUp_N3_N4 :: Dict (K.Rem 'K.RoundHalfUp (N 3) (N 4) ~ P 1)
_test_Rem_RoundHalfUp_N3_N4 =  Dict
_test_Div_RoundHalfUp_N3_P1 :: Dict (K.Div 'K.RoundHalfUp (N 3) (P 1) ~ N 3)
_test_Div_RoundHalfUp_N3_P1 =  Dict
_test_Rem_RoundHalfUp_N3_P1 :: Dict (K.Rem 'K.RoundHalfUp (N 3) (P 1) ~ P 0)
_test_Rem_RoundHalfUp_N3_P1 =  Dict
_test_Div_RoundHalfUp_N3_P2 :: Dict (K.Div 'K.RoundHalfUp (N 3) (P 2) ~ N 1)
_test_Div_RoundHalfUp_N3_P2 =  Dict
_test_Rem_RoundHalfUp_N3_P2 :: Dict (K.Rem 'K.RoundHalfUp (N 3) (P 2) ~ N 1)
_test_Rem_RoundHalfUp_N3_P2 =  Dict
_test_Div_RoundHalfUp_N3_P3 :: Dict (K.Div 'K.RoundHalfUp (N 3) (P 3) ~ N 1)
_test_Div_RoundHalfUp_N3_P3 =  Dict
_test_Rem_RoundHalfUp_N3_P3 :: Dict (K.Rem 'K.RoundHalfUp (N 3) (P 3) ~ P 0)
_test_Rem_RoundHalfUp_N3_P3 =  Dict
_test_Div_RoundHalfUp_N3_P4 :: Dict (K.Div 'K.RoundHalfUp (N 3) (P 4) ~ N 1)
_test_Div_RoundHalfUp_N3_P4 =  Dict
_test_Rem_RoundHalfUp_N3_P4 :: Dict (K.Rem 'K.RoundHalfUp (N 3) (P 4) ~ P 1)
_test_Rem_RoundHalfUp_N3_P4 =  Dict
_test_Div_RoundHalfUp_N4_N1 :: Dict (K.Div 'K.RoundHalfUp (N 4) (N 1) ~ P 4)
_test_Div_RoundHalfUp_N4_N1 =  Dict
_test_Rem_RoundHalfUp_N4_N1 :: Dict (K.Rem 'K.RoundHalfUp (N 4) (N 1) ~ P 0)
_test_Rem_RoundHalfUp_N4_N1 =  Dict
_test_Div_RoundHalfUp_N4_N2 :: Dict (K.Div 'K.RoundHalfUp (N 4) (N 2) ~ P 2)
_test_Div_RoundHalfUp_N4_N2 =  Dict
_test_Rem_RoundHalfUp_N4_N2 :: Dict (K.Rem 'K.RoundHalfUp (N 4) (N 2) ~ P 0)
_test_Rem_RoundHalfUp_N4_N2 =  Dict
_test_Div_RoundHalfUp_N4_N3 :: Dict (K.Div 'K.RoundHalfUp (N 4) (N 3) ~ P 1)
_test_Div_RoundHalfUp_N4_N3 =  Dict
_test_Rem_RoundHalfUp_N4_N3 :: Dict (K.Rem 'K.RoundHalfUp (N 4) (N 3) ~ N 1)
_test_Rem_RoundHalfUp_N4_N3 =  Dict
_test_Div_RoundHalfUp_N4_N4 :: Dict (K.Div 'K.RoundHalfUp (N 4) (N 4) ~ P 1)
_test_Div_RoundHalfUp_N4_N4 =  Dict
_test_Rem_RoundHalfUp_N4_N4 :: Dict (K.Rem 'K.RoundHalfUp (N 4) (N 4) ~ P 0)
_test_Rem_RoundHalfUp_N4_N4 =  Dict
_test_Div_RoundHalfUp_N4_P1 :: Dict (K.Div 'K.RoundHalfUp (N 4) (P 1) ~ N 4)
_test_Div_RoundHalfUp_N4_P1 =  Dict
_test_Rem_RoundHalfUp_N4_P1 :: Dict (K.Rem 'K.RoundHalfUp (N 4) (P 1) ~ P 0)
_test_Rem_RoundHalfUp_N4_P1 =  Dict
_test_Div_RoundHalfUp_N4_P2 :: Dict (K.Div 'K.RoundHalfUp (N 4) (P 2) ~ N 2)
_test_Div_RoundHalfUp_N4_P2 =  Dict
_test_Rem_RoundHalfUp_N4_P2 :: Dict (K.Rem 'K.RoundHalfUp (N 4) (P 2) ~ P 0)
_test_Rem_RoundHalfUp_N4_P2 =  Dict
_test_Div_RoundHalfUp_N4_P3 :: Dict (K.Div 'K.RoundHalfUp (N 4) (P 3) ~ N 1)
_test_Div_RoundHalfUp_N4_P3 =  Dict
_test_Rem_RoundHalfUp_N4_P3 :: Dict (K.Rem 'K.RoundHalfUp (N 4) (P 3) ~ N 1)
_test_Rem_RoundHalfUp_N4_P3 =  Dict
_test_Div_RoundHalfUp_N4_P4 :: Dict (K.Div 'K.RoundHalfUp (N 4) (P 4) ~ N 1)
_test_Div_RoundHalfUp_N4_P4 =  Dict
_test_Rem_RoundHalfUp_N4_P4 :: Dict (K.Rem 'K.RoundHalfUp (N 4) (P 4) ~ P 0)
_test_Rem_RoundHalfUp_N4_P4 =  Dict
_test_Div_RoundHalfUp_P0_N1 :: Dict (K.Div 'K.RoundHalfUp (P 0) (N 1) ~ P 0)
_test_Div_RoundHalfUp_P0_N1 =  Dict
_test_Rem_RoundHalfUp_P0_N1 :: Dict (K.Rem 'K.RoundHalfUp (P 0) (N 1) ~ P 0)
_test_Rem_RoundHalfUp_P0_N1 =  Dict
_test_Div_RoundHalfUp_P0_N2 :: Dict (K.Div 'K.RoundHalfUp (P 0) (N 2) ~ P 0)
_test_Div_RoundHalfUp_P0_N2 =  Dict
_test_Rem_RoundHalfUp_P0_N2 :: Dict (K.Rem 'K.RoundHalfUp (P 0) (N 2) ~ P 0)
_test_Rem_RoundHalfUp_P0_N2 =  Dict
_test_Div_RoundHalfUp_P0_N3 :: Dict (K.Div 'K.RoundHalfUp (P 0) (N 3) ~ P 0)
_test_Div_RoundHalfUp_P0_N3 =  Dict
_test_Rem_RoundHalfUp_P0_N3 :: Dict (K.Rem 'K.RoundHalfUp (P 0) (N 3) ~ P 0)
_test_Rem_RoundHalfUp_P0_N3 =  Dict
_test_Div_RoundHalfUp_P0_N4 :: Dict (K.Div 'K.RoundHalfUp (P 0) (N 4) ~ P 0)
_test_Div_RoundHalfUp_P0_N4 =  Dict
_test_Rem_RoundHalfUp_P0_N4 :: Dict (K.Rem 'K.RoundHalfUp (P 0) (N 4) ~ P 0)
_test_Rem_RoundHalfUp_P0_N4 =  Dict
_test_Div_RoundHalfUp_P0_P1 :: Dict (K.Div 'K.RoundHalfUp (P 0) (P 1) ~ P 0)
_test_Div_RoundHalfUp_P0_P1 =  Dict
_test_Rem_RoundHalfUp_P0_P1 :: Dict (K.Rem 'K.RoundHalfUp (P 0) (P 1) ~ P 0)
_test_Rem_RoundHalfUp_P0_P1 =  Dict
_test_Div_RoundHalfUp_P0_P2 :: Dict (K.Div 'K.RoundHalfUp (P 0) (P 2) ~ P 0)
_test_Div_RoundHalfUp_P0_P2 =  Dict
_test_Rem_RoundHalfUp_P0_P2 :: Dict (K.Rem 'K.RoundHalfUp (P 0) (P 2) ~ P 0)
_test_Rem_RoundHalfUp_P0_P2 =  Dict
_test_Div_RoundHalfUp_P0_P3 :: Dict (K.Div 'K.RoundHalfUp (P 0) (P 3) ~ P 0)
_test_Div_RoundHalfUp_P0_P3 =  Dict
_test_Rem_RoundHalfUp_P0_P3 :: Dict (K.Rem 'K.RoundHalfUp (P 0) (P 3) ~ P 0)
_test_Rem_RoundHalfUp_P0_P3 =  Dict
_test_Div_RoundHalfUp_P0_P4 :: Dict (K.Div 'K.RoundHalfUp (P 0) (P 4) ~ P 0)
_test_Div_RoundHalfUp_P0_P4 =  Dict
_test_Rem_RoundHalfUp_P0_P4 :: Dict (K.Rem 'K.RoundHalfUp (P 0) (P 4) ~ P 0)
_test_Rem_RoundHalfUp_P0_P4 =  Dict
_test_Div_RoundHalfUp_P1_N1 :: Dict (K.Div 'K.RoundHalfUp (P 1) (N 1) ~ N 1)
_test_Div_RoundHalfUp_P1_N1 =  Dict
_test_Rem_RoundHalfUp_P1_N1 :: Dict (K.Rem 'K.RoundHalfUp (P 1) (N 1) ~ P 0)
_test_Rem_RoundHalfUp_P1_N1 =  Dict
_test_Div_RoundHalfUp_P1_N2 :: Dict (K.Div 'K.RoundHalfUp (P 1) (N 2) ~ P 0)
_test_Div_RoundHalfUp_P1_N2 =  Dict
_test_Rem_RoundHalfUp_P1_N2 :: Dict (K.Rem 'K.RoundHalfUp (P 1) (N 2) ~ P 1)
_test_Rem_RoundHalfUp_P1_N2 =  Dict
_test_Div_RoundHalfUp_P1_N3 :: Dict (K.Div 'K.RoundHalfUp (P 1) (N 3) ~ P 0)
_test_Div_RoundHalfUp_P1_N3 =  Dict
_test_Rem_RoundHalfUp_P1_N3 :: Dict (K.Rem 'K.RoundHalfUp (P 1) (N 3) ~ P 1)
_test_Rem_RoundHalfUp_P1_N3 =  Dict
_test_Div_RoundHalfUp_P1_N4 :: Dict (K.Div 'K.RoundHalfUp (P 1) (N 4) ~ P 0)
_test_Div_RoundHalfUp_P1_N4 =  Dict
_test_Rem_RoundHalfUp_P1_N4 :: Dict (K.Rem 'K.RoundHalfUp (P 1) (N 4) ~ P 1)
_test_Rem_RoundHalfUp_P1_N4 =  Dict
_test_Div_RoundHalfUp_P1_P1 :: Dict (K.Div 'K.RoundHalfUp (P 1) (P 1) ~ P 1)
_test_Div_RoundHalfUp_P1_P1 =  Dict
_test_Rem_RoundHalfUp_P1_P1 :: Dict (K.Rem 'K.RoundHalfUp (P 1) (P 1) ~ P 0)
_test_Rem_RoundHalfUp_P1_P1 =  Dict
_test_Div_RoundHalfUp_P1_P2 :: Dict (K.Div 'K.RoundHalfUp (P 1) (P 2) ~ P 1)
_test_Div_RoundHalfUp_P1_P2 =  Dict
_test_Rem_RoundHalfUp_P1_P2 :: Dict (K.Rem 'K.RoundHalfUp (P 1) (P 2) ~ N 1)
_test_Rem_RoundHalfUp_P1_P2 =  Dict
_test_Div_RoundHalfUp_P1_P3 :: Dict (K.Div 'K.RoundHalfUp (P 1) (P 3) ~ P 0)
_test_Div_RoundHalfUp_P1_P3 =  Dict
_test_Rem_RoundHalfUp_P1_P3 :: Dict (K.Rem 'K.RoundHalfUp (P 1) (P 3) ~ P 1)
_test_Rem_RoundHalfUp_P1_P3 =  Dict
_test_Div_RoundHalfUp_P1_P4 :: Dict (K.Div 'K.RoundHalfUp (P 1) (P 4) ~ P 0)
_test_Div_RoundHalfUp_P1_P4 =  Dict
_test_Rem_RoundHalfUp_P1_P4 :: Dict (K.Rem 'K.RoundHalfUp (P 1) (P 4) ~ P 1)
_test_Rem_RoundHalfUp_P1_P4 =  Dict
_test_Div_RoundHalfUp_P2_N1 :: Dict (K.Div 'K.RoundHalfUp (P 2) (N 1) ~ N 2)
_test_Div_RoundHalfUp_P2_N1 =  Dict
_test_Rem_RoundHalfUp_P2_N1 :: Dict (K.Rem 'K.RoundHalfUp (P 2) (N 1) ~ P 0)
_test_Rem_RoundHalfUp_P2_N1 =  Dict
_test_Div_RoundHalfUp_P2_N2 :: Dict (K.Div 'K.RoundHalfUp (P 2) (N 2) ~ N 1)
_test_Div_RoundHalfUp_P2_N2 =  Dict
_test_Rem_RoundHalfUp_P2_N2 :: Dict (K.Rem 'K.RoundHalfUp (P 2) (N 2) ~ P 0)
_test_Rem_RoundHalfUp_P2_N2 =  Dict
_test_Div_RoundHalfUp_P2_N3 :: Dict (K.Div 'K.RoundHalfUp (P 2) (N 3) ~ N 1)
_test_Div_RoundHalfUp_P2_N3 =  Dict
_test_Rem_RoundHalfUp_P2_N3 :: Dict (K.Rem 'K.RoundHalfUp (P 2) (N 3) ~ N 1)
_test_Rem_RoundHalfUp_P2_N3 =  Dict
_test_Div_RoundHalfUp_P2_N4 :: Dict (K.Div 'K.RoundHalfUp (P 2) (N 4) ~ P 0)
_test_Div_RoundHalfUp_P2_N4 =  Dict
_test_Rem_RoundHalfUp_P2_N4 :: Dict (K.Rem 'K.RoundHalfUp (P 2) (N 4) ~ P 2)
_test_Rem_RoundHalfUp_P2_N4 =  Dict
_test_Div_RoundHalfUp_P2_P1 :: Dict (K.Div 'K.RoundHalfUp (P 2) (P 1) ~ P 2)
_test_Div_RoundHalfUp_P2_P1 =  Dict
_test_Rem_RoundHalfUp_P2_P1 :: Dict (K.Rem 'K.RoundHalfUp (P 2) (P 1) ~ P 0)
_test_Rem_RoundHalfUp_P2_P1 =  Dict
_test_Div_RoundHalfUp_P2_P2 :: Dict (K.Div 'K.RoundHalfUp (P 2) (P 2) ~ P 1)
_test_Div_RoundHalfUp_P2_P2 =  Dict
_test_Rem_RoundHalfUp_P2_P2 :: Dict (K.Rem 'K.RoundHalfUp (P 2) (P 2) ~ P 0)
_test_Rem_RoundHalfUp_P2_P2 =  Dict
_test_Div_RoundHalfUp_P2_P3 :: Dict (K.Div 'K.RoundHalfUp (P 2) (P 3) ~ P 1)
_test_Div_RoundHalfUp_P2_P3 =  Dict
_test_Rem_RoundHalfUp_P2_P3 :: Dict (K.Rem 'K.RoundHalfUp (P 2) (P 3) ~ N 1)
_test_Rem_RoundHalfUp_P2_P3 =  Dict
_test_Div_RoundHalfUp_P2_P4 :: Dict (K.Div 'K.RoundHalfUp (P 2) (P 4) ~ P 1)
_test_Div_RoundHalfUp_P2_P4 =  Dict
_test_Rem_RoundHalfUp_P2_P4 :: Dict (K.Rem 'K.RoundHalfUp (P 2) (P 4) ~ N 2)
_test_Rem_RoundHalfUp_P2_P4 =  Dict
_test_Div_RoundHalfUp_P3_N1 :: Dict (K.Div 'K.RoundHalfUp (P 3) (N 1) ~ N 3)
_test_Div_RoundHalfUp_P3_N1 =  Dict
_test_Rem_RoundHalfUp_P3_N1 :: Dict (K.Rem 'K.RoundHalfUp (P 3) (N 1) ~ P 0)
_test_Rem_RoundHalfUp_P3_N1 =  Dict
_test_Div_RoundHalfUp_P3_N2 :: Dict (K.Div 'K.RoundHalfUp (P 3) (N 2) ~ N 1)
_test_Div_RoundHalfUp_P3_N2 =  Dict
_test_Rem_RoundHalfUp_P3_N2 :: Dict (K.Rem 'K.RoundHalfUp (P 3) (N 2) ~ P 1)
_test_Rem_RoundHalfUp_P3_N2 =  Dict
_test_Div_RoundHalfUp_P3_N3 :: Dict (K.Div 'K.RoundHalfUp (P 3) (N 3) ~ N 1)
_test_Div_RoundHalfUp_P3_N3 =  Dict
_test_Rem_RoundHalfUp_P3_N3 :: Dict (K.Rem 'K.RoundHalfUp (P 3) (N 3) ~ P 0)
_test_Rem_RoundHalfUp_P3_N3 =  Dict
_test_Div_RoundHalfUp_P3_N4 :: Dict (K.Div 'K.RoundHalfUp (P 3) (N 4) ~ N 1)
_test_Div_RoundHalfUp_P3_N4 =  Dict
_test_Rem_RoundHalfUp_P3_N4 :: Dict (K.Rem 'K.RoundHalfUp (P 3) (N 4) ~ N 1)
_test_Rem_RoundHalfUp_P3_N4 =  Dict
_test_Div_RoundHalfUp_P3_P1 :: Dict (K.Div 'K.RoundHalfUp (P 3) (P 1) ~ P 3)
_test_Div_RoundHalfUp_P3_P1 =  Dict
_test_Rem_RoundHalfUp_P3_P1 :: Dict (K.Rem 'K.RoundHalfUp (P 3) (P 1) ~ P 0)
_test_Rem_RoundHalfUp_P3_P1 =  Dict
_test_Div_RoundHalfUp_P3_P2 :: Dict (K.Div 'K.RoundHalfUp (P 3) (P 2) ~ P 2)
_test_Div_RoundHalfUp_P3_P2 =  Dict
_test_Rem_RoundHalfUp_P3_P2 :: Dict (K.Rem 'K.RoundHalfUp (P 3) (P 2) ~ N 1)
_test_Rem_RoundHalfUp_P3_P2 =  Dict
_test_Div_RoundHalfUp_P3_P3 :: Dict (K.Div 'K.RoundHalfUp (P 3) (P 3) ~ P 1)
_test_Div_RoundHalfUp_P3_P3 =  Dict
_test_Rem_RoundHalfUp_P3_P3 :: Dict (K.Rem 'K.RoundHalfUp (P 3) (P 3) ~ P 0)
_test_Rem_RoundHalfUp_P3_P3 =  Dict
_test_Div_RoundHalfUp_P3_P4 :: Dict (K.Div 'K.RoundHalfUp (P 3) (P 4) ~ P 1)
_test_Div_RoundHalfUp_P3_P4 =  Dict
_test_Rem_RoundHalfUp_P3_P4 :: Dict (K.Rem 'K.RoundHalfUp (P 3) (P 4) ~ N 1)
_test_Rem_RoundHalfUp_P3_P4 =  Dict
_test_Div_RoundHalfUp_P4_N1 :: Dict (K.Div 'K.RoundHalfUp (P 4) (N 1) ~ N 4)
_test_Div_RoundHalfUp_P4_N1 =  Dict
_test_Rem_RoundHalfUp_P4_N1 :: Dict (K.Rem 'K.RoundHalfUp (P 4) (N 1) ~ P 0)
_test_Rem_RoundHalfUp_P4_N1 =  Dict
_test_Div_RoundHalfUp_P4_N2 :: Dict (K.Div 'K.RoundHalfUp (P 4) (N 2) ~ N 2)
_test_Div_RoundHalfUp_P4_N2 =  Dict
_test_Rem_RoundHalfUp_P4_N2 :: Dict (K.Rem 'K.RoundHalfUp (P 4) (N 2) ~ P 0)
_test_Rem_RoundHalfUp_P4_N2 =  Dict
_test_Div_RoundHalfUp_P4_N3 :: Dict (K.Div 'K.RoundHalfUp (P 4) (N 3) ~ N 1)
_test_Div_RoundHalfUp_P4_N3 =  Dict
_test_Rem_RoundHalfUp_P4_N3 :: Dict (K.Rem 'K.RoundHalfUp (P 4) (N 3) ~ P 1)
_test_Rem_RoundHalfUp_P4_N3 =  Dict
_test_Div_RoundHalfUp_P4_N4 :: Dict (K.Div 'K.RoundHalfUp (P 4) (N 4) ~ N 1)
_test_Div_RoundHalfUp_P4_N4 =  Dict
_test_Rem_RoundHalfUp_P4_N4 :: Dict (K.Rem 'K.RoundHalfUp (P 4) (N 4) ~ P 0)
_test_Rem_RoundHalfUp_P4_N4 =  Dict
_test_Div_RoundHalfUp_P4_P1 :: Dict (K.Div 'K.RoundHalfUp (P 4) (P 1) ~ P 4)
_test_Div_RoundHalfUp_P4_P1 =  Dict
_test_Rem_RoundHalfUp_P4_P1 :: Dict (K.Rem 'K.RoundHalfUp (P 4) (P 1) ~ P 0)
_test_Rem_RoundHalfUp_P4_P1 =  Dict
_test_Div_RoundHalfUp_P4_P2 :: Dict (K.Div 'K.RoundHalfUp (P 4) (P 2) ~ P 2)
_test_Div_RoundHalfUp_P4_P2 =  Dict
_test_Rem_RoundHalfUp_P4_P2 :: Dict (K.Rem 'K.RoundHalfUp (P 4) (P 2) ~ P 0)
_test_Rem_RoundHalfUp_P4_P2 =  Dict
_test_Div_RoundHalfUp_P4_P3 :: Dict (K.Div 'K.RoundHalfUp (P 4) (P 3) ~ P 1)
_test_Div_RoundHalfUp_P4_P3 =  Dict
_test_Rem_RoundHalfUp_P4_P3 :: Dict (K.Rem 'K.RoundHalfUp (P 4) (P 3) ~ P 1)
_test_Rem_RoundHalfUp_P4_P3 =  Dict
_test_Div_RoundHalfUp_P4_P4 :: Dict (K.Div 'K.RoundHalfUp (P 4) (P 4) ~ P 1)
_test_Div_RoundHalfUp_P4_P4 =  Dict
_test_Rem_RoundHalfUp_P4_P4 :: Dict (K.Rem 'K.RoundHalfUp (P 4) (P 4) ~ P 0)
_test_Rem_RoundHalfUp_P4_P4 =  Dict
_test_Div_RoundHalfZero_N1_N1 :: Dict (K.Div 'K.RoundHalfZero (N 1) (N 1) ~ P 1)
_test_Div_RoundHalfZero_N1_N1 =  Dict
_test_Rem_RoundHalfZero_N1_N1 :: Dict (K.Rem 'K.RoundHalfZero (N 1) (N 1) ~ P 0)
_test_Rem_RoundHalfZero_N1_N1 =  Dict
_test_Div_RoundHalfZero_N1_N2 :: Dict (K.Div 'K.RoundHalfZero (N 1) (N 2) ~ P 0)
_test_Div_RoundHalfZero_N1_N2 =  Dict
_test_Rem_RoundHalfZero_N1_N2 :: Dict (K.Rem 'K.RoundHalfZero (N 1) (N 2) ~ N 1)
_test_Rem_RoundHalfZero_N1_N2 =  Dict
_test_Div_RoundHalfZero_N1_N3 :: Dict (K.Div 'K.RoundHalfZero (N 1) (N 3) ~ P 0)
_test_Div_RoundHalfZero_N1_N3 =  Dict
_test_Rem_RoundHalfZero_N1_N3 :: Dict (K.Rem 'K.RoundHalfZero (N 1) (N 3) ~ N 1)
_test_Rem_RoundHalfZero_N1_N3 =  Dict
_test_Div_RoundHalfZero_N1_N4 :: Dict (K.Div 'K.RoundHalfZero (N 1) (N 4) ~ P 0)
_test_Div_RoundHalfZero_N1_N4 =  Dict
_test_Rem_RoundHalfZero_N1_N4 :: Dict (K.Rem 'K.RoundHalfZero (N 1) (N 4) ~ N 1)
_test_Rem_RoundHalfZero_N1_N4 =  Dict
_test_Div_RoundHalfZero_N1_P1 :: Dict (K.Div 'K.RoundHalfZero (N 1) (P 1) ~ N 1)
_test_Div_RoundHalfZero_N1_P1 =  Dict
_test_Rem_RoundHalfZero_N1_P1 :: Dict (K.Rem 'K.RoundHalfZero (N 1) (P 1) ~ P 0)
_test_Rem_RoundHalfZero_N1_P1 =  Dict
_test_Div_RoundHalfZero_N1_P2 :: Dict (K.Div 'K.RoundHalfZero (N 1) (P 2) ~ P 0)
_test_Div_RoundHalfZero_N1_P2 =  Dict
_test_Rem_RoundHalfZero_N1_P2 :: Dict (K.Rem 'K.RoundHalfZero (N 1) (P 2) ~ N 1)
_test_Rem_RoundHalfZero_N1_P2 =  Dict
_test_Div_RoundHalfZero_N1_P3 :: Dict (K.Div 'K.RoundHalfZero (N 1) (P 3) ~ P 0)
_test_Div_RoundHalfZero_N1_P3 =  Dict
_test_Rem_RoundHalfZero_N1_P3 :: Dict (K.Rem 'K.RoundHalfZero (N 1) (P 3) ~ N 1)
_test_Rem_RoundHalfZero_N1_P3 =  Dict
_test_Div_RoundHalfZero_N1_P4 :: Dict (K.Div 'K.RoundHalfZero (N 1) (P 4) ~ P 0)
_test_Div_RoundHalfZero_N1_P4 =  Dict
_test_Rem_RoundHalfZero_N1_P4 :: Dict (K.Rem 'K.RoundHalfZero (N 1) (P 4) ~ N 1)
_test_Rem_RoundHalfZero_N1_P4 =  Dict
_test_Div_RoundHalfZero_N2_N1 :: Dict (K.Div 'K.RoundHalfZero (N 2) (N 1) ~ P 2)
_test_Div_RoundHalfZero_N2_N1 =  Dict
_test_Rem_RoundHalfZero_N2_N1 :: Dict (K.Rem 'K.RoundHalfZero (N 2) (N 1) ~ P 0)
_test_Rem_RoundHalfZero_N2_N1 =  Dict
_test_Div_RoundHalfZero_N2_N2 :: Dict (K.Div 'K.RoundHalfZero (N 2) (N 2) ~ P 1)
_test_Div_RoundHalfZero_N2_N2 =  Dict
_test_Rem_RoundHalfZero_N2_N2 :: Dict (K.Rem 'K.RoundHalfZero (N 2) (N 2) ~ P 0)
_test_Rem_RoundHalfZero_N2_N2 =  Dict
_test_Div_RoundHalfZero_N2_N3 :: Dict (K.Div 'K.RoundHalfZero (N 2) (N 3) ~ P 1)
_test_Div_RoundHalfZero_N2_N3 =  Dict
_test_Rem_RoundHalfZero_N2_N3 :: Dict (K.Rem 'K.RoundHalfZero (N 2) (N 3) ~ P 1)
_test_Rem_RoundHalfZero_N2_N3 =  Dict
_test_Div_RoundHalfZero_N2_N4 :: Dict (K.Div 'K.RoundHalfZero (N 2) (N 4) ~ P 0)
_test_Div_RoundHalfZero_N2_N4 =  Dict
_test_Rem_RoundHalfZero_N2_N4 :: Dict (K.Rem 'K.RoundHalfZero (N 2) (N 4) ~ N 2)
_test_Rem_RoundHalfZero_N2_N4 =  Dict
_test_Div_RoundHalfZero_N2_P1 :: Dict (K.Div 'K.RoundHalfZero (N 2) (P 1) ~ N 2)
_test_Div_RoundHalfZero_N2_P1 =  Dict
_test_Rem_RoundHalfZero_N2_P1 :: Dict (K.Rem 'K.RoundHalfZero (N 2) (P 1) ~ P 0)
_test_Rem_RoundHalfZero_N2_P1 =  Dict
_test_Div_RoundHalfZero_N2_P2 :: Dict (K.Div 'K.RoundHalfZero (N 2) (P 2) ~ N 1)
_test_Div_RoundHalfZero_N2_P2 =  Dict
_test_Rem_RoundHalfZero_N2_P2 :: Dict (K.Rem 'K.RoundHalfZero (N 2) (P 2) ~ P 0)
_test_Rem_RoundHalfZero_N2_P2 =  Dict
_test_Div_RoundHalfZero_N2_P3 :: Dict (K.Div 'K.RoundHalfZero (N 2) (P 3) ~ N 1)
_test_Div_RoundHalfZero_N2_P3 =  Dict
_test_Rem_RoundHalfZero_N2_P3 :: Dict (K.Rem 'K.RoundHalfZero (N 2) (P 3) ~ P 1)
_test_Rem_RoundHalfZero_N2_P3 =  Dict
_test_Div_RoundHalfZero_N2_P4 :: Dict (K.Div 'K.RoundHalfZero (N 2) (P 4) ~ P 0)
_test_Div_RoundHalfZero_N2_P4 =  Dict
_test_Rem_RoundHalfZero_N2_P4 :: Dict (K.Rem 'K.RoundHalfZero (N 2) (P 4) ~ N 2)
_test_Rem_RoundHalfZero_N2_P4 =  Dict
_test_Div_RoundHalfZero_N3_N1 :: Dict (K.Div 'K.RoundHalfZero (N 3) (N 1) ~ P 3)
_test_Div_RoundHalfZero_N3_N1 =  Dict
_test_Rem_RoundHalfZero_N3_N1 :: Dict (K.Rem 'K.RoundHalfZero (N 3) (N 1) ~ P 0)
_test_Rem_RoundHalfZero_N3_N1 =  Dict
_test_Div_RoundHalfZero_N3_N2 :: Dict (K.Div 'K.RoundHalfZero (N 3) (N 2) ~ P 1)
_test_Div_RoundHalfZero_N3_N2 =  Dict
_test_Rem_RoundHalfZero_N3_N2 :: Dict (K.Rem 'K.RoundHalfZero (N 3) (N 2) ~ N 1)
_test_Rem_RoundHalfZero_N3_N2 =  Dict
_test_Div_RoundHalfZero_N3_N3 :: Dict (K.Div 'K.RoundHalfZero (N 3) (N 3) ~ P 1)
_test_Div_RoundHalfZero_N3_N3 =  Dict
_test_Rem_RoundHalfZero_N3_N3 :: Dict (K.Rem 'K.RoundHalfZero (N 3) (N 3) ~ P 0)
_test_Rem_RoundHalfZero_N3_N3 =  Dict
_test_Div_RoundHalfZero_N3_N4 :: Dict (K.Div 'K.RoundHalfZero (N 3) (N 4) ~ P 1)
_test_Div_RoundHalfZero_N3_N4 =  Dict
_test_Rem_RoundHalfZero_N3_N4 :: Dict (K.Rem 'K.RoundHalfZero (N 3) (N 4) ~ P 1)
_test_Rem_RoundHalfZero_N3_N4 =  Dict
_test_Div_RoundHalfZero_N3_P1 :: Dict (K.Div 'K.RoundHalfZero (N 3) (P 1) ~ N 3)
_test_Div_RoundHalfZero_N3_P1 =  Dict
_test_Rem_RoundHalfZero_N3_P1 :: Dict (K.Rem 'K.RoundHalfZero (N 3) (P 1) ~ P 0)
_test_Rem_RoundHalfZero_N3_P1 =  Dict
_test_Div_RoundHalfZero_N3_P2 :: Dict (K.Div 'K.RoundHalfZero (N 3) (P 2) ~ N 1)
_test_Div_RoundHalfZero_N3_P2 =  Dict
_test_Rem_RoundHalfZero_N3_P2 :: Dict (K.Rem 'K.RoundHalfZero (N 3) (P 2) ~ N 1)
_test_Rem_RoundHalfZero_N3_P2 =  Dict
_test_Div_RoundHalfZero_N3_P3 :: Dict (K.Div 'K.RoundHalfZero (N 3) (P 3) ~ N 1)
_test_Div_RoundHalfZero_N3_P3 =  Dict
_test_Rem_RoundHalfZero_N3_P3 :: Dict (K.Rem 'K.RoundHalfZero (N 3) (P 3) ~ P 0)
_test_Rem_RoundHalfZero_N3_P3 =  Dict
_test_Div_RoundHalfZero_N3_P4 :: Dict (K.Div 'K.RoundHalfZero (N 3) (P 4) ~ N 1)
_test_Div_RoundHalfZero_N3_P4 =  Dict
_test_Rem_RoundHalfZero_N3_P4 :: Dict (K.Rem 'K.RoundHalfZero (N 3) (P 4) ~ P 1)
_test_Rem_RoundHalfZero_N3_P4 =  Dict
_test_Div_RoundHalfZero_N4_N1 :: Dict (K.Div 'K.RoundHalfZero (N 4) (N 1) ~ P 4)
_test_Div_RoundHalfZero_N4_N1 =  Dict
_test_Rem_RoundHalfZero_N4_N1 :: Dict (K.Rem 'K.RoundHalfZero (N 4) (N 1) ~ P 0)
_test_Rem_RoundHalfZero_N4_N1 =  Dict
_test_Div_RoundHalfZero_N4_N2 :: Dict (K.Div 'K.RoundHalfZero (N 4) (N 2) ~ P 2)
_test_Div_RoundHalfZero_N4_N2 =  Dict
_test_Rem_RoundHalfZero_N4_N2 :: Dict (K.Rem 'K.RoundHalfZero (N 4) (N 2) ~ P 0)
_test_Rem_RoundHalfZero_N4_N2 =  Dict
_test_Div_RoundHalfZero_N4_N3 :: Dict (K.Div 'K.RoundHalfZero (N 4) (N 3) ~ P 1)
_test_Div_RoundHalfZero_N4_N3 =  Dict
_test_Rem_RoundHalfZero_N4_N3 :: Dict (K.Rem 'K.RoundHalfZero (N 4) (N 3) ~ N 1)
_test_Rem_RoundHalfZero_N4_N3 =  Dict
_test_Div_RoundHalfZero_N4_N4 :: Dict (K.Div 'K.RoundHalfZero (N 4) (N 4) ~ P 1)
_test_Div_RoundHalfZero_N4_N4 =  Dict
_test_Rem_RoundHalfZero_N4_N4 :: Dict (K.Rem 'K.RoundHalfZero (N 4) (N 4) ~ P 0)
_test_Rem_RoundHalfZero_N4_N4 =  Dict
_test_Div_RoundHalfZero_N4_P1 :: Dict (K.Div 'K.RoundHalfZero (N 4) (P 1) ~ N 4)
_test_Div_RoundHalfZero_N4_P1 =  Dict
_test_Rem_RoundHalfZero_N4_P1 :: Dict (K.Rem 'K.RoundHalfZero (N 4) (P 1) ~ P 0)
_test_Rem_RoundHalfZero_N4_P1 =  Dict
_test_Div_RoundHalfZero_N4_P2 :: Dict (K.Div 'K.RoundHalfZero (N 4) (P 2) ~ N 2)
_test_Div_RoundHalfZero_N4_P2 =  Dict
_test_Rem_RoundHalfZero_N4_P2 :: Dict (K.Rem 'K.RoundHalfZero (N 4) (P 2) ~ P 0)
_test_Rem_RoundHalfZero_N4_P2 =  Dict
_test_Div_RoundHalfZero_N4_P3 :: Dict (K.Div 'K.RoundHalfZero (N 4) (P 3) ~ N 1)
_test_Div_RoundHalfZero_N4_P3 =  Dict
_test_Rem_RoundHalfZero_N4_P3 :: Dict (K.Rem 'K.RoundHalfZero (N 4) (P 3) ~ N 1)
_test_Rem_RoundHalfZero_N4_P3 =  Dict
_test_Div_RoundHalfZero_N4_P4 :: Dict (K.Div 'K.RoundHalfZero (N 4) (P 4) ~ N 1)
_test_Div_RoundHalfZero_N4_P4 =  Dict
_test_Rem_RoundHalfZero_N4_P4 :: Dict (K.Rem 'K.RoundHalfZero (N 4) (P 4) ~ P 0)
_test_Rem_RoundHalfZero_N4_P4 =  Dict
_test_Div_RoundHalfZero_P0_N1 :: Dict (K.Div 'K.RoundHalfZero (P 0) (N 1) ~ P 0)
_test_Div_RoundHalfZero_P0_N1 =  Dict
_test_Rem_RoundHalfZero_P0_N1 :: Dict (K.Rem 'K.RoundHalfZero (P 0) (N 1) ~ P 0)
_test_Rem_RoundHalfZero_P0_N1 =  Dict
_test_Div_RoundHalfZero_P0_N2 :: Dict (K.Div 'K.RoundHalfZero (P 0) (N 2) ~ P 0)
_test_Div_RoundHalfZero_P0_N2 =  Dict
_test_Rem_RoundHalfZero_P0_N2 :: Dict (K.Rem 'K.RoundHalfZero (P 0) (N 2) ~ P 0)
_test_Rem_RoundHalfZero_P0_N2 =  Dict
_test_Div_RoundHalfZero_P0_N3 :: Dict (K.Div 'K.RoundHalfZero (P 0) (N 3) ~ P 0)
_test_Div_RoundHalfZero_P0_N3 =  Dict
_test_Rem_RoundHalfZero_P0_N3 :: Dict (K.Rem 'K.RoundHalfZero (P 0) (N 3) ~ P 0)
_test_Rem_RoundHalfZero_P0_N3 =  Dict
_test_Div_RoundHalfZero_P0_N4 :: Dict (K.Div 'K.RoundHalfZero (P 0) (N 4) ~ P 0)
_test_Div_RoundHalfZero_P0_N4 =  Dict
_test_Rem_RoundHalfZero_P0_N4 :: Dict (K.Rem 'K.RoundHalfZero (P 0) (N 4) ~ P 0)
_test_Rem_RoundHalfZero_P0_N4 =  Dict
_test_Div_RoundHalfZero_P0_P1 :: Dict (K.Div 'K.RoundHalfZero (P 0) (P 1) ~ P 0)
_test_Div_RoundHalfZero_P0_P1 =  Dict
_test_Rem_RoundHalfZero_P0_P1 :: Dict (K.Rem 'K.RoundHalfZero (P 0) (P 1) ~ P 0)
_test_Rem_RoundHalfZero_P0_P1 =  Dict
_test_Div_RoundHalfZero_P0_P2 :: Dict (K.Div 'K.RoundHalfZero (P 0) (P 2) ~ P 0)
_test_Div_RoundHalfZero_P0_P2 =  Dict
_test_Rem_RoundHalfZero_P0_P2 :: Dict (K.Rem 'K.RoundHalfZero (P 0) (P 2) ~ P 0)
_test_Rem_RoundHalfZero_P0_P2 =  Dict
_test_Div_RoundHalfZero_P0_P3 :: Dict (K.Div 'K.RoundHalfZero (P 0) (P 3) ~ P 0)
_test_Div_RoundHalfZero_P0_P3 =  Dict
_test_Rem_RoundHalfZero_P0_P3 :: Dict (K.Rem 'K.RoundHalfZero (P 0) (P 3) ~ P 0)
_test_Rem_RoundHalfZero_P0_P3 =  Dict
_test_Div_RoundHalfZero_P0_P4 :: Dict (K.Div 'K.RoundHalfZero (P 0) (P 4) ~ P 0)
_test_Div_RoundHalfZero_P0_P4 =  Dict
_test_Rem_RoundHalfZero_P0_P4 :: Dict (K.Rem 'K.RoundHalfZero (P 0) (P 4) ~ P 0)
_test_Rem_RoundHalfZero_P0_P4 =  Dict
_test_Div_RoundHalfZero_P1_N1 :: Dict (K.Div 'K.RoundHalfZero (P 1) (N 1) ~ N 1)
_test_Div_RoundHalfZero_P1_N1 =  Dict
_test_Rem_RoundHalfZero_P1_N1 :: Dict (K.Rem 'K.RoundHalfZero (P 1) (N 1) ~ P 0)
_test_Rem_RoundHalfZero_P1_N1 =  Dict
_test_Div_RoundHalfZero_P1_N2 :: Dict (K.Div 'K.RoundHalfZero (P 1) (N 2) ~ P 0)
_test_Div_RoundHalfZero_P1_N2 =  Dict
_test_Rem_RoundHalfZero_P1_N2 :: Dict (K.Rem 'K.RoundHalfZero (P 1) (N 2) ~ P 1)
_test_Rem_RoundHalfZero_P1_N2 =  Dict
_test_Div_RoundHalfZero_P1_N3 :: Dict (K.Div 'K.RoundHalfZero (P 1) (N 3) ~ P 0)
_test_Div_RoundHalfZero_P1_N3 =  Dict
_test_Rem_RoundHalfZero_P1_N3 :: Dict (K.Rem 'K.RoundHalfZero (P 1) (N 3) ~ P 1)
_test_Rem_RoundHalfZero_P1_N3 =  Dict
_test_Div_RoundHalfZero_P1_N4 :: Dict (K.Div 'K.RoundHalfZero (P 1) (N 4) ~ P 0)
_test_Div_RoundHalfZero_P1_N4 =  Dict
_test_Rem_RoundHalfZero_P1_N4 :: Dict (K.Rem 'K.RoundHalfZero (P 1) (N 4) ~ P 1)
_test_Rem_RoundHalfZero_P1_N4 =  Dict
_test_Div_RoundHalfZero_P1_P1 :: Dict (K.Div 'K.RoundHalfZero (P 1) (P 1) ~ P 1)
_test_Div_RoundHalfZero_P1_P1 =  Dict
_test_Rem_RoundHalfZero_P1_P1 :: Dict (K.Rem 'K.RoundHalfZero (P 1) (P 1) ~ P 0)
_test_Rem_RoundHalfZero_P1_P1 =  Dict
_test_Div_RoundHalfZero_P1_P2 :: Dict (K.Div 'K.RoundHalfZero (P 1) (P 2) ~ P 0)
_test_Div_RoundHalfZero_P1_P2 =  Dict
_test_Rem_RoundHalfZero_P1_P2 :: Dict (K.Rem 'K.RoundHalfZero (P 1) (P 2) ~ P 1)
_test_Rem_RoundHalfZero_P1_P2 =  Dict
_test_Div_RoundHalfZero_P1_P3 :: Dict (K.Div 'K.RoundHalfZero (P 1) (P 3) ~ P 0)
_test_Div_RoundHalfZero_P1_P3 =  Dict
_test_Rem_RoundHalfZero_P1_P3 :: Dict (K.Rem 'K.RoundHalfZero (P 1) (P 3) ~ P 1)
_test_Rem_RoundHalfZero_P1_P3 =  Dict
_test_Div_RoundHalfZero_P1_P4 :: Dict (K.Div 'K.RoundHalfZero (P 1) (P 4) ~ P 0)
_test_Div_RoundHalfZero_P1_P4 =  Dict
_test_Rem_RoundHalfZero_P1_P4 :: Dict (K.Rem 'K.RoundHalfZero (P 1) (P 4) ~ P 1)
_test_Rem_RoundHalfZero_P1_P4 =  Dict
_test_Div_RoundHalfZero_P2_N1 :: Dict (K.Div 'K.RoundHalfZero (P 2) (N 1) ~ N 2)
_test_Div_RoundHalfZero_P2_N1 =  Dict
_test_Rem_RoundHalfZero_P2_N1 :: Dict (K.Rem 'K.RoundHalfZero (P 2) (N 1) ~ P 0)
_test_Rem_RoundHalfZero_P2_N1 =  Dict
_test_Div_RoundHalfZero_P2_N2 :: Dict (K.Div 'K.RoundHalfZero (P 2) (N 2) ~ N 1)
_test_Div_RoundHalfZero_P2_N2 =  Dict
_test_Rem_RoundHalfZero_P2_N2 :: Dict (K.Rem 'K.RoundHalfZero (P 2) (N 2) ~ P 0)
_test_Rem_RoundHalfZero_P2_N2 =  Dict
_test_Div_RoundHalfZero_P2_N3 :: Dict (K.Div 'K.RoundHalfZero (P 2) (N 3) ~ N 1)
_test_Div_RoundHalfZero_P2_N3 =  Dict
_test_Rem_RoundHalfZero_P2_N3 :: Dict (K.Rem 'K.RoundHalfZero (P 2) (N 3) ~ N 1)
_test_Rem_RoundHalfZero_P2_N3 =  Dict
_test_Div_RoundHalfZero_P2_N4 :: Dict (K.Div 'K.RoundHalfZero (P 2) (N 4) ~ P 0)
_test_Div_RoundHalfZero_P2_N4 =  Dict
_test_Rem_RoundHalfZero_P2_N4 :: Dict (K.Rem 'K.RoundHalfZero (P 2) (N 4) ~ P 2)
_test_Rem_RoundHalfZero_P2_N4 =  Dict
_test_Div_RoundHalfZero_P2_P1 :: Dict (K.Div 'K.RoundHalfZero (P 2) (P 1) ~ P 2)
_test_Div_RoundHalfZero_P2_P1 =  Dict
_test_Rem_RoundHalfZero_P2_P1 :: Dict (K.Rem 'K.RoundHalfZero (P 2) (P 1) ~ P 0)
_test_Rem_RoundHalfZero_P2_P1 =  Dict
_test_Div_RoundHalfZero_P2_P2 :: Dict (K.Div 'K.RoundHalfZero (P 2) (P 2) ~ P 1)
_test_Div_RoundHalfZero_P2_P2 =  Dict
_test_Rem_RoundHalfZero_P2_P2 :: Dict (K.Rem 'K.RoundHalfZero (P 2) (P 2) ~ P 0)
_test_Rem_RoundHalfZero_P2_P2 =  Dict
_test_Div_RoundHalfZero_P2_P3 :: Dict (K.Div 'K.RoundHalfZero (P 2) (P 3) ~ P 1)
_test_Div_RoundHalfZero_P2_P3 =  Dict
_test_Rem_RoundHalfZero_P2_P3 :: Dict (K.Rem 'K.RoundHalfZero (P 2) (P 3) ~ N 1)
_test_Rem_RoundHalfZero_P2_P3 =  Dict
_test_Div_RoundHalfZero_P2_P4 :: Dict (K.Div 'K.RoundHalfZero (P 2) (P 4) ~ P 0)
_test_Div_RoundHalfZero_P2_P4 =  Dict
_test_Rem_RoundHalfZero_P2_P4 :: Dict (K.Rem 'K.RoundHalfZero (P 2) (P 4) ~ P 2)
_test_Rem_RoundHalfZero_P2_P4 =  Dict
_test_Div_RoundHalfZero_P3_N1 :: Dict (K.Div 'K.RoundHalfZero (P 3) (N 1) ~ N 3)
_test_Div_RoundHalfZero_P3_N1 =  Dict
_test_Rem_RoundHalfZero_P3_N1 :: Dict (K.Rem 'K.RoundHalfZero (P 3) (N 1) ~ P 0)
_test_Rem_RoundHalfZero_P3_N1 =  Dict
_test_Div_RoundHalfZero_P3_N2 :: Dict (K.Div 'K.RoundHalfZero (P 3) (N 2) ~ N 1)
_test_Div_RoundHalfZero_P3_N2 =  Dict
_test_Rem_RoundHalfZero_P3_N2 :: Dict (K.Rem 'K.RoundHalfZero (P 3) (N 2) ~ P 1)
_test_Rem_RoundHalfZero_P3_N2 =  Dict
_test_Div_RoundHalfZero_P3_N3 :: Dict (K.Div 'K.RoundHalfZero (P 3) (N 3) ~ N 1)
_test_Div_RoundHalfZero_P3_N3 =  Dict
_test_Rem_RoundHalfZero_P3_N3 :: Dict (K.Rem 'K.RoundHalfZero (P 3) (N 3) ~ P 0)
_test_Rem_RoundHalfZero_P3_N3 =  Dict
_test_Div_RoundHalfZero_P3_N4 :: Dict (K.Div 'K.RoundHalfZero (P 3) (N 4) ~ N 1)
_test_Div_RoundHalfZero_P3_N4 =  Dict
_test_Rem_RoundHalfZero_P3_N4 :: Dict (K.Rem 'K.RoundHalfZero (P 3) (N 4) ~ N 1)
_test_Rem_RoundHalfZero_P3_N4 =  Dict
_test_Div_RoundHalfZero_P3_P1 :: Dict (K.Div 'K.RoundHalfZero (P 3) (P 1) ~ P 3)
_test_Div_RoundHalfZero_P3_P1 =  Dict
_test_Rem_RoundHalfZero_P3_P1 :: Dict (K.Rem 'K.RoundHalfZero (P 3) (P 1) ~ P 0)
_test_Rem_RoundHalfZero_P3_P1 =  Dict
_test_Div_RoundHalfZero_P3_P2 :: Dict (K.Div 'K.RoundHalfZero (P 3) (P 2) ~ P 1)
_test_Div_RoundHalfZero_P3_P2 =  Dict
_test_Rem_RoundHalfZero_P3_P2 :: Dict (K.Rem 'K.RoundHalfZero (P 3) (P 2) ~ P 1)
_test_Rem_RoundHalfZero_P3_P2 =  Dict
_test_Div_RoundHalfZero_P3_P3 :: Dict (K.Div 'K.RoundHalfZero (P 3) (P 3) ~ P 1)
_test_Div_RoundHalfZero_P3_P3 =  Dict
_test_Rem_RoundHalfZero_P3_P3 :: Dict (K.Rem 'K.RoundHalfZero (P 3) (P 3) ~ P 0)
_test_Rem_RoundHalfZero_P3_P3 =  Dict
_test_Div_RoundHalfZero_P3_P4 :: Dict (K.Div 'K.RoundHalfZero (P 3) (P 4) ~ P 1)
_test_Div_RoundHalfZero_P3_P4 =  Dict
_test_Rem_RoundHalfZero_P3_P4 :: Dict (K.Rem 'K.RoundHalfZero (P 3) (P 4) ~ N 1)
_test_Rem_RoundHalfZero_P3_P4 =  Dict
_test_Div_RoundHalfZero_P4_N1 :: Dict (K.Div 'K.RoundHalfZero (P 4) (N 1) ~ N 4)
_test_Div_RoundHalfZero_P4_N1 =  Dict
_test_Rem_RoundHalfZero_P4_N1 :: Dict (K.Rem 'K.RoundHalfZero (P 4) (N 1) ~ P 0)
_test_Rem_RoundHalfZero_P4_N1 =  Dict
_test_Div_RoundHalfZero_P4_N2 :: Dict (K.Div 'K.RoundHalfZero (P 4) (N 2) ~ N 2)
_test_Div_RoundHalfZero_P4_N2 =  Dict
_test_Rem_RoundHalfZero_P4_N2 :: Dict (K.Rem 'K.RoundHalfZero (P 4) (N 2) ~ P 0)
_test_Rem_RoundHalfZero_P4_N2 =  Dict
_test_Div_RoundHalfZero_P4_N3 :: Dict (K.Div 'K.RoundHalfZero (P 4) (N 3) ~ N 1)
_test_Div_RoundHalfZero_P4_N3 =  Dict
_test_Rem_RoundHalfZero_P4_N3 :: Dict (K.Rem 'K.RoundHalfZero (P 4) (N 3) ~ P 1)
_test_Rem_RoundHalfZero_P4_N3 =  Dict
_test_Div_RoundHalfZero_P4_N4 :: Dict (K.Div 'K.RoundHalfZero (P 4) (N 4) ~ N 1)
_test_Div_RoundHalfZero_P4_N4 =  Dict
_test_Rem_RoundHalfZero_P4_N4 :: Dict (K.Rem 'K.RoundHalfZero (P 4) (N 4) ~ P 0)
_test_Rem_RoundHalfZero_P4_N4 =  Dict
_test_Div_RoundHalfZero_P4_P1 :: Dict (K.Div 'K.RoundHalfZero (P 4) (P 1) ~ P 4)
_test_Div_RoundHalfZero_P4_P1 =  Dict
_test_Rem_RoundHalfZero_P4_P1 :: Dict (K.Rem 'K.RoundHalfZero (P 4) (P 1) ~ P 0)
_test_Rem_RoundHalfZero_P4_P1 =  Dict
_test_Div_RoundHalfZero_P4_P2 :: Dict (K.Div 'K.RoundHalfZero (P 4) (P 2) ~ P 2)
_test_Div_RoundHalfZero_P4_P2 =  Dict
_test_Rem_RoundHalfZero_P4_P2 :: Dict (K.Rem 'K.RoundHalfZero (P 4) (P 2) ~ P 0)
_test_Rem_RoundHalfZero_P4_P2 =  Dict
_test_Div_RoundHalfZero_P4_P3 :: Dict (K.Div 'K.RoundHalfZero (P 4) (P 3) ~ P 1)
_test_Div_RoundHalfZero_P4_P3 =  Dict
_test_Rem_RoundHalfZero_P4_P3 :: Dict (K.Rem 'K.RoundHalfZero (P 4) (P 3) ~ P 1)
_test_Rem_RoundHalfZero_P4_P3 =  Dict
_test_Div_RoundHalfZero_P4_P4 :: Dict (K.Div 'K.RoundHalfZero (P 4) (P 4) ~ P 1)
_test_Div_RoundHalfZero_P4_P4 =  Dict
_test_Rem_RoundHalfZero_P4_P4 :: Dict (K.Rem 'K.RoundHalfZero (P 4) (P 4) ~ P 0)
_test_Rem_RoundHalfZero_P4_P4 =  Dict
_test_Div_RoundUp_N1_N1 :: Dict (K.Div 'K.RoundUp (N 1) (N 1) ~ P 1)
_test_Div_RoundUp_N1_N1 =  Dict
_test_Rem_RoundUp_N1_N1 :: Dict (K.Rem 'K.RoundUp (N 1) (N 1) ~ P 0)
_test_Rem_RoundUp_N1_N1 =  Dict
_test_Div_RoundUp_N1_N2 :: Dict (K.Div 'K.RoundUp (N 1) (N 2) ~ P 1)
_test_Div_RoundUp_N1_N2 =  Dict
_test_Rem_RoundUp_N1_N2 :: Dict (K.Rem 'K.RoundUp (N 1) (N 2) ~ P 1)
_test_Rem_RoundUp_N1_N2 =  Dict
_test_Div_RoundUp_N1_N3 :: Dict (K.Div 'K.RoundUp (N 1) (N 3) ~ P 1)
_test_Div_RoundUp_N1_N3 =  Dict
_test_Rem_RoundUp_N1_N3 :: Dict (K.Rem 'K.RoundUp (N 1) (N 3) ~ P 2)
_test_Rem_RoundUp_N1_N3 =  Dict
_test_Div_RoundUp_N1_N4 :: Dict (K.Div 'K.RoundUp (N 1) (N 4) ~ P 1)
_test_Div_RoundUp_N1_N4 =  Dict
_test_Rem_RoundUp_N1_N4 :: Dict (K.Rem 'K.RoundUp (N 1) (N 4) ~ P 3)
_test_Rem_RoundUp_N1_N4 =  Dict
_test_Div_RoundUp_N1_P1 :: Dict (K.Div 'K.RoundUp (N 1) (P 1) ~ N 1)
_test_Div_RoundUp_N1_P1 =  Dict
_test_Rem_RoundUp_N1_P1 :: Dict (K.Rem 'K.RoundUp (N 1) (P 1) ~ P 0)
_test_Rem_RoundUp_N1_P1 =  Dict
_test_Div_RoundUp_N1_P2 :: Dict (K.Div 'K.RoundUp (N 1) (P 2) ~ P 0)
_test_Div_RoundUp_N1_P2 =  Dict
_test_Rem_RoundUp_N1_P2 :: Dict (K.Rem 'K.RoundUp (N 1) (P 2) ~ N 1)
_test_Rem_RoundUp_N1_P2 =  Dict
_test_Div_RoundUp_N1_P3 :: Dict (K.Div 'K.RoundUp (N 1) (P 3) ~ P 0)
_test_Div_RoundUp_N1_P3 =  Dict
_test_Rem_RoundUp_N1_P3 :: Dict (K.Rem 'K.RoundUp (N 1) (P 3) ~ N 1)
_test_Rem_RoundUp_N1_P3 =  Dict
_test_Div_RoundUp_N1_P4 :: Dict (K.Div 'K.RoundUp (N 1) (P 4) ~ P 0)
_test_Div_RoundUp_N1_P4 =  Dict
_test_Rem_RoundUp_N1_P4 :: Dict (K.Rem 'K.RoundUp (N 1) (P 4) ~ N 1)
_test_Rem_RoundUp_N1_P4 =  Dict
_test_Div_RoundUp_N2_N1 :: Dict (K.Div 'K.RoundUp (N 2) (N 1) ~ P 2)
_test_Div_RoundUp_N2_N1 =  Dict
_test_Rem_RoundUp_N2_N1 :: Dict (K.Rem 'K.RoundUp (N 2) (N 1) ~ P 0)
_test_Rem_RoundUp_N2_N1 =  Dict
_test_Div_RoundUp_N2_N2 :: Dict (K.Div 'K.RoundUp (N 2) (N 2) ~ P 1)
_test_Div_RoundUp_N2_N2 =  Dict
_test_Rem_RoundUp_N2_N2 :: Dict (K.Rem 'K.RoundUp (N 2) (N 2) ~ P 0)
_test_Rem_RoundUp_N2_N2 =  Dict
_test_Div_RoundUp_N2_N3 :: Dict (K.Div 'K.RoundUp (N 2) (N 3) ~ P 1)
_test_Div_RoundUp_N2_N3 =  Dict
_test_Rem_RoundUp_N2_N3 :: Dict (K.Rem 'K.RoundUp (N 2) (N 3) ~ P 1)
_test_Rem_RoundUp_N2_N3 =  Dict
_test_Div_RoundUp_N2_N4 :: Dict (K.Div 'K.RoundUp (N 2) (N 4) ~ P 1)
_test_Div_RoundUp_N2_N4 =  Dict
_test_Rem_RoundUp_N2_N4 :: Dict (K.Rem 'K.RoundUp (N 2) (N 4) ~ P 2)
_test_Rem_RoundUp_N2_N4 =  Dict
_test_Div_RoundUp_N2_P1 :: Dict (K.Div 'K.RoundUp (N 2) (P 1) ~ N 2)
_test_Div_RoundUp_N2_P1 =  Dict
_test_Rem_RoundUp_N2_P1 :: Dict (K.Rem 'K.RoundUp (N 2) (P 1) ~ P 0)
_test_Rem_RoundUp_N2_P1 =  Dict
_test_Div_RoundUp_N2_P2 :: Dict (K.Div 'K.RoundUp (N 2) (P 2) ~ N 1)
_test_Div_RoundUp_N2_P2 =  Dict
_test_Rem_RoundUp_N2_P2 :: Dict (K.Rem 'K.RoundUp (N 2) (P 2) ~ P 0)
_test_Rem_RoundUp_N2_P2 =  Dict
_test_Div_RoundUp_N2_P3 :: Dict (K.Div 'K.RoundUp (N 2) (P 3) ~ P 0)
_test_Div_RoundUp_N2_P3 =  Dict
_test_Rem_RoundUp_N2_P3 :: Dict (K.Rem 'K.RoundUp (N 2) (P 3) ~ N 2)
_test_Rem_RoundUp_N2_P3 =  Dict
_test_Div_RoundUp_N2_P4 :: Dict (K.Div 'K.RoundUp (N 2) (P 4) ~ P 0)
_test_Div_RoundUp_N2_P4 =  Dict
_test_Rem_RoundUp_N2_P4 :: Dict (K.Rem 'K.RoundUp (N 2) (P 4) ~ N 2)
_test_Rem_RoundUp_N2_P4 =  Dict
_test_Div_RoundUp_N3_N1 :: Dict (K.Div 'K.RoundUp (N 3) (N 1) ~ P 3)
_test_Div_RoundUp_N3_N1 =  Dict
_test_Rem_RoundUp_N3_N1 :: Dict (K.Rem 'K.RoundUp (N 3) (N 1) ~ P 0)
_test_Rem_RoundUp_N3_N1 =  Dict
_test_Div_RoundUp_N3_N2 :: Dict (K.Div 'K.RoundUp (N 3) (N 2) ~ P 2)
_test_Div_RoundUp_N3_N2 =  Dict
_test_Rem_RoundUp_N3_N2 :: Dict (K.Rem 'K.RoundUp (N 3) (N 2) ~ P 1)
_test_Rem_RoundUp_N3_N2 =  Dict
_test_Div_RoundUp_N3_N3 :: Dict (K.Div 'K.RoundUp (N 3) (N 3) ~ P 1)
_test_Div_RoundUp_N3_N3 =  Dict
_test_Rem_RoundUp_N3_N3 :: Dict (K.Rem 'K.RoundUp (N 3) (N 3) ~ P 0)
_test_Rem_RoundUp_N3_N3 =  Dict
_test_Div_RoundUp_N3_N4 :: Dict (K.Div 'K.RoundUp (N 3) (N 4) ~ P 1)
_test_Div_RoundUp_N3_N4 =  Dict
_test_Rem_RoundUp_N3_N4 :: Dict (K.Rem 'K.RoundUp (N 3) (N 4) ~ P 1)
_test_Rem_RoundUp_N3_N4 =  Dict
_test_Div_RoundUp_N3_P1 :: Dict (K.Div 'K.RoundUp (N 3) (P 1) ~ N 3)
_test_Div_RoundUp_N3_P1 =  Dict
_test_Rem_RoundUp_N3_P1 :: Dict (K.Rem 'K.RoundUp (N 3) (P 1) ~ P 0)
_test_Rem_RoundUp_N3_P1 =  Dict
_test_Div_RoundUp_N3_P2 :: Dict (K.Div 'K.RoundUp (N 3) (P 2) ~ N 1)
_test_Div_RoundUp_N3_P2 =  Dict
_test_Rem_RoundUp_N3_P2 :: Dict (K.Rem 'K.RoundUp (N 3) (P 2) ~ N 1)
_test_Rem_RoundUp_N3_P2 =  Dict
_test_Div_RoundUp_N3_P3 :: Dict (K.Div 'K.RoundUp (N 3) (P 3) ~ N 1)
_test_Div_RoundUp_N3_P3 =  Dict
_test_Rem_RoundUp_N3_P3 :: Dict (K.Rem 'K.RoundUp (N 3) (P 3) ~ P 0)
_test_Rem_RoundUp_N3_P3 =  Dict
_test_Div_RoundUp_N3_P4 :: Dict (K.Div 'K.RoundUp (N 3) (P 4) ~ P 0)
_test_Div_RoundUp_N3_P4 =  Dict
_test_Rem_RoundUp_N3_P4 :: Dict (K.Rem 'K.RoundUp (N 3) (P 4) ~ N 3)
_test_Rem_RoundUp_N3_P4 =  Dict
_test_Div_RoundUp_N4_N1 :: Dict (K.Div 'K.RoundUp (N 4) (N 1) ~ P 4)
_test_Div_RoundUp_N4_N1 =  Dict
_test_Rem_RoundUp_N4_N1 :: Dict (K.Rem 'K.RoundUp (N 4) (N 1) ~ P 0)
_test_Rem_RoundUp_N4_N1 =  Dict
_test_Div_RoundUp_N4_N2 :: Dict (K.Div 'K.RoundUp (N 4) (N 2) ~ P 2)
_test_Div_RoundUp_N4_N2 =  Dict
_test_Rem_RoundUp_N4_N2 :: Dict (K.Rem 'K.RoundUp (N 4) (N 2) ~ P 0)
_test_Rem_RoundUp_N4_N2 =  Dict
_test_Div_RoundUp_N4_N3 :: Dict (K.Div 'K.RoundUp (N 4) (N 3) ~ P 2)
_test_Div_RoundUp_N4_N3 =  Dict
_test_Rem_RoundUp_N4_N3 :: Dict (K.Rem 'K.RoundUp (N 4) (N 3) ~ P 2)
_test_Rem_RoundUp_N4_N3 =  Dict
_test_Div_RoundUp_N4_N4 :: Dict (K.Div 'K.RoundUp (N 4) (N 4) ~ P 1)
_test_Div_RoundUp_N4_N4 =  Dict
_test_Rem_RoundUp_N4_N4 :: Dict (K.Rem 'K.RoundUp (N 4) (N 4) ~ P 0)
_test_Rem_RoundUp_N4_N4 =  Dict
_test_Div_RoundUp_N4_P1 :: Dict (K.Div 'K.RoundUp (N 4) (P 1) ~ N 4)
_test_Div_RoundUp_N4_P1 =  Dict
_test_Rem_RoundUp_N4_P1 :: Dict (K.Rem 'K.RoundUp (N 4) (P 1) ~ P 0)
_test_Rem_RoundUp_N4_P1 =  Dict
_test_Div_RoundUp_N4_P2 :: Dict (K.Div 'K.RoundUp (N 4) (P 2) ~ N 2)
_test_Div_RoundUp_N4_P2 =  Dict
_test_Rem_RoundUp_N4_P2 :: Dict (K.Rem 'K.RoundUp (N 4) (P 2) ~ P 0)
_test_Rem_RoundUp_N4_P2 =  Dict
_test_Div_RoundUp_N4_P3 :: Dict (K.Div 'K.RoundUp (N 4) (P 3) ~ N 1)
_test_Div_RoundUp_N4_P3 =  Dict
_test_Rem_RoundUp_N4_P3 :: Dict (K.Rem 'K.RoundUp (N 4) (P 3) ~ N 1)
_test_Rem_RoundUp_N4_P3 =  Dict
_test_Div_RoundUp_N4_P4 :: Dict (K.Div 'K.RoundUp (N 4) (P 4) ~ N 1)
_test_Div_RoundUp_N4_P4 =  Dict
_test_Rem_RoundUp_N4_P4 :: Dict (K.Rem 'K.RoundUp (N 4) (P 4) ~ P 0)
_test_Rem_RoundUp_N4_P4 =  Dict
_test_Div_RoundUp_P0_N1 :: Dict (K.Div 'K.RoundUp (P 0) (N 1) ~ P 0)
_test_Div_RoundUp_P0_N1 =  Dict
_test_Rem_RoundUp_P0_N1 :: Dict (K.Rem 'K.RoundUp (P 0) (N 1) ~ P 0)
_test_Rem_RoundUp_P0_N1 =  Dict
_test_Div_RoundUp_P0_N2 :: Dict (K.Div 'K.RoundUp (P 0) (N 2) ~ P 0)
_test_Div_RoundUp_P0_N2 =  Dict
_test_Rem_RoundUp_P0_N2 :: Dict (K.Rem 'K.RoundUp (P 0) (N 2) ~ P 0)
_test_Rem_RoundUp_P0_N2 =  Dict
_test_Div_RoundUp_P0_N3 :: Dict (K.Div 'K.RoundUp (P 0) (N 3) ~ P 0)
_test_Div_RoundUp_P0_N3 =  Dict
_test_Rem_RoundUp_P0_N3 :: Dict (K.Rem 'K.RoundUp (P 0) (N 3) ~ P 0)
_test_Rem_RoundUp_P0_N3 =  Dict
_test_Div_RoundUp_P0_N4 :: Dict (K.Div 'K.RoundUp (P 0) (N 4) ~ P 0)
_test_Div_RoundUp_P0_N4 =  Dict
_test_Rem_RoundUp_P0_N4 :: Dict (K.Rem 'K.RoundUp (P 0) (N 4) ~ P 0)
_test_Rem_RoundUp_P0_N4 =  Dict
_test_Div_RoundUp_P0_P1 :: Dict (K.Div 'K.RoundUp (P 0) (P 1) ~ P 0)
_test_Div_RoundUp_P0_P1 =  Dict
_test_Rem_RoundUp_P0_P1 :: Dict (K.Rem 'K.RoundUp (P 0) (P 1) ~ P 0)
_test_Rem_RoundUp_P0_P1 =  Dict
_test_Div_RoundUp_P0_P2 :: Dict (K.Div 'K.RoundUp (P 0) (P 2) ~ P 0)
_test_Div_RoundUp_P0_P2 =  Dict
_test_Rem_RoundUp_P0_P2 :: Dict (K.Rem 'K.RoundUp (P 0) (P 2) ~ P 0)
_test_Rem_RoundUp_P0_P2 =  Dict
_test_Div_RoundUp_P0_P3 :: Dict (K.Div 'K.RoundUp (P 0) (P 3) ~ P 0)
_test_Div_RoundUp_P0_P3 =  Dict
_test_Rem_RoundUp_P0_P3 :: Dict (K.Rem 'K.RoundUp (P 0) (P 3) ~ P 0)
_test_Rem_RoundUp_P0_P3 =  Dict
_test_Div_RoundUp_P0_P4 :: Dict (K.Div 'K.RoundUp (P 0) (P 4) ~ P 0)
_test_Div_RoundUp_P0_P4 =  Dict
_test_Rem_RoundUp_P0_P4 :: Dict (K.Rem 'K.RoundUp (P 0) (P 4) ~ P 0)
_test_Rem_RoundUp_P0_P4 =  Dict
_test_Div_RoundUp_P1_N1 :: Dict (K.Div 'K.RoundUp (P 1) (N 1) ~ N 1)
_test_Div_RoundUp_P1_N1 =  Dict
_test_Rem_RoundUp_P1_N1 :: Dict (K.Rem 'K.RoundUp (P 1) (N 1) ~ P 0)
_test_Rem_RoundUp_P1_N1 =  Dict
_test_Div_RoundUp_P1_N2 :: Dict (K.Div 'K.RoundUp (P 1) (N 2) ~ P 0)
_test_Div_RoundUp_P1_N2 =  Dict
_test_Rem_RoundUp_P1_N2 :: Dict (K.Rem 'K.RoundUp (P 1) (N 2) ~ P 1)
_test_Rem_RoundUp_P1_N2 =  Dict
_test_Div_RoundUp_P1_N3 :: Dict (K.Div 'K.RoundUp (P 1) (N 3) ~ P 0)
_test_Div_RoundUp_P1_N3 =  Dict
_test_Rem_RoundUp_P1_N3 :: Dict (K.Rem 'K.RoundUp (P 1) (N 3) ~ P 1)
_test_Rem_RoundUp_P1_N3 =  Dict
_test_Div_RoundUp_P1_N4 :: Dict (K.Div 'K.RoundUp (P 1) (N 4) ~ P 0)
_test_Div_RoundUp_P1_N4 =  Dict
_test_Rem_RoundUp_P1_N4 :: Dict (K.Rem 'K.RoundUp (P 1) (N 4) ~ P 1)
_test_Rem_RoundUp_P1_N4 =  Dict
_test_Div_RoundUp_P1_P1 :: Dict (K.Div 'K.RoundUp (P 1) (P 1) ~ P 1)
_test_Div_RoundUp_P1_P1 =  Dict
_test_Rem_RoundUp_P1_P1 :: Dict (K.Rem 'K.RoundUp (P 1) (P 1) ~ P 0)
_test_Rem_RoundUp_P1_P1 =  Dict
_test_Div_RoundUp_P1_P2 :: Dict (K.Div 'K.RoundUp (P 1) (P 2) ~ P 1)
_test_Div_RoundUp_P1_P2 =  Dict
_test_Rem_RoundUp_P1_P2 :: Dict (K.Rem 'K.RoundUp (P 1) (P 2) ~ N 1)
_test_Rem_RoundUp_P1_P2 =  Dict
_test_Div_RoundUp_P1_P3 :: Dict (K.Div 'K.RoundUp (P 1) (P 3) ~ P 1)
_test_Div_RoundUp_P1_P3 =  Dict
_test_Rem_RoundUp_P1_P3 :: Dict (K.Rem 'K.RoundUp (P 1) (P 3) ~ N 2)
_test_Rem_RoundUp_P1_P3 =  Dict
_test_Div_RoundUp_P1_P4 :: Dict (K.Div 'K.RoundUp (P 1) (P 4) ~ P 1)
_test_Div_RoundUp_P1_P4 =  Dict
_test_Rem_RoundUp_P1_P4 :: Dict (K.Rem 'K.RoundUp (P 1) (P 4) ~ N 3)
_test_Rem_RoundUp_P1_P4 =  Dict
_test_Div_RoundUp_P2_N1 :: Dict (K.Div 'K.RoundUp (P 2) (N 1) ~ N 2)
_test_Div_RoundUp_P2_N1 =  Dict
_test_Rem_RoundUp_P2_N1 :: Dict (K.Rem 'K.RoundUp (P 2) (N 1) ~ P 0)
_test_Rem_RoundUp_P2_N1 =  Dict
_test_Div_RoundUp_P2_N2 :: Dict (K.Div 'K.RoundUp (P 2) (N 2) ~ N 1)
_test_Div_RoundUp_P2_N2 =  Dict
_test_Rem_RoundUp_P2_N2 :: Dict (K.Rem 'K.RoundUp (P 2) (N 2) ~ P 0)
_test_Rem_RoundUp_P2_N2 =  Dict
_test_Div_RoundUp_P2_N3 :: Dict (K.Div 'K.RoundUp (P 2) (N 3) ~ P 0)
_test_Div_RoundUp_P2_N3 =  Dict
_test_Rem_RoundUp_P2_N3 :: Dict (K.Rem 'K.RoundUp (P 2) (N 3) ~ P 2)
_test_Rem_RoundUp_P2_N3 =  Dict
_test_Div_RoundUp_P2_N4 :: Dict (K.Div 'K.RoundUp (P 2) (N 4) ~ P 0)
_test_Div_RoundUp_P2_N4 =  Dict
_test_Rem_RoundUp_P2_N4 :: Dict (K.Rem 'K.RoundUp (P 2) (N 4) ~ P 2)
_test_Rem_RoundUp_P2_N4 =  Dict
_test_Div_RoundUp_P2_P1 :: Dict (K.Div 'K.RoundUp (P 2) (P 1) ~ P 2)
_test_Div_RoundUp_P2_P1 =  Dict
_test_Rem_RoundUp_P2_P1 :: Dict (K.Rem 'K.RoundUp (P 2) (P 1) ~ P 0)
_test_Rem_RoundUp_P2_P1 =  Dict
_test_Div_RoundUp_P2_P2 :: Dict (K.Div 'K.RoundUp (P 2) (P 2) ~ P 1)
_test_Div_RoundUp_P2_P2 =  Dict
_test_Rem_RoundUp_P2_P2 :: Dict (K.Rem 'K.RoundUp (P 2) (P 2) ~ P 0)
_test_Rem_RoundUp_P2_P2 =  Dict
_test_Div_RoundUp_P2_P3 :: Dict (K.Div 'K.RoundUp (P 2) (P 3) ~ P 1)
_test_Div_RoundUp_P2_P3 =  Dict
_test_Rem_RoundUp_P2_P3 :: Dict (K.Rem 'K.RoundUp (P 2) (P 3) ~ N 1)
_test_Rem_RoundUp_P2_P3 =  Dict
_test_Div_RoundUp_P2_P4 :: Dict (K.Div 'K.RoundUp (P 2) (P 4) ~ P 1)
_test_Div_RoundUp_P2_P4 =  Dict
_test_Rem_RoundUp_P2_P4 :: Dict (K.Rem 'K.RoundUp (P 2) (P 4) ~ N 2)
_test_Rem_RoundUp_P2_P4 =  Dict
_test_Div_RoundUp_P3_N1 :: Dict (K.Div 'K.RoundUp (P 3) (N 1) ~ N 3)
_test_Div_RoundUp_P3_N1 =  Dict
_test_Rem_RoundUp_P3_N1 :: Dict (K.Rem 'K.RoundUp (P 3) (N 1) ~ P 0)
_test_Rem_RoundUp_P3_N1 =  Dict
_test_Div_RoundUp_P3_N2 :: Dict (K.Div 'K.RoundUp (P 3) (N 2) ~ N 1)
_test_Div_RoundUp_P3_N2 =  Dict
_test_Rem_RoundUp_P3_N2 :: Dict (K.Rem 'K.RoundUp (P 3) (N 2) ~ P 1)
_test_Rem_RoundUp_P3_N2 =  Dict
_test_Div_RoundUp_P3_N3 :: Dict (K.Div 'K.RoundUp (P 3) (N 3) ~ N 1)
_test_Div_RoundUp_P3_N3 =  Dict
_test_Rem_RoundUp_P3_N3 :: Dict (K.Rem 'K.RoundUp (P 3) (N 3) ~ P 0)
_test_Rem_RoundUp_P3_N3 =  Dict
_test_Div_RoundUp_P3_N4 :: Dict (K.Div 'K.RoundUp (P 3) (N 4) ~ P 0)
_test_Div_RoundUp_P3_N4 =  Dict
_test_Rem_RoundUp_P3_N4 :: Dict (K.Rem 'K.RoundUp (P 3) (N 4) ~ P 3)
_test_Rem_RoundUp_P3_N4 =  Dict
_test_Div_RoundUp_P3_P1 :: Dict (K.Div 'K.RoundUp (P 3) (P 1) ~ P 3)
_test_Div_RoundUp_P3_P1 =  Dict
_test_Rem_RoundUp_P3_P1 :: Dict (K.Rem 'K.RoundUp (P 3) (P 1) ~ P 0)
_test_Rem_RoundUp_P3_P1 =  Dict
_test_Div_RoundUp_P3_P2 :: Dict (K.Div 'K.RoundUp (P 3) (P 2) ~ P 2)
_test_Div_RoundUp_P3_P2 =  Dict
_test_Rem_RoundUp_P3_P2 :: Dict (K.Rem 'K.RoundUp (P 3) (P 2) ~ N 1)
_test_Rem_RoundUp_P3_P2 =  Dict
_test_Div_RoundUp_P3_P3 :: Dict (K.Div 'K.RoundUp (P 3) (P 3) ~ P 1)
_test_Div_RoundUp_P3_P3 =  Dict
_test_Rem_RoundUp_P3_P3 :: Dict (K.Rem 'K.RoundUp (P 3) (P 3) ~ P 0)
_test_Rem_RoundUp_P3_P3 =  Dict
_test_Div_RoundUp_P3_P4 :: Dict (K.Div 'K.RoundUp (P 3) (P 4) ~ P 1)
_test_Div_RoundUp_P3_P4 =  Dict
_test_Rem_RoundUp_P3_P4 :: Dict (K.Rem 'K.RoundUp (P 3) (P 4) ~ N 1)
_test_Rem_RoundUp_P3_P4 =  Dict
_test_Div_RoundUp_P4_N1 :: Dict (K.Div 'K.RoundUp (P 4) (N 1) ~ N 4)
_test_Div_RoundUp_P4_N1 =  Dict
_test_Rem_RoundUp_P4_N1 :: Dict (K.Rem 'K.RoundUp (P 4) (N 1) ~ P 0)
_test_Rem_RoundUp_P4_N1 =  Dict
_test_Div_RoundUp_P4_N2 :: Dict (K.Div 'K.RoundUp (P 4) (N 2) ~ N 2)
_test_Div_RoundUp_P4_N2 =  Dict
_test_Rem_RoundUp_P4_N2 :: Dict (K.Rem 'K.RoundUp (P 4) (N 2) ~ P 0)
_test_Rem_RoundUp_P4_N2 =  Dict
_test_Div_RoundUp_P4_N3 :: Dict (K.Div 'K.RoundUp (P 4) (N 3) ~ N 1)
_test_Div_RoundUp_P4_N3 =  Dict
_test_Rem_RoundUp_P4_N3 :: Dict (K.Rem 'K.RoundUp (P 4) (N 3) ~ P 1)
_test_Rem_RoundUp_P4_N3 =  Dict
_test_Div_RoundUp_P4_N4 :: Dict (K.Div 'K.RoundUp (P 4) (N 4) ~ N 1)
_test_Div_RoundUp_P4_N4 =  Dict
_test_Rem_RoundUp_P4_N4 :: Dict (K.Rem 'K.RoundUp (P 4) (N 4) ~ P 0)
_test_Rem_RoundUp_P4_N4 =  Dict
_test_Div_RoundUp_P4_P1 :: Dict (K.Div 'K.RoundUp (P 4) (P 1) ~ P 4)
_test_Div_RoundUp_P4_P1 =  Dict
_test_Rem_RoundUp_P4_P1 :: Dict (K.Rem 'K.RoundUp (P 4) (P 1) ~ P 0)
_test_Rem_RoundUp_P4_P1 =  Dict
_test_Div_RoundUp_P4_P2 :: Dict (K.Div 'K.RoundUp (P 4) (P 2) ~ P 2)
_test_Div_RoundUp_P4_P2 =  Dict
_test_Rem_RoundUp_P4_P2 :: Dict (K.Rem 'K.RoundUp (P 4) (P 2) ~ P 0)
_test_Rem_RoundUp_P4_P2 =  Dict
_test_Div_RoundUp_P4_P3 :: Dict (K.Div 'K.RoundUp (P 4) (P 3) ~ P 2)
_test_Div_RoundUp_P4_P3 =  Dict
_test_Rem_RoundUp_P4_P3 :: Dict (K.Rem 'K.RoundUp (P 4) (P 3) ~ N 2)
_test_Rem_RoundUp_P4_P3 =  Dict
_test_Div_RoundUp_P4_P4 :: Dict (K.Div 'K.RoundUp (P 4) (P 4) ~ P 1)
_test_Div_RoundUp_P4_P4 =  Dict
_test_Rem_RoundUp_P4_P4 :: Dict (K.Rem 'K.RoundUp (P 4) (P 4) ~ P 0)
_test_Rem_RoundUp_P4_P4 =  Dict
_test_Div_RoundZero_N1_N1 :: Dict (K.Div 'K.RoundZero (N 1) (N 1) ~ P 1)
_test_Div_RoundZero_N1_N1 =  Dict
_test_Rem_RoundZero_N1_N1 :: Dict (K.Rem 'K.RoundZero (N 1) (N 1) ~ P 0)
_test_Rem_RoundZero_N1_N1 =  Dict
_test_Div_RoundZero_N1_N2 :: Dict (K.Div 'K.RoundZero (N 1) (N 2) ~ P 0)
_test_Div_RoundZero_N1_N2 =  Dict
_test_Rem_RoundZero_N1_N2 :: Dict (K.Rem 'K.RoundZero (N 1) (N 2) ~ N 1)
_test_Rem_RoundZero_N1_N2 =  Dict
_test_Div_RoundZero_N1_N3 :: Dict (K.Div 'K.RoundZero (N 1) (N 3) ~ P 0)
_test_Div_RoundZero_N1_N3 =  Dict
_test_Rem_RoundZero_N1_N3 :: Dict (K.Rem 'K.RoundZero (N 1) (N 3) ~ N 1)
_test_Rem_RoundZero_N1_N3 =  Dict
_test_Div_RoundZero_N1_N4 :: Dict (K.Div 'K.RoundZero (N 1) (N 4) ~ P 0)
_test_Div_RoundZero_N1_N4 =  Dict
_test_Rem_RoundZero_N1_N4 :: Dict (K.Rem 'K.RoundZero (N 1) (N 4) ~ N 1)
_test_Rem_RoundZero_N1_N4 =  Dict
_test_Div_RoundZero_N1_P1 :: Dict (K.Div 'K.RoundZero (N 1) (P 1) ~ N 1)
_test_Div_RoundZero_N1_P1 =  Dict
_test_Rem_RoundZero_N1_P1 :: Dict (K.Rem 'K.RoundZero (N 1) (P 1) ~ P 0)
_test_Rem_RoundZero_N1_P1 =  Dict
_test_Div_RoundZero_N1_P2 :: Dict (K.Div 'K.RoundZero (N 1) (P 2) ~ P 0)
_test_Div_RoundZero_N1_P2 =  Dict
_test_Rem_RoundZero_N1_P2 :: Dict (K.Rem 'K.RoundZero (N 1) (P 2) ~ N 1)
_test_Rem_RoundZero_N1_P2 =  Dict
_test_Div_RoundZero_N1_P3 :: Dict (K.Div 'K.RoundZero (N 1) (P 3) ~ P 0)
_test_Div_RoundZero_N1_P3 =  Dict
_test_Rem_RoundZero_N1_P3 :: Dict (K.Rem 'K.RoundZero (N 1) (P 3) ~ N 1)
_test_Rem_RoundZero_N1_P3 =  Dict
_test_Div_RoundZero_N1_P4 :: Dict (K.Div 'K.RoundZero (N 1) (P 4) ~ P 0)
_test_Div_RoundZero_N1_P4 =  Dict
_test_Rem_RoundZero_N1_P4 :: Dict (K.Rem 'K.RoundZero (N 1) (P 4) ~ N 1)
_test_Rem_RoundZero_N1_P4 =  Dict
_test_Div_RoundZero_N2_N1 :: Dict (K.Div 'K.RoundZero (N 2) (N 1) ~ P 2)
_test_Div_RoundZero_N2_N1 =  Dict
_test_Rem_RoundZero_N2_N1 :: Dict (K.Rem 'K.RoundZero (N 2) (N 1) ~ P 0)
_test_Rem_RoundZero_N2_N1 =  Dict
_test_Div_RoundZero_N2_N2 :: Dict (K.Div 'K.RoundZero (N 2) (N 2) ~ P 1)
_test_Div_RoundZero_N2_N2 =  Dict
_test_Rem_RoundZero_N2_N2 :: Dict (K.Rem 'K.RoundZero (N 2) (N 2) ~ P 0)
_test_Rem_RoundZero_N2_N2 =  Dict
_test_Div_RoundZero_N2_N3 :: Dict (K.Div 'K.RoundZero (N 2) (N 3) ~ P 0)
_test_Div_RoundZero_N2_N3 =  Dict
_test_Rem_RoundZero_N2_N3 :: Dict (K.Rem 'K.RoundZero (N 2) (N 3) ~ N 2)
_test_Rem_RoundZero_N2_N3 =  Dict
_test_Div_RoundZero_N2_N4 :: Dict (K.Div 'K.RoundZero (N 2) (N 4) ~ P 0)
_test_Div_RoundZero_N2_N4 =  Dict
_test_Rem_RoundZero_N2_N4 :: Dict (K.Rem 'K.RoundZero (N 2) (N 4) ~ N 2)
_test_Rem_RoundZero_N2_N4 =  Dict
_test_Div_RoundZero_N2_P1 :: Dict (K.Div 'K.RoundZero (N 2) (P 1) ~ N 2)
_test_Div_RoundZero_N2_P1 =  Dict
_test_Rem_RoundZero_N2_P1 :: Dict (K.Rem 'K.RoundZero (N 2) (P 1) ~ P 0)
_test_Rem_RoundZero_N2_P1 =  Dict
_test_Div_RoundZero_N2_P2 :: Dict (K.Div 'K.RoundZero (N 2) (P 2) ~ N 1)
_test_Div_RoundZero_N2_P2 =  Dict
_test_Rem_RoundZero_N2_P2 :: Dict (K.Rem 'K.RoundZero (N 2) (P 2) ~ P 0)
_test_Rem_RoundZero_N2_P2 =  Dict
_test_Div_RoundZero_N2_P3 :: Dict (K.Div 'K.RoundZero (N 2) (P 3) ~ P 0)
_test_Div_RoundZero_N2_P3 =  Dict
_test_Rem_RoundZero_N2_P3 :: Dict (K.Rem 'K.RoundZero (N 2) (P 3) ~ N 2)
_test_Rem_RoundZero_N2_P3 =  Dict
_test_Div_RoundZero_N2_P4 :: Dict (K.Div 'K.RoundZero (N 2) (P 4) ~ P 0)
_test_Div_RoundZero_N2_P4 =  Dict
_test_Rem_RoundZero_N2_P4 :: Dict (K.Rem 'K.RoundZero (N 2) (P 4) ~ N 2)
_test_Rem_RoundZero_N2_P4 =  Dict
_test_Div_RoundZero_N3_N1 :: Dict (K.Div 'K.RoundZero (N 3) (N 1) ~ P 3)
_test_Div_RoundZero_N3_N1 =  Dict
_test_Rem_RoundZero_N3_N1 :: Dict (K.Rem 'K.RoundZero (N 3) (N 1) ~ P 0)
_test_Rem_RoundZero_N3_N1 =  Dict
_test_Div_RoundZero_N3_N2 :: Dict (K.Div 'K.RoundZero (N 3) (N 2) ~ P 1)
_test_Div_RoundZero_N3_N2 =  Dict
_test_Rem_RoundZero_N3_N2 :: Dict (K.Rem 'K.RoundZero (N 3) (N 2) ~ N 1)
_test_Rem_RoundZero_N3_N2 =  Dict
_test_Div_RoundZero_N3_N3 :: Dict (K.Div 'K.RoundZero (N 3) (N 3) ~ P 1)
_test_Div_RoundZero_N3_N3 =  Dict
_test_Rem_RoundZero_N3_N3 :: Dict (K.Rem 'K.RoundZero (N 3) (N 3) ~ P 0)
_test_Rem_RoundZero_N3_N3 =  Dict
_test_Div_RoundZero_N3_N4 :: Dict (K.Div 'K.RoundZero (N 3) (N 4) ~ P 0)
_test_Div_RoundZero_N3_N4 =  Dict
_test_Rem_RoundZero_N3_N4 :: Dict (K.Rem 'K.RoundZero (N 3) (N 4) ~ N 3)
_test_Rem_RoundZero_N3_N4 =  Dict
_test_Div_RoundZero_N3_P1 :: Dict (K.Div 'K.RoundZero (N 3) (P 1) ~ N 3)
_test_Div_RoundZero_N3_P1 =  Dict
_test_Rem_RoundZero_N3_P1 :: Dict (K.Rem 'K.RoundZero (N 3) (P 1) ~ P 0)
_test_Rem_RoundZero_N3_P1 =  Dict
_test_Div_RoundZero_N3_P2 :: Dict (K.Div 'K.RoundZero (N 3) (P 2) ~ N 1)
_test_Div_RoundZero_N3_P2 =  Dict
_test_Rem_RoundZero_N3_P2 :: Dict (K.Rem 'K.RoundZero (N 3) (P 2) ~ N 1)
_test_Rem_RoundZero_N3_P2 =  Dict
_test_Div_RoundZero_N3_P3 :: Dict (K.Div 'K.RoundZero (N 3) (P 3) ~ N 1)
_test_Div_RoundZero_N3_P3 =  Dict
_test_Rem_RoundZero_N3_P3 :: Dict (K.Rem 'K.RoundZero (N 3) (P 3) ~ P 0)
_test_Rem_RoundZero_N3_P3 =  Dict
_test_Div_RoundZero_N3_P4 :: Dict (K.Div 'K.RoundZero (N 3) (P 4) ~ P 0)
_test_Div_RoundZero_N3_P4 =  Dict
_test_Rem_RoundZero_N3_P4 :: Dict (K.Rem 'K.RoundZero (N 3) (P 4) ~ N 3)
_test_Rem_RoundZero_N3_P4 =  Dict
_test_Div_RoundZero_N4_N1 :: Dict (K.Div 'K.RoundZero (N 4) (N 1) ~ P 4)
_test_Div_RoundZero_N4_N1 =  Dict
_test_Rem_RoundZero_N4_N1 :: Dict (K.Rem 'K.RoundZero (N 4) (N 1) ~ P 0)
_test_Rem_RoundZero_N4_N1 =  Dict
_test_Div_RoundZero_N4_N2 :: Dict (K.Div 'K.RoundZero (N 4) (N 2) ~ P 2)
_test_Div_RoundZero_N4_N2 =  Dict
_test_Rem_RoundZero_N4_N2 :: Dict (K.Rem 'K.RoundZero (N 4) (N 2) ~ P 0)
_test_Rem_RoundZero_N4_N2 =  Dict
_test_Div_RoundZero_N4_N3 :: Dict (K.Div 'K.RoundZero (N 4) (N 3) ~ P 1)
_test_Div_RoundZero_N4_N3 =  Dict
_test_Rem_RoundZero_N4_N3 :: Dict (K.Rem 'K.RoundZero (N 4) (N 3) ~ N 1)
_test_Rem_RoundZero_N4_N3 =  Dict
_test_Div_RoundZero_N4_N4 :: Dict (K.Div 'K.RoundZero (N 4) (N 4) ~ P 1)
_test_Div_RoundZero_N4_N4 =  Dict
_test_Rem_RoundZero_N4_N4 :: Dict (K.Rem 'K.RoundZero (N 4) (N 4) ~ P 0)
_test_Rem_RoundZero_N4_N4 =  Dict
_test_Div_RoundZero_N4_P1 :: Dict (K.Div 'K.RoundZero (N 4) (P 1) ~ N 4)
_test_Div_RoundZero_N4_P1 =  Dict
_test_Rem_RoundZero_N4_P1 :: Dict (K.Rem 'K.RoundZero (N 4) (P 1) ~ P 0)
_test_Rem_RoundZero_N4_P1 =  Dict
_test_Div_RoundZero_N4_P2 :: Dict (K.Div 'K.RoundZero (N 4) (P 2) ~ N 2)
_test_Div_RoundZero_N4_P2 =  Dict
_test_Rem_RoundZero_N4_P2 :: Dict (K.Rem 'K.RoundZero (N 4) (P 2) ~ P 0)
_test_Rem_RoundZero_N4_P2 =  Dict
_test_Div_RoundZero_N4_P3 :: Dict (K.Div 'K.RoundZero (N 4) (P 3) ~ N 1)
_test_Div_RoundZero_N4_P3 =  Dict
_test_Rem_RoundZero_N4_P3 :: Dict (K.Rem 'K.RoundZero (N 4) (P 3) ~ N 1)
_test_Rem_RoundZero_N4_P3 =  Dict
_test_Div_RoundZero_N4_P4 :: Dict (K.Div 'K.RoundZero (N 4) (P 4) ~ N 1)
_test_Div_RoundZero_N4_P4 =  Dict
_test_Rem_RoundZero_N4_P4 :: Dict (K.Rem 'K.RoundZero (N 4) (P 4) ~ P 0)
_test_Rem_RoundZero_N4_P4 =  Dict
_test_Div_RoundZero_P0_N1 :: Dict (K.Div 'K.RoundZero (P 0) (N 1) ~ P 0)
_test_Div_RoundZero_P0_N1 =  Dict
_test_Rem_RoundZero_P0_N1 :: Dict (K.Rem 'K.RoundZero (P 0) (N 1) ~ P 0)
_test_Rem_RoundZero_P0_N1 =  Dict
_test_Div_RoundZero_P0_N2 :: Dict (K.Div 'K.RoundZero (P 0) (N 2) ~ P 0)
_test_Div_RoundZero_P0_N2 =  Dict
_test_Rem_RoundZero_P0_N2 :: Dict (K.Rem 'K.RoundZero (P 0) (N 2) ~ P 0)
_test_Rem_RoundZero_P0_N2 =  Dict
_test_Div_RoundZero_P0_N3 :: Dict (K.Div 'K.RoundZero (P 0) (N 3) ~ P 0)
_test_Div_RoundZero_P0_N3 =  Dict
_test_Rem_RoundZero_P0_N3 :: Dict (K.Rem 'K.RoundZero (P 0) (N 3) ~ P 0)
_test_Rem_RoundZero_P0_N3 =  Dict
_test_Div_RoundZero_P0_N4 :: Dict (K.Div 'K.RoundZero (P 0) (N 4) ~ P 0)
_test_Div_RoundZero_P0_N4 =  Dict
_test_Rem_RoundZero_P0_N4 :: Dict (K.Rem 'K.RoundZero (P 0) (N 4) ~ P 0)
_test_Rem_RoundZero_P0_N4 =  Dict
_test_Div_RoundZero_P0_P1 :: Dict (K.Div 'K.RoundZero (P 0) (P 1) ~ P 0)
_test_Div_RoundZero_P0_P1 =  Dict
_test_Rem_RoundZero_P0_P1 :: Dict (K.Rem 'K.RoundZero (P 0) (P 1) ~ P 0)
_test_Rem_RoundZero_P0_P1 =  Dict
_test_Div_RoundZero_P0_P2 :: Dict (K.Div 'K.RoundZero (P 0) (P 2) ~ P 0)
_test_Div_RoundZero_P0_P2 =  Dict
_test_Rem_RoundZero_P0_P2 :: Dict (K.Rem 'K.RoundZero (P 0) (P 2) ~ P 0)
_test_Rem_RoundZero_P0_P2 =  Dict
_test_Div_RoundZero_P0_P3 :: Dict (K.Div 'K.RoundZero (P 0) (P 3) ~ P 0)
_test_Div_RoundZero_P0_P3 =  Dict
_test_Rem_RoundZero_P0_P3 :: Dict (K.Rem 'K.RoundZero (P 0) (P 3) ~ P 0)
_test_Rem_RoundZero_P0_P3 =  Dict
_test_Div_RoundZero_P0_P4 :: Dict (K.Div 'K.RoundZero (P 0) (P 4) ~ P 0)
_test_Div_RoundZero_P0_P4 =  Dict
_test_Rem_RoundZero_P0_P4 :: Dict (K.Rem 'K.RoundZero (P 0) (P 4) ~ P 0)
_test_Rem_RoundZero_P0_P4 =  Dict
_test_Div_RoundZero_P1_N1 :: Dict (K.Div 'K.RoundZero (P 1) (N 1) ~ N 1)
_test_Div_RoundZero_P1_N1 =  Dict
_test_Rem_RoundZero_P1_N1 :: Dict (K.Rem 'K.RoundZero (P 1) (N 1) ~ P 0)
_test_Rem_RoundZero_P1_N1 =  Dict
_test_Div_RoundZero_P1_N2 :: Dict (K.Div 'K.RoundZero (P 1) (N 2) ~ P 0)
_test_Div_RoundZero_P1_N2 =  Dict
_test_Rem_RoundZero_P1_N2 :: Dict (K.Rem 'K.RoundZero (P 1) (N 2) ~ P 1)
_test_Rem_RoundZero_P1_N2 =  Dict
_test_Div_RoundZero_P1_N3 :: Dict (K.Div 'K.RoundZero (P 1) (N 3) ~ P 0)
_test_Div_RoundZero_P1_N3 =  Dict
_test_Rem_RoundZero_P1_N3 :: Dict (K.Rem 'K.RoundZero (P 1) (N 3) ~ P 1)
_test_Rem_RoundZero_P1_N3 =  Dict
_test_Div_RoundZero_P1_N4 :: Dict (K.Div 'K.RoundZero (P 1) (N 4) ~ P 0)
_test_Div_RoundZero_P1_N4 =  Dict
_test_Rem_RoundZero_P1_N4 :: Dict (K.Rem 'K.RoundZero (P 1) (N 4) ~ P 1)
_test_Rem_RoundZero_P1_N4 =  Dict
_test_Div_RoundZero_P1_P1 :: Dict (K.Div 'K.RoundZero (P 1) (P 1) ~ P 1)
_test_Div_RoundZero_P1_P1 =  Dict
_test_Rem_RoundZero_P1_P1 :: Dict (K.Rem 'K.RoundZero (P 1) (P 1) ~ P 0)
_test_Rem_RoundZero_P1_P1 =  Dict
_test_Div_RoundZero_P1_P2 :: Dict (K.Div 'K.RoundZero (P 1) (P 2) ~ P 0)
_test_Div_RoundZero_P1_P2 =  Dict
_test_Rem_RoundZero_P1_P2 :: Dict (K.Rem 'K.RoundZero (P 1) (P 2) ~ P 1)
_test_Rem_RoundZero_P1_P2 =  Dict
_test_Div_RoundZero_P1_P3 :: Dict (K.Div 'K.RoundZero (P 1) (P 3) ~ P 0)
_test_Div_RoundZero_P1_P3 =  Dict
_test_Rem_RoundZero_P1_P3 :: Dict (K.Rem 'K.RoundZero (P 1) (P 3) ~ P 1)
_test_Rem_RoundZero_P1_P3 =  Dict
_test_Div_RoundZero_P1_P4 :: Dict (K.Div 'K.RoundZero (P 1) (P 4) ~ P 0)
_test_Div_RoundZero_P1_P4 =  Dict
_test_Rem_RoundZero_P1_P4 :: Dict (K.Rem 'K.RoundZero (P 1) (P 4) ~ P 1)
_test_Rem_RoundZero_P1_P4 =  Dict
_test_Div_RoundZero_P2_N1 :: Dict (K.Div 'K.RoundZero (P 2) (N 1) ~ N 2)
_test_Div_RoundZero_P2_N1 =  Dict
_test_Rem_RoundZero_P2_N1 :: Dict (K.Rem 'K.RoundZero (P 2) (N 1) ~ P 0)
_test_Rem_RoundZero_P2_N1 =  Dict
_test_Div_RoundZero_P2_N2 :: Dict (K.Div 'K.RoundZero (P 2) (N 2) ~ N 1)
_test_Div_RoundZero_P2_N2 =  Dict
_test_Rem_RoundZero_P2_N2 :: Dict (K.Rem 'K.RoundZero (P 2) (N 2) ~ P 0)
_test_Rem_RoundZero_P2_N2 =  Dict
_test_Div_RoundZero_P2_N3 :: Dict (K.Div 'K.RoundZero (P 2) (N 3) ~ P 0)
_test_Div_RoundZero_P2_N3 =  Dict
_test_Rem_RoundZero_P2_N3 :: Dict (K.Rem 'K.RoundZero (P 2) (N 3) ~ P 2)
_test_Rem_RoundZero_P2_N3 =  Dict
_test_Div_RoundZero_P2_N4 :: Dict (K.Div 'K.RoundZero (P 2) (N 4) ~ P 0)
_test_Div_RoundZero_P2_N4 =  Dict
_test_Rem_RoundZero_P2_N4 :: Dict (K.Rem 'K.RoundZero (P 2) (N 4) ~ P 2)
_test_Rem_RoundZero_P2_N4 =  Dict
_test_Div_RoundZero_P2_P1 :: Dict (K.Div 'K.RoundZero (P 2) (P 1) ~ P 2)
_test_Div_RoundZero_P2_P1 =  Dict
_test_Rem_RoundZero_P2_P1 :: Dict (K.Rem 'K.RoundZero (P 2) (P 1) ~ P 0)
_test_Rem_RoundZero_P2_P1 =  Dict
_test_Div_RoundZero_P2_P2 :: Dict (K.Div 'K.RoundZero (P 2) (P 2) ~ P 1)
_test_Div_RoundZero_P2_P2 =  Dict
_test_Rem_RoundZero_P2_P2 :: Dict (K.Rem 'K.RoundZero (P 2) (P 2) ~ P 0)
_test_Rem_RoundZero_P2_P2 =  Dict
_test_Div_RoundZero_P2_P3 :: Dict (K.Div 'K.RoundZero (P 2) (P 3) ~ P 0)
_test_Div_RoundZero_P2_P3 =  Dict
_test_Rem_RoundZero_P2_P3 :: Dict (K.Rem 'K.RoundZero (P 2) (P 3) ~ P 2)
_test_Rem_RoundZero_P2_P3 =  Dict
_test_Div_RoundZero_P2_P4 :: Dict (K.Div 'K.RoundZero (P 2) (P 4) ~ P 0)
_test_Div_RoundZero_P2_P4 =  Dict
_test_Rem_RoundZero_P2_P4 :: Dict (K.Rem 'K.RoundZero (P 2) (P 4) ~ P 2)
_test_Rem_RoundZero_P2_P4 =  Dict
_test_Div_RoundZero_P3_N1 :: Dict (K.Div 'K.RoundZero (P 3) (N 1) ~ N 3)
_test_Div_RoundZero_P3_N1 =  Dict
_test_Rem_RoundZero_P3_N1 :: Dict (K.Rem 'K.RoundZero (P 3) (N 1) ~ P 0)
_test_Rem_RoundZero_P3_N1 =  Dict
_test_Div_RoundZero_P3_N2 :: Dict (K.Div 'K.RoundZero (P 3) (N 2) ~ N 1)
_test_Div_RoundZero_P3_N2 =  Dict
_test_Rem_RoundZero_P3_N2 :: Dict (K.Rem 'K.RoundZero (P 3) (N 2) ~ P 1)
_test_Rem_RoundZero_P3_N2 =  Dict
_test_Div_RoundZero_P3_N3 :: Dict (K.Div 'K.RoundZero (P 3) (N 3) ~ N 1)
_test_Div_RoundZero_P3_N3 =  Dict
_test_Rem_RoundZero_P3_N3 :: Dict (K.Rem 'K.RoundZero (P 3) (N 3) ~ P 0)
_test_Rem_RoundZero_P3_N3 =  Dict
_test_Div_RoundZero_P3_N4 :: Dict (K.Div 'K.RoundZero (P 3) (N 4) ~ P 0)
_test_Div_RoundZero_P3_N4 =  Dict
_test_Rem_RoundZero_P3_N4 :: Dict (K.Rem 'K.RoundZero (P 3) (N 4) ~ P 3)
_test_Rem_RoundZero_P3_N4 =  Dict
_test_Div_RoundZero_P3_P1 :: Dict (K.Div 'K.RoundZero (P 3) (P 1) ~ P 3)
_test_Div_RoundZero_P3_P1 =  Dict
_test_Rem_RoundZero_P3_P1 :: Dict (K.Rem 'K.RoundZero (P 3) (P 1) ~ P 0)
_test_Rem_RoundZero_P3_P1 =  Dict
_test_Div_RoundZero_P3_P2 :: Dict (K.Div 'K.RoundZero (P 3) (P 2) ~ P 1)
_test_Div_RoundZero_P3_P2 =  Dict
_test_Rem_RoundZero_P3_P2 :: Dict (K.Rem 'K.RoundZero (P 3) (P 2) ~ P 1)
_test_Rem_RoundZero_P3_P2 =  Dict
_test_Div_RoundZero_P3_P3 :: Dict (K.Div 'K.RoundZero (P 3) (P 3) ~ P 1)
_test_Div_RoundZero_P3_P3 =  Dict
_test_Rem_RoundZero_P3_P3 :: Dict (K.Rem 'K.RoundZero (P 3) (P 3) ~ P 0)
_test_Rem_RoundZero_P3_P3 =  Dict
_test_Div_RoundZero_P3_P4 :: Dict (K.Div 'K.RoundZero (P 3) (P 4) ~ P 0)
_test_Div_RoundZero_P3_P4 =  Dict
_test_Rem_RoundZero_P3_P4 :: Dict (K.Rem 'K.RoundZero (P 3) (P 4) ~ P 3)
_test_Rem_RoundZero_P3_P4 =  Dict
_test_Div_RoundZero_P4_N1 :: Dict (K.Div 'K.RoundZero (P 4) (N 1) ~ N 4)
_test_Div_RoundZero_P4_N1 =  Dict
_test_Rem_RoundZero_P4_N1 :: Dict (K.Rem 'K.RoundZero (P 4) (N 1) ~ P 0)
_test_Rem_RoundZero_P4_N1 =  Dict
_test_Div_RoundZero_P4_N2 :: Dict (K.Div 'K.RoundZero (P 4) (N 2) ~ N 2)
_test_Div_RoundZero_P4_N2 =  Dict
_test_Rem_RoundZero_P4_N2 :: Dict (K.Rem 'K.RoundZero (P 4) (N 2) ~ P 0)
_test_Rem_RoundZero_P4_N2 =  Dict
_test_Div_RoundZero_P4_N3 :: Dict (K.Div 'K.RoundZero (P 4) (N 3) ~ N 1)
_test_Div_RoundZero_P4_N3 =  Dict
_test_Rem_RoundZero_P4_N3 :: Dict (K.Rem 'K.RoundZero (P 4) (N 3) ~ P 1)
_test_Rem_RoundZero_P4_N3 =  Dict
_test_Div_RoundZero_P4_N4 :: Dict (K.Div 'K.RoundZero (P 4) (N 4) ~ N 1)
_test_Div_RoundZero_P4_N4 =  Dict
_test_Rem_RoundZero_P4_N4 :: Dict (K.Rem 'K.RoundZero (P 4) (N 4) ~ P 0)
_test_Rem_RoundZero_P4_N4 =  Dict
_test_Div_RoundZero_P4_P1 :: Dict (K.Div 'K.RoundZero (P 4) (P 1) ~ P 4)
_test_Div_RoundZero_P4_P1 =  Dict
_test_Rem_RoundZero_P4_P1 :: Dict (K.Rem 'K.RoundZero (P 4) (P 1) ~ P 0)
_test_Rem_RoundZero_P4_P1 =  Dict
_test_Div_RoundZero_P4_P2 :: Dict (K.Div 'K.RoundZero (P 4) (P 2) ~ P 2)
_test_Div_RoundZero_P4_P2 =  Dict
_test_Rem_RoundZero_P4_P2 :: Dict (K.Rem 'K.RoundZero (P 4) (P 2) ~ P 0)
_test_Rem_RoundZero_P4_P2 =  Dict
_test_Div_RoundZero_P4_P3 :: Dict (K.Div 'K.RoundZero (P 4) (P 3) ~ P 1)
_test_Div_RoundZero_P4_P3 =  Dict
_test_Rem_RoundZero_P4_P3 :: Dict (K.Rem 'K.RoundZero (P 4) (P 3) ~ P 1)
_test_Rem_RoundZero_P4_P3 =  Dict
_test_Div_RoundZero_P4_P4 :: Dict (K.Div 'K.RoundZero (P 4) (P 4) ~ P 1)
_test_Div_RoundZero_P4_P4 =  Dict
_test_Rem_RoundZero_P4_P4 :: Dict (K.Rem 'K.RoundZero (P 4) (P 4) ~ P 0)
_test_Rem_RoundZero_P4_P4 =  Dict
