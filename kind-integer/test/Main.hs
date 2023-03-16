{-# LANGUAGE MagicHash #-}
module Main (main) where

import Control.Applicative
import Data.Maybe
import Data.Proxy
import Data.Type.Ord (type (<=))
import GHC.Exts (Constraint, proxy#)
import System.Exit
import Text.Read
import Prelude hiding (Integer)
import Prelude qualified as P

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

_testDiv  = Dict
_testDiv :: Dict
  ( P 0 ~ P 0 `K.Div` P 1
  , P 0 ~ N 0 `K.Div` N 1
  , P 0 ~ P 0 `K.Div` N 1
  , P 0 ~ N 0 `K.Div` P 1

  , P 1 ~ P 1 `K.Div` P 1
  , P 1 ~ N 1 `K.Div` N 1
  , N 1 ~ P 1 `K.Div` N 1
  , N 1 ~ N 1 `K.Div` P 1

  , P 2 ~ P 2 `K.Div` P 1
  , P 2 ~ N 2 `K.Div` N 1
  , N 2 ~ P 2 `K.Div` N 1
  , N 2 ~ N 2 `K.Div` P 1

  , P 1 ~ P 2 `K.Div` P 2
  , P 1 ~ N 2 `K.Div` N 2
  , N 1 ~ P 2 `K.Div` N 2
  , N 1 ~ N 2 `K.Div` P 2

  , P 1 ~ P 3 `K.Div` P 2
  , P 1 ~ N 3 `K.Div` N 2
  , N 2 ~ P 3 `K.Div` N 2
  , N 2 ~ N 3 `K.Div` P 2

  , P 0 ~ P 0 `K.Div` P 1
  , P 0 ~ N 0 `K.Div` N 1
  , P 0 ~ P 0 `K.Div` N 1
  , P 0 ~ N 0 `K.Div` P 1

  , P 0 ~ P 1 `K.Div` P 2
  , P 0 ~ N 1 `K.Div` N 2
  , N 1 ~ P 1 `K.Div` N 2
  , N 1 ~ N 1 `K.Div` P 2
  )

_testMod  = Dict
_testMod :: Dict
  ( P 0 ~ P 0 `K.Mod` P 1
  , P 0 ~ N 0 `K.Mod` N 1
  , P 0 ~ P 0 `K.Mod` N 1
  , P 0 ~ N 0 `K.Mod` P 1

  , P 0 ~ P 1 `K.Mod` P 1
  , P 0 ~ N 1 `K.Mod` N 1
  , P 0 ~ P 1 `K.Mod` N 1
  , P 0 ~ N 1 `K.Mod` P 1

  , P 0 ~ P 2 `K.Mod` P 1
  , P 0 ~ N 2 `K.Mod` N 1
  , P 0 ~ P 2 `K.Mod` N 1
  , P 0 ~ N 2 `K.Mod` P 1

  , P 0 ~ P 2 `K.Mod` P 2
  , P 0 ~ N 2 `K.Mod` N 2
  , P 0 ~ P 2 `K.Mod` N 2
  , P 0 ~ N 2 `K.Mod` P 2

  , P 1 ~ P 3 `K.Mod` P 2
  , N 1 ~ N 3 `K.Mod` N 2
  , N 1 ~ P 3 `K.Mod` N 2
  , P 1 ~ N 3 `K.Mod` P 2

  , P 0 ~ P 0 `K.Mod` P 1
  , P 0 ~ N 0 `K.Mod` N 1
  , P 0 ~ P 0 `K.Mod` N 1
  , P 0 ~ N 0 `K.Mod` P 1

  , P 1 ~ P 1 `K.Mod` P 2
  , N 1 ~ N 1 `K.Mod` N 2
  , N 1 ~ P 1 `K.Mod` N 2
  , P 1 ~ N 1 `K.Mod` P 2
  )

_testQuot  = Dict
_testQuot :: Dict
  ( P 0 ~ P 0 `K.Quot` P 1
  , P 0 ~ N 0 `K.Quot` N 1
  , P 0 ~ P 0 `K.Quot` N 1
  , P 0 ~ N 0 `K.Quot` P 1

  , P 1 ~ P 1 `K.Quot` P 1
  , P 1 ~ N 1 `K.Quot` N 1
  , N 1 ~ P 1 `K.Quot` N 1
  , N 1 ~ N 1 `K.Quot` P 1

  , P 2 ~ P 2 `K.Quot` P 1
  , P 2 ~ N 2 `K.Quot` N 1
  , N 2 ~ P 2 `K.Quot` N 1
  , N 2 ~ N 2 `K.Quot` P 1

  , P 1 ~ P 2 `K.Quot` P 2
  , P 1 ~ N 2 `K.Quot` N 2
  , N 1 ~ P 2 `K.Quot` N 2
  , N 1 ~ N 2 `K.Quot` P 2

  , P 1 ~ P 3 `K.Quot` P 2
  , P 1 ~ N 3 `K.Quot` N 2
  , N 1 ~ P 3 `K.Quot` N 2
  , N 1 ~ N 3 `K.Quot` P 2

  , P 0 ~ P 0 `K.Quot` P 1
  , P 0 ~ N 0 `K.Quot` N 1
  , P 0 ~ P 0 `K.Quot` N 1
  , P 0 ~ N 0 `K.Quot` P 1

  , P 0 ~ P 1 `K.Quot` P 2
  , P 0 ~ N 1 `K.Quot` N 2
  , P 0 ~ P 1 `K.Quot` N 2
  , P 0 ~ N 1 `K.Quot` P 2
  )

_testRem  = Dict
_testRem :: Dict
  ( P 0 ~ P 0 `K.Rem` P 1
  , P 0 ~ N 0 `K.Rem` N 1
  , P 0 ~ P 0 `K.Rem` N 1
  , P 0 ~ N 0 `K.Rem` P 1

  , P 0 ~ P 1 `K.Rem` P 1
  , P 0 ~ N 1 `K.Rem` N 1
  , P 0 ~ P 1 `K.Rem` N 1
  , P 0 ~ N 1 `K.Rem` P 1

  , P 0 ~ P 2 `K.Rem` P 1
  , P 0 ~ N 2 `K.Rem` N 1
  , P 0 ~ P 2 `K.Rem` N 1
  , P 0 ~ N 2 `K.Rem` P 1

  , P 0 ~ P 2 `K.Rem` P 2
  , P 0 ~ N 2 `K.Rem` N 2
  , P 0 ~ P 2 `K.Rem` N 2
  , P 0 ~ N 2 `K.Rem` P 2

  , P 1 ~ P 3 `K.Rem` P 2
  , N 1 ~ N 3 `K.Rem` N 2
  , P 1 ~ P 3 `K.Rem` N 2
  , N 1 ~ N 3 `K.Rem` P 2

  , P 0 ~ P 0 `K.Rem` P 1
  , P 0 ~ N 0 `K.Rem` N 1
  , P 0 ~ P 0 `K.Rem` N 1
  , P 0 ~ N 0 `K.Rem` P 1

  , P 1 ~ P 1 `K.Rem` P 2
  , N 1 ~ N 1 `K.Rem` N 2
  , P 1 ~ P 1 `K.Rem` N 2
  , N 1 ~ N 1 `K.Rem` P 2
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

_testAbs =  Dict
_testAbs :: Dict
  ( 0 ~ K.Abs (P 0)
  , 0 ~ K.Abs (P 0)
  , 1 ~ K.Abs (P 1)
  , 1 ~ K.Abs (N 1)
  , 2 ~ K.Abs (P 2)
  , 2 ~ K.Abs (N 2)
  )

_testEven =  Dict
_testEven :: Dict
  ( 'True  ~ K.Even (P 0)
  , 'True  ~ K.Even (P 0)
  , 'False ~ K.Even (P 1)
  , 'False ~ K.Even (N 1)
  , 'True  ~ K.Even (P 2)
  , 'True  ~ K.Even (N 2)
  )

_testOdd =  Dict
_testOdd :: Dict
  ( 'False  ~ K.Odd (P 0)
  , 'False  ~ K.Odd (P 0)
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
  oks <- sequence xs
  if and oks
     then do putStrLn "All tests passed successfully."
             exitSuccess
     else do putStrLn "Some tests failed."
             exitFailure

main :: IO ()
main = testsMain
  [ assert "integerVal . someIntegerVal == id" $
    flip all [-5 .. 5] $ \a ->
      case K.someIntegerVal a of
        K.SomeInteger pa ->
          a == K.integerVal pa

  , assert "integerVal' . someIntegerVal == id" $
    flip all [-5 .. 5] $ \a ->
      case K.someIntegerVal a of
        K.SomeInteger (_ :: Proxy a) ->
          a == K.integerVal' (proxy# @a)

  , assert "sameIntegerVal a a" $
    flip all [-5 .. 5] $ \a ->
      case K.someIntegerVal a of
        K.SomeInteger pa ->
          isJust (K.sameInteger pa pa)

  , assert "sameIntegerVal a a'" $
    flip all [-5 .. 5] $ \a ->
      case (K.someIntegerVal a, K.someIntegerVal a) of
        (K.SomeInteger pa1, K.SomeInteger pa2) ->
          isJust (K.sameInteger pa1 pa2)

  , assert "sameIntegerVal a b" $
    flip all (liftA2 (,) [-5 .. 5] [-5 .. 5])$ \(a, b) ->
      case (K.someIntegerVal a, K.someIntegerVal b) of
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
      (a == b) == (K.someIntegerVal a == K.someIntegerVal b)

  , assert "Ord SomeInteger" $
    flip all (liftA2 (,) [-5 .. 5] [-5 .. 5])$ \(a, b) ->
      (a `compare` b) == (K.someIntegerVal a `compare` K.someIntegerVal b)

  , assert "Show SomeInteger" $
    flip all [-5 .. 5] $ \i ->
      show i == show (K.someIntegerVal i)

  , assert "Read SomeInteger" $
    flip all [-5 .. 5] $ \i ->
      let str = show (i :: P.Integer)
      in readMaybe @P.Integer str
            == fmap (\(K.SomeInteger p) -> K.integerVal p)
                    (readMaybe @K.SomeInteger str)

  ]


