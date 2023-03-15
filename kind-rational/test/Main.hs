{-# LANGUAGE MagicHash #-}
module Main (main) where

import Control.Applicative
import Data.Maybe
import Data.Proxy
import Data.Type.Ord (type (<=))
import GHC.Exts (Constraint, proxy#)
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
      let str = show (i :: Integer)
      in readMaybe @Integer str
            == fmap (\(K.SomeInteger p) -> K.integerVal p)
                    (readMaybe @K.SomeInteger str)

  ]


