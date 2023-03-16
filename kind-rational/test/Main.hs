{-# LANGUAGE MagicHash #-}
module Main (main) where

import Control.Applicative
import Control.Monad
import Data.Maybe
import Data.Proxy
import Data.Type.Ord (type (<=))
import GHC.Exts (Constraint, proxy#)
import System.Exit
import Text.Read
import GHC.Real qualified as P
import Prelude hiding (Rational, Integer)
import Prelude qualified as P

import KindInteger (P, N)
import KindInteger qualified as I
import KindRational qualified as K

--------------------------------------------------------------------------------

data Dict (c :: Constraint) where
  Dict :: c => Dict c

--------------------------------------------------------------------------------

-- _testEq =  Dict
-- _testEq :: Dict
--   ( P 0 K.== P 0,   'True ~ (P 0 K.==? P 0)
--   , N 0 K.== N 0,   'True ~ (N 0 K.==? N 0)
--   , P 0 K.== N 0,   'True ~ (P 0 K.==? N 0)
--   , N 0 K.== P 0,   'True ~ (N 0 K.==? P 0)
--
--   , P 0 K./= P 1,   'True ~ (P 0 K./=? P 1)
--   , P 0 K./= N 1,   'True ~ (P 0 K./=? N 1)
--
--   , N 0 K./= N 1,   'True ~ (N 0 K./=? N 1)
--   , N 0 K./= N 1,   'True ~ (N 0 K./=? N 1)
--
--   , P 1 K./= P 0,   'True ~ (P 1 K./=? P 0)
--   , P 1 K./= N 0,   'True ~ (P 1 K./=? N 0)
--
--   , N 1 K./= N 0,   'True ~ (N 1 K./=? N 0)
--   , N 1 K./= N 0,   'True ~ (N 1 K./=? N 0)
--   )
--
-- _testCmp =  Dict
-- _testCmp :: Dict
--   ( P 0 <= P 0
--   , P 0 <= N 0
--   , N 0 <= P 0
--   , N 0 <= N 0
--
--   , N 2 <= N 1
--   , N 1 <= N 0
--   , N 0 <= P 1
--
--   , P 0 <= P 1
--   , P 1 <= P 2
--   )
--
-- _testAdd  = Dict
-- _testAdd :: Dict
--   ( P 0 ~ P 0 K.+ P 0
--   , P 0 ~ N 0 K.+ N 0
--   , P 0 ~ P 0 K.+ N 0
--   , P 0 ~ N 0 K.+ P 0
--
--   , P 1 ~ P 1 K.+ P 0
--   , N 1 ~ N 1 K.+ N 0
--   , P 1 ~ P 1 K.+ N 0
--   , N 1 ~ N 1 K.+ P 0
--
--   , P 1 ~ P 0 K.+ P 1
--   , N 1 ~ N 0 K.+ N 1
--   , N 1 ~ P 0 K.+ N 1
--   , P 1 ~ N 0 K.+ P 1
--
--   , P 2 ~ P 1 K.+ P 1
--   , N 2 ~ N 1 K.+ N 1
--   , P 0 ~ P 1 K.+ N 1
--   , P 0 ~ N 1 K.+ P 1
--   )
--
-- _testMul  = Dict
-- _testMul :: Dict
--   ( P 0 ~ P 0 K.* P 0
--   , P 0 ~ N 0 K.* N 0
--   , P 0 ~ P 0 K.* N 0
--   , P 0 ~ N 0 K.* P 0
--
--   , P 0 ~ P 1 K.* P 0
--   , P 0 ~ N 1 K.* N 0
--   , P 0 ~ P 1 K.* N 0
--   , P 0 ~ N 1 K.* P 0
--
--   , P 0 ~ P 0 K.* P 1
--   , P 0 ~ N 0 K.* N 1
--   , P 0 ~ P 0 K.* N 1
--   , P 0 ~ N 0 K.* P 1
--
--   , P 1 ~ P 1 K.* P 1
--   , P 1 ~ N 1 K.* N 1
--   , N 1 ~ P 1 K.* N 1
--   , N 1 ~ N 1 K.* P 1
--
--   , P 2 ~ P 2 K.* P 1
--   , P 2 ~ N 2 K.* N 1
--   , N 2 ~ P 2 K.* N 1
--   , N 2 ~ N 2 K.* P 1
--
--   , P 6 ~ P 2 K.* P 3
--   , P 6 ~ N 2 K.* N 3
--   , N 6 ~ P 2 K.* N 3
--   , N 6 ~ N 2 K.* P 3
--   )
--
-- _testDiv  = Dict
-- _testDiv :: Dict
--   ( P 0 ~ P 0 `K.Div` P 1
--   , P 0 ~ N 0 `K.Div` N 1
--   , P 0 ~ P 0 `K.Div` N 1
--   , P 0 ~ N 0 `K.Div` P 1
--
--   , P 1 ~ P 1 `K.Div` P 1
--   , P 1 ~ N 1 `K.Div` N 1
--   , N 1 ~ P 1 `K.Div` N 1
--   , N 1 ~ N 1 `K.Div` P 1
--
--   , P 2 ~ P 2 `K.Div` P 1
--   , P 2 ~ N 2 `K.Div` N 1
--   , N 2 ~ P 2 `K.Div` N 1
--   , N 2 ~ N 2 `K.Div` P 1
--
--   , P 1 ~ P 2 `K.Div` P 2
--   , P 1 ~ N 2 `K.Div` N 2
--   , N 1 ~ P 2 `K.Div` N 2
--   , N 1 ~ N 2 `K.Div` P 2
--
--   , P 1 ~ P 3 `K.Div` P 2
--   , P 1 ~ N 3 `K.Div` N 2
--   , N 2 ~ P 3 `K.Div` N 2
--   , N 2 ~ N 3 `K.Div` P 2
--
--   , P 0 ~ P 0 `K.Div` P 1
--   , P 0 ~ N 0 `K.Div` N 1
--   , P 0 ~ P 0 `K.Div` N 1
--   , P 0 ~ N 0 `K.Div` P 1
--
--   , P 0 ~ P 1 `K.Div` P 2
--   , P 0 ~ N 1 `K.Div` N 2
--   , N 1 ~ P 1 `K.Div` N 2
--   , N 1 ~ N 1 `K.Div` P 2
--   )
--
-- _testMod  = Dict
-- _testMod :: Dict
--   ( P 0 ~ P 0 `K.Mod` P 1
--   , P 0 ~ N 0 `K.Mod` N 1
--   , P 0 ~ P 0 `K.Mod` N 1
--   , P 0 ~ N 0 `K.Mod` P 1
--
--   , P 0 ~ P 1 `K.Mod` P 1
--   , P 0 ~ N 1 `K.Mod` N 1
--   , P 0 ~ P 1 `K.Mod` N 1
--   , P 0 ~ N 1 `K.Mod` P 1
--
--   , P 0 ~ P 2 `K.Mod` P 1
--   , P 0 ~ N 2 `K.Mod` N 1
--   , P 0 ~ P 2 `K.Mod` N 1
--   , P 0 ~ N 2 `K.Mod` P 1
--
--   , P 0 ~ P 2 `K.Mod` P 2
--   , P 0 ~ N 2 `K.Mod` N 2
--   , P 0 ~ P 2 `K.Mod` N 2
--   , P 0 ~ N 2 `K.Mod` P 2
--
--   , P 1 ~ P 3 `K.Mod` P 2
--   , N 1 ~ N 3 `K.Mod` N 2
--   , N 1 ~ P 3 `K.Mod` N 2
--   , P 1 ~ N 3 `K.Mod` P 2
--
--   , P 0 ~ P 0 `K.Mod` P 1
--   , P 0 ~ N 0 `K.Mod` N 1
--   , P 0 ~ P 0 `K.Mod` N 1
--   , P 0 ~ N 0 `K.Mod` P 1
--
--   , P 1 ~ P 1 `K.Mod` P 2
--   , N 1 ~ N 1 `K.Mod` N 2
--   , N 1 ~ P 1 `K.Mod` N 2
--   , P 1 ~ N 1 `K.Mod` P 2
--   )
--
-- _testQuot  = Dict
-- _testQuot :: Dict
--   ( P 0 ~ P 0 `K.Quot` P 1
--   , P 0 ~ N 0 `K.Quot` N 1
--   , P 0 ~ P 0 `K.Quot` N 1
--   , P 0 ~ N 0 `K.Quot` P 1
--
--   , P 1 ~ P 1 `K.Quot` P 1
--   , P 1 ~ N 1 `K.Quot` N 1
--   , N 1 ~ P 1 `K.Quot` N 1
--   , N 1 ~ N 1 `K.Quot` P 1
--
--   , P 2 ~ P 2 `K.Quot` P 1
--   , P 2 ~ N 2 `K.Quot` N 1
--   , N 2 ~ P 2 `K.Quot` N 1
--   , N 2 ~ N 2 `K.Quot` P 1
--
--   , P 1 ~ P 2 `K.Quot` P 2
--   , P 1 ~ N 2 `K.Quot` N 2
--   , N 1 ~ P 2 `K.Quot` N 2
--   , N 1 ~ N 2 `K.Quot` P 2
--
--   , P 1 ~ P 3 `K.Quot` P 2
--   , P 1 ~ N 3 `K.Quot` N 2
--   , N 1 ~ P 3 `K.Quot` N 2
--   , N 1 ~ N 3 `K.Quot` P 2
--
--   , P 0 ~ P 0 `K.Quot` P 1
--   , P 0 ~ N 0 `K.Quot` N 1
--   , P 0 ~ P 0 `K.Quot` N 1
--   , P 0 ~ N 0 `K.Quot` P 1
--
--   , P 0 ~ P 1 `K.Quot` P 2
--   , P 0 ~ N 1 `K.Quot` N 2
--   , P 0 ~ P 1 `K.Quot` N 2
--   , P 0 ~ N 1 `K.Quot` P 2
--   )
--
-- _testRem  = Dict
-- _testRem :: Dict
--   ( P 0 ~ P 0 `K.Rem` P 1
--   , P 0 ~ N 0 `K.Rem` N 1
--   , P 0 ~ P 0 `K.Rem` N 1
--   , P 0 ~ N 0 `K.Rem` P 1
--
--   , P 0 ~ P 1 `K.Rem` P 1
--   , P 0 ~ N 1 `K.Rem` N 1
--   , P 0 ~ P 1 `K.Rem` N 1
--   , P 0 ~ N 1 `K.Rem` P 1
--
--   , P 0 ~ P 2 `K.Rem` P 1
--   , P 0 ~ N 2 `K.Rem` N 1
--   , P 0 ~ P 2 `K.Rem` N 1
--   , P 0 ~ N 2 `K.Rem` P 1
--
--   , P 0 ~ P 2 `K.Rem` P 2
--   , P 0 ~ N 2 `K.Rem` N 2
--   , P 0 ~ P 2 `K.Rem` N 2
--   , P 0 ~ N 2 `K.Rem` P 2
--
--   , P 1 ~ P 3 `K.Rem` P 2
--   , N 1 ~ N 3 `K.Rem` N 2
--   , P 1 ~ P 3 `K.Rem` N 2
--   , N 1 ~ N 3 `K.Rem` P 2
--
--   , P 0 ~ P 0 `K.Rem` P 1
--   , P 0 ~ N 0 `K.Rem` N 1
--   , P 0 ~ P 0 `K.Rem` N 1
--   , P 0 ~ N 0 `K.Rem` P 1
--
--   , P 1 ~ P 1 `K.Rem` P 2
--   , N 1 ~ N 1 `K.Rem` N 2
--   , P 1 ~ P 1 `K.Rem` N 2
--   , N 1 ~ N 1 `K.Rem` P 2
--   )
--
-- _testLog2 =  Dict
-- _testLog2 :: Dict
--   ( P 0 ~ K.Log2 (P 1)
--   , P 1 ~ K.Log2 (P 2)
--   , P 1 ~ K.Log2 (P 3)
--   , P 2 ~ K.Log2 (P 4)
--   , P 2 ~ K.Log2 (P 5)
--   , P 2 ~ K.Log2 (P 6)
--   , P 2 ~ K.Log2 (P 7)
--   , P 3 ~ K.Log2 (P 8)
--   , P 3 ~ K.Log2 (P 9)
--   , P 3 ~ K.Log2 (P 10)
--   , P 3 ~ K.Log2 (P 11)
--   , P 3 ~ K.Log2 (P 12)
--   , P 3 ~ K.Log2 (P 13)
--   , P 3 ~ K.Log2 (P 14)
--   , P 3 ~ K.Log2 (P 15)
--   , P 4 ~ K.Log2 (P 16)
--   , P 4 ~ K.Log2 (P 17)
--   , P 4 ~ K.Log2 (P 18)
--   , P 4 ~ K.Log2 (P 19)
--   , P 4 ~ K.Log2 (P 20)
--   , P 4 ~ K.Log2 (P 21)
--   , P 4 ~ K.Log2 (P 22)
--   , P 4 ~ K.Log2 (P 23)
--   , P 4 ~ K.Log2 (P 24)
--   , P 4 ~ K.Log2 (P 25)
--   , P 4 ~ K.Log2 (P 26)
--   , P 4 ~ K.Log2 (P 27)
--   , P 4 ~ K.Log2 (P 28)
--   , P 4 ~ K.Log2 (P 29)
--   , P 4 ~ K.Log2 (P 30)
--   , P 4 ~ K.Log2 (P 31)
--   , P 5 ~ K.Log2 (P 32)
--   )
--
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

rats :: P.Integer -> [P.Rational]
rats i = do n <- [negate i .. i]
            d <- [negate i .. i]
            guard (d /= 0)
            pure (n P.% d)

main :: IO ()
main = testsMain
  [ assert "integerVal . someIntegerVal == id" $
    flip all (rats 5) $ \r ->
      case K.someRationalVal r of
        K.SomeRational pa ->
          r == K.rationalVal pa

  , assert "integerVal' . someIntegerVal == id" $
    flip all (rats 5) $ \r ->
      case K.someRationalVal r of
        K.SomeRational (_ :: Proxy a) ->
          r == K.rationalVal' (proxy# @a)

  , assert "sameIntegerVal a a" $
    flip all (rats 5) $ \r ->
      case K.someRationalVal r of
        K.SomeRational pa ->
          isJust (K.sameRational pa pa)

  , assert "sameIntegerVal a a'" $
    flip all (rats 5) $ \a ->
      case (K.someRationalVal a, K.someRationalVal a) of
        (K.SomeRational pa1, K.SomeRational pa2) ->
          isJust (K.sameRational pa1 pa2)

  , assert "sameIntegerVal a b" $
    flip all (liftA2 (,) (rats 5) (rats 5)) $ \(a, b) ->
      case (K.someRationalVal a, K.someRationalVal b) of
        (K.SomeRational pa, K.SomeRational pb)
          | a == b    -> isJust    (K.sameRational pa pb)
          | otherwise -> isNothing (K.sameRational pa pb)

  , assert "Eq SomeInteger" $
    flip all (liftA2 (,) (rats 5) (rats 5))$ \(a, b) ->
      (a == b) == (K.someRationalVal a == K.someRationalVal b)

  , assert "Ord SomeInteger" $
    flip all (liftA2 (,) (rats 5) (rats 5))$ \(a, b) ->
      (a `compare` b) == (K.someRationalVal a `compare` K.someRationalVal b)

  , assert "Show SomeInteger" $
    flip all (rats 5) $ \a ->
      show a == show (K.someRationalVal a)

  , assert "Read SomeInteger" $
    flip all (rats 5) $ \r ->
      let str = show r
      in readMaybe @P.Rational str
            == fmap (\(K.SomeRational p) -> K.rationalVal p)
                    (readMaybe @K.SomeRational str)

  , assert "div" $
    flip all (rats 5) $ \r ->
      let (n P.:% d) = r
      in P.div n d == K.div r

  , assert "mod" $
    flip all (rats 5) $ \r ->
      let (n P.:% d) = r
      in  P.mod n d == K.mod r

  , assert "quot" $
    flip all (rats 5) $ \r ->
      let (n P.:% d) = r
      in P.quot n d == K.quot r
  ]


