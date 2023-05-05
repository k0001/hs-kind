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
import Data.Singletons
import Data.String
import GHC.Exts (Constraint)
import GHC.TypeLits qualified as L
import Prelude hiding (Integer)
import Prelude qualified as P
import Prelude.Singletons qualified as P
import System.Exit
import Text.Read
import Text.ParserCombinators.ReadP qualified as ReadP
import Text.ParserCombinators.ReadPrec qualified as ReadPrec
import Text.Show.Singletons

import KindInteger (P, N, Z, pattern SP, pattern SN, pattern SZ,
  SInteger, pattern SInteger)
import KindInteger qualified as K

--------------------------------------------------------------------------------

data Dict (c :: Constraint) where
  Dict :: c => Dict c

readPrecMaybe :: ReadPrec a -> String -> Maybe a
readPrecMaybe p s =
  case readPrec_to_S (p <* ReadPrec.lift ReadP.skipSpaces) 0 s of
    [(x, "")] -> Just x
    _         -> Nothing

--------------------------------------------------------------------------------

_testEq =  Dict
_testEq :: Dict
  ( 'True ~ (Z P.== Z)
  , 'True ~ (Z P.== Z)
  , 'True ~ (Z P.== Z)
  , 'True ~ (Z P.== Z)

  , 'True ~ (Z P./= P 1)
  , 'True ~ (Z P./= N 1)

  , 'True ~ (Z P./= N 1)
  , 'True ~ (Z P./= N 1)

  , 'True ~ (P 1 P./= Z)
  , 'True ~ (P 1 P./= Z)

  , 'True ~ (N 1 P./= Z)
  , 'True ~ (N 1 P./= Z)
  )

_testCmp =  Dict
_testCmp :: Dict
  ( Z <= Z
  , Z <= Z
  , Z <= Z
  , Z <= Z

  , N 2 <= N 1
  , N 1 <= Z
  , Z <= P 1

  , Z <= P 1
  , P 1 <= P 2
  )

_testAdd  = Dict
_testAdd :: Dict
  ( Z ~ Z P.+ Z
  , Z ~ Z P.+ Z
  , Z ~ Z P.+ Z
  , Z ~ Z P.+ Z

  , P 1 ~ P 1 P.+ Z
  , N 1 ~ N 1 P.+ Z
  , P 1 ~ P 1 P.+ Z
  , N 1 ~ N 1 P.+ Z

  , P 1 ~ Z P.+ P 1
  , N 1 ~ Z P.+ N 1
  , N 1 ~ Z P.+ N 1
  , P 1 ~ Z P.+ P 1

  , P 2 ~ P 1 P.+ P 1
  , N 2 ~ N 1 P.+ N 1
  , Z ~ P 1 P.+ N 1
  , Z ~ N 1 P.+ P 1
  )

_testMul  = Dict
_testMul :: Dict
  ( Z ~ Z P.* Z
  , Z ~ Z P.* Z
  , Z ~ Z P.* Z
  , Z ~ Z P.* Z

  , Z ~ P 1 P.* Z
  , Z ~ N 1 P.* Z
  , Z ~ P 1 P.* Z
  , Z ~ N 1 P.* Z

  , Z ~ Z P.* P 1
  , Z ~ Z P.* N 1
  , Z ~ Z P.* N 1
  , Z ~ Z P.* P 1

  , P 1 ~ P 1 P.* P 1
  , P 1 ~ N 1 P.* N 1
  , N 1 ~ P 1 P.* N 1
  , N 1 ~ N 1 P.* P 1

  , P 2 ~ P 2 P.* P 1
  , P 2 ~ N 2 P.* N 1
  , N 2 ~ P 2 P.* N 1
  , N 2 ~ N 2 P.* P 1

  , P 6 ~ P 2 P.* P 3
  , P 6 ~ N 2 P.* N 3
  , N 6 ~ P 2 P.* N 3
  , N 6 ~ N 2 P.* P 3
  )

_testLog2 =  Dict
_testLog2 :: Dict
  ( 0 ~ K.Log2 (P 1)
  , 1 ~ K.Log2 (P 2)
  , 1 ~ K.Log2 (P 3)
  , 2 ~ K.Log2 (P 4)
  , 2 ~ K.Log2 (P 5)
  , 2 ~ K.Log2 (P 6)
  , 2 ~ K.Log2 (P 7)
  , 3 ~ K.Log2 (P 8)
  , 3 ~ K.Log2 (P 9)
  , 3 ~ K.Log2 (P 10)
  , 3 ~ K.Log2 (P 11)
  , 3 ~ K.Log2 (P 12)
  , 3 ~ K.Log2 (P 13)
  , 3 ~ K.Log2 (P 14)
  , 3 ~ K.Log2 (P 15)
  , 4 ~ K.Log2 (P 16)
  , 4 ~ K.Log2 (P 17)
  , 4 ~ K.Log2 (P 18)
  , 4 ~ K.Log2 (P 19)
  , 4 ~ K.Log2 (P 20)
  , 4 ~ K.Log2 (P 21)
  , 4 ~ K.Log2 (P 22)
  , 4 ~ K.Log2 (P 23)
  , 4 ~ K.Log2 (P 24)
  , 4 ~ K.Log2 (P 25)
  , 4 ~ K.Log2 (P 26)
  , 4 ~ K.Log2 (P 27)
  , 4 ~ K.Log2 (P 28)
  , 4 ~ K.Log2 (P 29)
  , 4 ~ K.Log2 (P 30)
  , 4 ~ K.Log2 (P 31)
  , 5 ~ K.Log2 (P 32)
  )

_testNegate =  Dict
_testNegate :: Dict
  ( Z ~ P.Negate Z
  , Z ~ P.Negate Z
  , N 1 ~ P.Negate (P 1)
  , P 1 ~ P.Negate (N 1)
  , N 2 ~ P.Negate (P 2)
  , P 2 ~ P.Negate (N 2)
  )

_testSign =  Dict
_testSign :: Dict
  ( Z ~ P.Signum Z
  , Z ~ P.Signum Z
  , P 1 ~ P.Signum (P 1)
  , N 1 ~ P.Signum (N 1)
  , P 1 ~ P.Signum (P 2)
  , N 1 ~ P.Signum (N 2)
  )

_testAbs =  Dict
_testAbs :: Dict
  ( 0 ~ K.Abs Z
  , 0 ~ K.Abs Z
  , 1 ~ K.Abs (P 1)
  , 1 ~ K.Abs (N 1)
  , 2 ~ K.Abs (P 2)
  , 2 ~ K.Abs (N 2)
  )

_testEven =  Dict
_testEven :: Dict
  ( 'True  ~ K.Even Z
  , 'True  ~ K.Even Z
  , 'False ~ K.Even (P 1)
  , 'False ~ K.Even (N 1)
  , 'True  ~ K.Even (P 2)
  , 'True  ~ K.Even (N 2)
  )

_testOdd =  Dict
_testOdd :: Dict
  ( 'False  ~ K.Odd Z
  , 'False  ~ K.Odd Z
  , 'True   ~ K.Odd (P 1)
  , 'True   ~ K.Odd (N 1)
  , 'False  ~ K.Odd (P 2)
  , 'False  ~ K.Odd (N 2)
  )

_testGCD =  Dict
_testGCD :: Dict
  ( 0 ~ K.GCD Z Z
  , 0 ~ K.GCD Z Z
  , 0 ~ K.GCD Z Z
  , 0 ~ K.GCD Z Z

  , 1 ~ K.GCD (P 1) Z
  , 1 ~ K.GCD (P 1) Z
  , 1 ~ K.GCD (N 1) Z
  , 1 ~ K.GCD (N 1) Z

  , 1 ~ K.GCD Z (P 1)
  , 1 ~ K.GCD Z (N 1)
  , 1 ~ K.GCD Z (P 1)
  , 1 ~ K.GCD Z (N 1)

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
  ( 0 ~ K.LCM Z Z
  , 0 ~ K.LCM Z Z
  , 0 ~ K.LCM Z Z
  , 0 ~ K.LCM Z Z

  , 0 ~ K.LCM (P 1) Z
  , 0 ~ K.LCM (P 1) Z
  , 0 ~ K.LCM (N 1) Z
  , 0 ~ K.LCM (N 1) Z

  , 0 ~ K.LCM Z (P 1)
  , 0 ~ K.LCM Z (N 1)
  , 0 ~ K.LCM Z (P 1)
  , 0 ~ K.LCM Z (N 1)

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

sn1, sz, sp1 :: K.SomeInteger
sn1 = K.SomeInteger (Proxy @(N 1))
sz = K.SomeInteger (Proxy @Z)
sp1 = K.SomeInteger (Proxy @(P 1))


main :: IO ()
main = testsMain $
  [ assert "Prelude % throws RatioZeroDenominator" =<< do
      ea <- Ex.try (Ex.evaluate (1 P.% 0 :: P.Rational))
      pure (ea == Left Ex.RatioZeroDenominator)

  , assert "integerVal . someIntegerVal == id" $
    flip all [-5 .. 5] $ \a ->
      case K.someIntegerVal a of
        K.SomeInteger pa ->
          a == K.integerVal pa

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

  , assert "demote @Z" $ demote @Z == (0 :: P.Integer)
  , assert "demote @(P 1)" $ demote @(P 1) == (1 :: P.Integer)
  , assert "demote @(N 1)" $ demote @(N 1) == ((-1) :: P.Integer)

  , assert "Eq SomeInteger" $
    flip all (liftA2 (,) [(-2)..2] [(-2)..2]) $ \(a, b) ->
      (==) (K.someIntegerVal a) (K.someIntegerVal b)
        == (==) a b

  , assert "Ord SomeInteger" $
    flip all (liftA2 (,) [(-2)..2] [(-2)..2]) $ \(a, b) ->
      compare (K.someIntegerVal a) (K.someIntegerVal b)
        == compare a b

  , assert "Show SomeInteger" $
    and [ show sn1 == "-1"
        , show sz == "0"
        , show sp1 == "1" ]

  , assert "Read SomeInteger" $
    and [ readMaybe "-1" == Just sn1
        , readMaybe "0" == Just sz
        , readMaybe "1" == Just sp1 ]

  , assert "TestEquality 0 0" $
     isJust (testEquality (SInteger @Z) (SInteger @Z))
  , assert "TestEquality 0 +1" $
     isNothing (testEquality (SInteger @Z) (SInteger @(P 1)))
  , assert "TestEquality 0 -1" $
     isNothing (testEquality (SInteger @Z) (SInteger @(N 1)))
  , assert "TestEquality +1 0" $
     isNothing (testEquality (SInteger @(P 1)) (SInteger @Z))
  , assert "TestEquality -1 0" $
     isNothing (testEquality (SInteger @(N 1)) (SInteger @Z))

  , assert "show SInteger 0" $
     "SInteger @Z" == show (SInteger @Z)
  , assert "show SInteger +1" $
     "SInteger @(P 1)" == show (SInteger @(P 1))
  , assert "show SInteger -1" $
     "SInteger @(N 1)" == show (SInteger @(N 1))

  , assert "sShow SInteger 0" $
     fromString "0" == fromSing (P.sShow_ (SInteger @Z))
  , assert "sShow SInteger +1" $
     fromString "1" == fromSing (P.sShow_ (SInteger @(P 1)))
  , assert "sShow SInteger -1" $
     fromString "-1" == fromSing (P.sShow_ (SInteger @(N 1)))

  , assert "sShowsPrec appPrec SInteger 0" $
     fromString "0y" == fromSing (P.sShowsPrec sAppPrec (SInteger @Z) (sing @"y"))
  , assert "sShowsPrec appPrec SInteger +1" $
     fromString "1y" == fromSing (P.sShowsPrec sAppPrec (SInteger @(P 1)) (sing @"y"))
  , assert "sShowPrec appPrec SInteger -1" $
     fromString "-1y" == fromSing (P.sShowsPrec sAppPrec (SInteger @(N 1)) (sing @"y"))

  , assert "sShowsPrec appPrec1 SInteger 0" $
     fromString "0y" == fromSing (P.sShowsPrec sAppPrec1 (SInteger @Z) (sing @"y"))
  , assert "sShowsPrec appPrec1 SInteger +1" $
     fromString "1y" == fromSing (P.sShowsPrec sAppPrec1 (SInteger @(P 1)) (sing @"y"))
  , assert "sShowPrec appPrec1 SInteger -1" $
     fromString "-1y" == fromSing (P.sShowsPrec sAppPrec1 (SInteger @(N 1)) (sing @"y"))

  , assert "sShowLit SInteger 0" $
     fromString "Z" == fromSing (K.sShowLit (SInteger @Z))
  , assert "sShowLit SInteger +1" $
     fromString "P 1" == fromSing (K.sShowLit (SInteger @(P 1)))
  , assert "sShowLit SInteger -1" $
     fromString "N 1" == fromSing (K.sShowLit (SInteger @(N 1)))

  , assert "sShowsPrecLit appPrec SInteger 0" $
     fromString "Zy" == fromSing (K.sShowsPrecLit sAppPrec (SInteger @Z) (sing @"y"))
  , assert "sShowsPrecLit appPrec SInteger +1" $
     fromString "P 1y" == fromSing (K.sShowsPrecLit sAppPrec (SInteger @(P 1)) (sing @"y"))
  , assert "sShowPrec appPrec SInteger -1" $
     fromString "N 1y" == fromSing (K.sShowsPrecLit sAppPrec (SInteger @(N 1)) (sing @"y"))

  , assert "sShowsPrecLit appPrec1 SInteger 0" $
     fromString "Zy" == fromSing (K.sShowsPrecLit sAppPrec1 (SInteger @Z) (sing @"y"))
  , assert "sShowsPrecLit appPrec1 SInteger +1" $
     fromString "(P 1)y" == fromSing (K.sShowsPrecLit sAppPrec1 (SInteger @(P 1)) (sing @"y"))
  , assert "sShowPrec appPrec1 SInteger -1" $
     fromString "(N 1)y" == fromSing (K.sShowsPrecLit sAppPrec1 (SInteger @(N 1)) (sing @"y"))

  , assert "readPrecLit Z" $
     readPrecMaybe K.readPrecLit "Z" == Just 0
  , assert "readPrecLit P" $
     readPrecMaybe K.readPrecLit "P 0" == Nothing &&
     readPrecMaybe K.readPrecLit "P 1" == Just 1
  , assert "readPrecLit N" $
     readPrecMaybe K.readPrecLit "N 0" == Nothing &&
     readPrecMaybe K.readPrecLit "N 1" == Just (-1)

  , assert "Read SInteger 0" $
     readMaybe @(SInteger Z) "SInteger @Z" == Just (SInteger @Z)
  , assert "Read SInteger +1" $
     readMaybe @(SInteger (P 1)) "SInteger @(P 1)" == Just (SInteger @(P 1))
  , assert "Read SInteger -1" $
     readMaybe @(SInteger (N 1)) "SInteger @(N 1)" == Just (SInteger @(N 1))

  , assert "SZ == SInteger @Z" $
    case SInteger @Z of
      SZ -> SZ == SInteger @Z

  , assert "SN (sing @1) == SInteger @(N 1)" $
    case SInteger @(N 1) of
      SN s1 -> (fromSing @L.Natural s1 == 1) &&
               (SN s1 == SInteger @(N 1))

  , assert "SP (sing @1) == SInteger @(P 1)" $
    case SInteger @(P 1) of
      SP s1 -> (fromSing @L.Natural s1 == 1) &&
               (SP s1 == SInteger @(P 1))

  , assert "KnownInteger i ==> KnownNat (Abs i)" $
    let fabs :: forall i. SInteger i -> P.Integer
        fabs si = K.withKnownInteger si (L.natVal (Proxy @(K.Abs i)))
    in and [ 1 == fabs (sing @(N 1))
           , 0 == fabs (sing @Z)
           , 1 == fabs (sing @(P 1)) ]

  , assert "sNegate" $
    flip all [-5 .. 5] $ \(a :: P.Integer) ->
      negate a == withSomeSing a (fromSing . P.sNegate)

  , assert "%+" $
    flip all (liftA2 (,) [-5 .. 5] [-5 .. 5])$ \(a, b) ->
      a + b == (withSomeSing (a :: P.Integer) $ \sa ->
                withSomeSing (b :: P.Integer) $ \sb ->
                fromSing $ sa P.%+ sb)

  , assert "%-" $
    flip all (liftA2 (,) [-5 .. 5] [-5 .. 5])$ \(a, b) ->
      a - b == (withSomeSing (a :: P.Integer) $ \sa ->
                withSomeSing (b :: P.Integer) $ \sb ->
                fromSing $ sa P.%- sb)

  , assert "%*" $
    flip all (liftA2 (,) [-5 .. 5] [-5 .. 5])$ \(a, b) ->
      a * b == (withSomeSing (a :: P.Integer) $ \sa ->
                withSomeSing (b :: P.Integer) $ \sb ->
                fromSing $ sa P.%* sb)

  , assert "%^" $
    flip all (liftA2 (,) [-5 .. 5] [0 .. 5])$ \(a, b) ->
      a ^ b == (withSomeSing (a :: P.Integer) $ \sa ->
                withSomeSing (b :: P.Integer) $ \sb ->
                fromSing $ sa K.%^ sb)

  , assert "sOdd" $
    flip all [-5 .. 5] $ \(a :: P.Integer) ->
      odd a == withSomeSing a (fromSing . K.sOdd)

  , assert "sEven" $
    flip all [-5 .. 5] $ \(a :: P.Integer) ->
      even a == withSomeSing a (fromSing . K.sEven)

  , assert "sAbs" $
    flip all [-5 .. 5] $ \(a :: P.Integer) ->
      fromInteger @L.Natural (abs a)
        == withSomeSing a (fromSing . K.sAbs)

  , assert "sSignum" $
    flip all [-5 .. 5] $ \(a :: P.Integer) ->
      signum a == withSomeSing a (fromSing . P.sSignum)

  , assert "sGCD" $
    flip all (liftA2 (,) [-5 .. 5] [-5 .. 5])$ \(a, b) ->
      fromInteger @L.Natural (gcd a b)
        == (withSomeSing (a :: P.Integer) $ \sa ->
            withSomeSing (b :: P.Integer) $ \sb ->
            fromSing $ K.sGCD sa sb)

  , assert "sLCM" $
    flip all (liftA2 (,) [-5 .. 5] [-5 .. 5])$ \(a, b) ->
      fromInteger @L.Natural (lcm a b)
        == (withSomeSing (a :: P.Integer) $ \sa ->
            withSomeSing (b :: P.Integer) $ \sb ->
            fromSing $ K.sLCM sa sb)

  , assert "sSign" $
    flip all [-5 .. 5] $ \(a :: P.Integer) ->
      signum a == withSomeSing a (fromSing . P.sSignum)

  , fmap and $ sequence $ do
     r :: K.Round <- [minBound .. maxBound]
     a :: P.Integer <- [-5 .. 5]
     b :: P.Integer <- filter (/= 0) [-5 .. 5]
     pure $ assert (mconcat ["sDivRem ", show r, " ", show a, " ", show b]) $
       K.divRem r a b == (withSomeSing r $ \sr ->
                          withSomeSing a $ \sa ->
                          withSomeSing b $ \sb ->
                          let (q, m) = K.sDivRem sr sa sb
                           in (fromSing q, fromSing m))

  , fmap and $ sequence $ do
     r :: K.Round <- [minBound .. maxBound]
     a :: P.Integer <- [-5 .. 5]
     b :: P.Integer <- filter (/= 0) [-5 .. 5]
     pure $ assert (mconcat ["sDiv ", show r, " ", show a, " ", show b]) $
       K.div r a b == (withSomeSing r $ \sr ->
                       withSomeSing a $ \sa ->
                       withSomeSing b $ \sb ->
                       fromSing (K.sDiv sr sa sb))

  , fmap and $ sequence $ do
     r :: K.Round <- [minBound .. maxBound]
     a :: P.Integer <- [-5 .. 5]
     b :: P.Integer <- filter (/= 0) [-5 .. 5]
     pure $ assert (mconcat ["sRem ", show r, " ", show a, " ", show b]) $
       K.rem r a b == (withSomeSing r $ \sr ->
                       withSomeSing a $ \sa ->
                       withSomeSing b $ \sb ->
                       fromSing (K.sRem sr sa sb))

  , fmap and $ sequence $ do
      n :: L.Natural <- [0..5]
      pure $ assert (mconcat ["sFromNatural ", show n]) $
        toInteger n == withSomeSing n (fromSing . K.sFromNatural)

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

-- _divRemTestCode :: String
-- _divRemTestCode = unlines $ List.sort $ do
--   b <- [-4 .. 4]
--   guard (b P./= 0)
--   a <- [-4 .. 4]
--   r <- [minBound..maxBound]
--   let (q, m) = K.divRem r a b
--       sname :: String -> ShowS
--       sname t = showString "_test_"
--               . showString t
--               . showChar '_'
--               . showsPrec 0 r
--               . showChar '_'
--               . showChar (if a < 0 then 'N' else 'P')
--               . shows (abs a)
--               . showChar '_'
--               . showChar (if b < 0 then 'N' else 'P')
--               . shows (abs b)
--       sDiv :: ShowS
--       sDiv = sname "Div"
--            . showString " :: Dict (K.Div 'K."
--            . shows r
--            . showChar ' '
--            . showsPrec appPrec1 (K.fromPrelude a)
--            . showChar ' '
--            . showsPrec appPrec1 (K.fromPrelude b)
--            . showString " ~ "
--            . showsPrec appPrec (K.fromPrelude q)
--            . showString ")\n"
--            . sname "Div"
--            . showString " =  Dict"
--       sRem :: ShowS
--       sRem = sname "Rem"
--            . showString " :: Dict (K.Rem 'K."
--            . shows r
--            . showChar ' '
--            . showsPrec appPrec1 (K.fromPrelude a)
--            . showChar ' '
--            . showsPrec appPrec1 (K.fromPrelude b)
--            . showString " ~ "
--            . showsPrec appPrec (K.fromPrelude m)
--            . showString ")\n"
--            . sname "Rem"
--            . showString " =  Dict"
--       ss :: ShowS
--       ss = sDiv . showChar '\n' . sRem
--   pure (ss "")


-- The following tests are generated by `_divRemTestCode` in this remule.
-- Copy and paste by hand.
_test_Div_RoundAway_N1_N1 :: Dict (K.Div 'K.RoundAway (N 1) (N 1) ~ P 1)
_test_Div_RoundAway_N1_N1 =  Dict
_test_Rem_RoundAway_N1_N1 :: Dict (K.Rem 'K.RoundAway (N 1) (N 1) ~ Z)
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
_test_Rem_RoundAway_N1_P1 :: Dict (K.Rem 'K.RoundAway (N 1) (P 1) ~ Z)
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
_test_Rem_RoundAway_N2_N1 :: Dict (K.Rem 'K.RoundAway (N 2) (N 1) ~ Z)
_test_Rem_RoundAway_N2_N1 =  Dict
_test_Div_RoundAway_N2_N2 :: Dict (K.Div 'K.RoundAway (N 2) (N 2) ~ P 1)
_test_Div_RoundAway_N2_N2 =  Dict
_test_Rem_RoundAway_N2_N2 :: Dict (K.Rem 'K.RoundAway (N 2) (N 2) ~ Z)
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
_test_Rem_RoundAway_N2_P1 :: Dict (K.Rem 'K.RoundAway (N 2) (P 1) ~ Z)
_test_Rem_RoundAway_N2_P1 =  Dict
_test_Div_RoundAway_N2_P2 :: Dict (K.Div 'K.RoundAway (N 2) (P 2) ~ N 1)
_test_Div_RoundAway_N2_P2 =  Dict
_test_Rem_RoundAway_N2_P2 :: Dict (K.Rem 'K.RoundAway (N 2) (P 2) ~ Z)
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
_test_Rem_RoundAway_N3_N1 :: Dict (K.Rem 'K.RoundAway (N 3) (N 1) ~ Z)
_test_Rem_RoundAway_N3_N1 =  Dict
_test_Div_RoundAway_N3_N2 :: Dict (K.Div 'K.RoundAway (N 3) (N 2) ~ P 2)
_test_Div_RoundAway_N3_N2 =  Dict
_test_Rem_RoundAway_N3_N2 :: Dict (K.Rem 'K.RoundAway (N 3) (N 2) ~ P 1)
_test_Rem_RoundAway_N3_N2 =  Dict
_test_Div_RoundAway_N3_N3 :: Dict (K.Div 'K.RoundAway (N 3) (N 3) ~ P 1)
_test_Div_RoundAway_N3_N3 =  Dict
_test_Rem_RoundAway_N3_N3 :: Dict (K.Rem 'K.RoundAway (N 3) (N 3) ~ Z)
_test_Rem_RoundAway_N3_N3 =  Dict
_test_Div_RoundAway_N3_N4 :: Dict (K.Div 'K.RoundAway (N 3) (N 4) ~ P 1)
_test_Div_RoundAway_N3_N4 =  Dict
_test_Rem_RoundAway_N3_N4 :: Dict (K.Rem 'K.RoundAway (N 3) (N 4) ~ P 1)
_test_Rem_RoundAway_N3_N4 =  Dict
_test_Div_RoundAway_N3_P1 :: Dict (K.Div 'K.RoundAway (N 3) (P 1) ~ N 3)
_test_Div_RoundAway_N3_P1 =  Dict
_test_Rem_RoundAway_N3_P1 :: Dict (K.Rem 'K.RoundAway (N 3) (P 1) ~ Z)
_test_Rem_RoundAway_N3_P1 =  Dict
_test_Div_RoundAway_N3_P2 :: Dict (K.Div 'K.RoundAway (N 3) (P 2) ~ N 2)
_test_Div_RoundAway_N3_P2 =  Dict
_test_Rem_RoundAway_N3_P2 :: Dict (K.Rem 'K.RoundAway (N 3) (P 2) ~ P 1)
_test_Rem_RoundAway_N3_P2 =  Dict
_test_Div_RoundAway_N3_P3 :: Dict (K.Div 'K.RoundAway (N 3) (P 3) ~ N 1)
_test_Div_RoundAway_N3_P3 =  Dict
_test_Rem_RoundAway_N3_P3 :: Dict (K.Rem 'K.RoundAway (N 3) (P 3) ~ Z)
_test_Rem_RoundAway_N3_P3 =  Dict
_test_Div_RoundAway_N3_P4 :: Dict (K.Div 'K.RoundAway (N 3) (P 4) ~ N 1)
_test_Div_RoundAway_N3_P4 =  Dict
_test_Rem_RoundAway_N3_P4 :: Dict (K.Rem 'K.RoundAway (N 3) (P 4) ~ P 1)
_test_Rem_RoundAway_N3_P4 =  Dict
_test_Div_RoundAway_N4_N1 :: Dict (K.Div 'K.RoundAway (N 4) (N 1) ~ P 4)
_test_Div_RoundAway_N4_N1 =  Dict
_test_Rem_RoundAway_N4_N1 :: Dict (K.Rem 'K.RoundAway (N 4) (N 1) ~ Z)
_test_Rem_RoundAway_N4_N1 =  Dict
_test_Div_RoundAway_N4_N2 :: Dict (K.Div 'K.RoundAway (N 4) (N 2) ~ P 2)
_test_Div_RoundAway_N4_N2 =  Dict
_test_Rem_RoundAway_N4_N2 :: Dict (K.Rem 'K.RoundAway (N 4) (N 2) ~ Z)
_test_Rem_RoundAway_N4_N2 =  Dict
_test_Div_RoundAway_N4_N3 :: Dict (K.Div 'K.RoundAway (N 4) (N 3) ~ P 2)
_test_Div_RoundAway_N4_N3 =  Dict
_test_Rem_RoundAway_N4_N3 :: Dict (K.Rem 'K.RoundAway (N 4) (N 3) ~ P 2)
_test_Rem_RoundAway_N4_N3 =  Dict
_test_Div_RoundAway_N4_N4 :: Dict (K.Div 'K.RoundAway (N 4) (N 4) ~ P 1)
_test_Div_RoundAway_N4_N4 =  Dict
_test_Rem_RoundAway_N4_N4 :: Dict (K.Rem 'K.RoundAway (N 4) (N 4) ~ Z)
_test_Rem_RoundAway_N4_N4 =  Dict
_test_Div_RoundAway_N4_P1 :: Dict (K.Div 'K.RoundAway (N 4) (P 1) ~ N 4)
_test_Div_RoundAway_N4_P1 =  Dict
_test_Rem_RoundAway_N4_P1 :: Dict (K.Rem 'K.RoundAway (N 4) (P 1) ~ Z)
_test_Rem_RoundAway_N4_P1 =  Dict
_test_Div_RoundAway_N4_P2 :: Dict (K.Div 'K.RoundAway (N 4) (P 2) ~ N 2)
_test_Div_RoundAway_N4_P2 =  Dict
_test_Rem_RoundAway_N4_P2 :: Dict (K.Rem 'K.RoundAway (N 4) (P 2) ~ Z)
_test_Rem_RoundAway_N4_P2 =  Dict
_test_Div_RoundAway_N4_P3 :: Dict (K.Div 'K.RoundAway (N 4) (P 3) ~ N 2)
_test_Div_RoundAway_N4_P3 =  Dict
_test_Rem_RoundAway_N4_P3 :: Dict (K.Rem 'K.RoundAway (N 4) (P 3) ~ P 2)
_test_Rem_RoundAway_N4_P3 =  Dict
_test_Div_RoundAway_N4_P4 :: Dict (K.Div 'K.RoundAway (N 4) (P 4) ~ N 1)
_test_Div_RoundAway_N4_P4 =  Dict
_test_Rem_RoundAway_N4_P4 :: Dict (K.Rem 'K.RoundAway (N 4) (P 4) ~ Z)
_test_Rem_RoundAway_N4_P4 =  Dict
_test_Rem_RoundAway_Z_N1 :: Dict (K.Rem 'K.RoundAway Z (N 1) ~ Z)
_test_Rem_RoundAway_Z_N1 =  Dict
_test_Rem_RoundAway_Z_N2 :: Dict (K.Rem 'K.RoundAway Z (N 2) ~ Z)
_test_Rem_RoundAway_Z_N2 =  Dict
_test_Rem_RoundAway_Z_N3 :: Dict (K.Rem 'K.RoundAway Z (N 3) ~ Z)
_test_Rem_RoundAway_Z_N3 =  Dict
_test_Rem_RoundAway_Z_N4 :: Dict (K.Rem 'K.RoundAway Z (N 4) ~ Z)
_test_Rem_RoundAway_Z_N4 =  Dict
_test_Rem_RoundAway_Z_P1 :: Dict (K.Rem 'K.RoundAway Z (P 1) ~ Z)
_test_Rem_RoundAway_Z_P1 =  Dict
_test_Rem_RoundAway_Z_P2 :: Dict (K.Rem 'K.RoundAway Z (P 2) ~ Z)
_test_Rem_RoundAway_Z_P2 =  Dict
_test_Rem_RoundAway_Z_P3 :: Dict (K.Rem 'K.RoundAway Z (P 3) ~ Z)
_test_Rem_RoundAway_Z_P3 =  Dict
_test_Rem_RoundAway_Z_P4 :: Dict (K.Rem 'K.RoundAway Z (P 4) ~ Z)
_test_Rem_RoundAway_Z_P4 =  Dict
_test_Div_RoundAway_P1_N1 :: Dict (K.Div 'K.RoundAway (P 1) (N 1) ~ N 1)
_test_Div_RoundAway_P1_N1 =  Dict
_test_Rem_RoundAway_P1_N1 :: Dict (K.Rem 'K.RoundAway (P 1) (N 1) ~ Z)
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
_test_Rem_RoundAway_P1_P1 :: Dict (K.Rem 'K.RoundAway (P 1) (P 1) ~ Z)
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
_test_Rem_RoundAway_P2_N1 :: Dict (K.Rem 'K.RoundAway (P 2) (N 1) ~ Z)
_test_Rem_RoundAway_P2_N1 =  Dict
_test_Div_RoundAway_P2_N2 :: Dict (K.Div 'K.RoundAway (P 2) (N 2) ~ N 1)
_test_Div_RoundAway_P2_N2 =  Dict
_test_Rem_RoundAway_P2_N2 :: Dict (K.Rem 'K.RoundAway (P 2) (N 2) ~ Z)
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
_test_Rem_RoundAway_P2_P1 :: Dict (K.Rem 'K.RoundAway (P 2) (P 1) ~ Z)
_test_Rem_RoundAway_P2_P1 =  Dict
_test_Div_RoundAway_P2_P2 :: Dict (K.Div 'K.RoundAway (P 2) (P 2) ~ P 1)
_test_Div_RoundAway_P2_P2 =  Dict
_test_Rem_RoundAway_P2_P2 :: Dict (K.Rem 'K.RoundAway (P 2) (P 2) ~ Z)
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
_test_Rem_RoundAway_P3_N1 :: Dict (K.Rem 'K.RoundAway (P 3) (N 1) ~ Z)
_test_Rem_RoundAway_P3_N1 =  Dict
_test_Div_RoundAway_P3_N2 :: Dict (K.Div 'K.RoundAway (P 3) (N 2) ~ N 2)
_test_Div_RoundAway_P3_N2 =  Dict
_test_Rem_RoundAway_P3_N2 :: Dict (K.Rem 'K.RoundAway (P 3) (N 2) ~ N 1)
_test_Rem_RoundAway_P3_N2 =  Dict
_test_Div_RoundAway_P3_N3 :: Dict (K.Div 'K.RoundAway (P 3) (N 3) ~ N 1)
_test_Div_RoundAway_P3_N3 =  Dict
_test_Rem_RoundAway_P3_N3 :: Dict (K.Rem 'K.RoundAway (P 3) (N 3) ~ Z)
_test_Rem_RoundAway_P3_N3 =  Dict
_test_Div_RoundAway_P3_N4 :: Dict (K.Div 'K.RoundAway (P 3) (N 4) ~ N 1)
_test_Div_RoundAway_P3_N4 =  Dict
_test_Rem_RoundAway_P3_N4 :: Dict (K.Rem 'K.RoundAway (P 3) (N 4) ~ N 1)
_test_Rem_RoundAway_P3_N4 =  Dict
_test_Div_RoundAway_P3_P1 :: Dict (K.Div 'K.RoundAway (P 3) (P 1) ~ P 3)
_test_Div_RoundAway_P3_P1 =  Dict
_test_Rem_RoundAway_P3_P1 :: Dict (K.Rem 'K.RoundAway (P 3) (P 1) ~ Z)
_test_Rem_RoundAway_P3_P1 =  Dict
_test_Div_RoundAway_P3_P2 :: Dict (K.Div 'K.RoundAway (P 3) (P 2) ~ P 2)
_test_Div_RoundAway_P3_P2 =  Dict
_test_Rem_RoundAway_P3_P2 :: Dict (K.Rem 'K.RoundAway (P 3) (P 2) ~ N 1)
_test_Rem_RoundAway_P3_P2 =  Dict
_test_Div_RoundAway_P3_P3 :: Dict (K.Div 'K.RoundAway (P 3) (P 3) ~ P 1)
_test_Div_RoundAway_P3_P3 =  Dict
_test_Rem_RoundAway_P3_P3 :: Dict (K.Rem 'K.RoundAway (P 3) (P 3) ~ Z)
_test_Rem_RoundAway_P3_P3 =  Dict
_test_Div_RoundAway_P3_P4 :: Dict (K.Div 'K.RoundAway (P 3) (P 4) ~ P 1)
_test_Div_RoundAway_P3_P4 =  Dict
_test_Rem_RoundAway_P3_P4 :: Dict (K.Rem 'K.RoundAway (P 3) (P 4) ~ N 1)
_test_Rem_RoundAway_P3_P4 =  Dict
_test_Div_RoundAway_P4_N1 :: Dict (K.Div 'K.RoundAway (P 4) (N 1) ~ N 4)
_test_Div_RoundAway_P4_N1 =  Dict
_test_Rem_RoundAway_P4_N1 :: Dict (K.Rem 'K.RoundAway (P 4) (N 1) ~ Z)
_test_Rem_RoundAway_P4_N1 =  Dict
_test_Div_RoundAway_P4_N2 :: Dict (K.Div 'K.RoundAway (P 4) (N 2) ~ N 2)
_test_Div_RoundAway_P4_N2 =  Dict
_test_Rem_RoundAway_P4_N2 :: Dict (K.Rem 'K.RoundAway (P 4) (N 2) ~ Z)
_test_Rem_RoundAway_P4_N2 =  Dict
_test_Div_RoundAway_P4_N3 :: Dict (K.Div 'K.RoundAway (P 4) (N 3) ~ N 2)
_test_Div_RoundAway_P4_N3 =  Dict
_test_Rem_RoundAway_P4_N3 :: Dict (K.Rem 'K.RoundAway (P 4) (N 3) ~ N 2)
_test_Rem_RoundAway_P4_N3 =  Dict
_test_Div_RoundAway_P4_N4 :: Dict (K.Div 'K.RoundAway (P 4) (N 4) ~ N 1)
_test_Div_RoundAway_P4_N4 =  Dict
_test_Rem_RoundAway_P4_N4 :: Dict (K.Rem 'K.RoundAway (P 4) (N 4) ~ Z)
_test_Rem_RoundAway_P4_N4 =  Dict
_test_Div_RoundAway_P4_P1 :: Dict (K.Div 'K.RoundAway (P 4) (P 1) ~ P 4)
_test_Div_RoundAway_P4_P1 =  Dict
_test_Rem_RoundAway_P4_P1 :: Dict (K.Rem 'K.RoundAway (P 4) (P 1) ~ Z)
_test_Rem_RoundAway_P4_P1 =  Dict
_test_Div_RoundAway_P4_P2 :: Dict (K.Div 'K.RoundAway (P 4) (P 2) ~ P 2)
_test_Div_RoundAway_P4_P2 =  Dict
_test_Rem_RoundAway_P4_P2 :: Dict (K.Rem 'K.RoundAway (P 4) (P 2) ~ Z)
_test_Rem_RoundAway_P4_P2 =  Dict
_test_Div_RoundAway_P4_P3 :: Dict (K.Div 'K.RoundAway (P 4) (P 3) ~ P 2)
_test_Div_RoundAway_P4_P3 =  Dict
_test_Rem_RoundAway_P4_P3 :: Dict (K.Rem 'K.RoundAway (P 4) (P 3) ~ N 2)
_test_Rem_RoundAway_P4_P3 =  Dict
_test_Div_RoundAway_P4_P4 :: Dict (K.Div 'K.RoundAway (P 4) (P 4) ~ P 1)
_test_Div_RoundAway_P4_P4 =  Dict
_test_Rem_RoundAway_P4_P4 :: Dict (K.Rem 'K.RoundAway (P 4) (P 4) ~ Z)
_test_Rem_RoundAway_P4_P4 =  Dict
_test_Div_RoundDown_N1_N1 :: Dict (K.Div 'K.RoundDown (N 1) (N 1) ~ P 1)
_test_Div_RoundDown_N1_N1 =  Dict
_test_Rem_RoundDown_N1_N1 :: Dict (K.Rem 'K.RoundDown (N 1) (N 1) ~ Z)
_test_Rem_RoundDown_N1_N1 =  Dict
_test_Div_RoundDown_N1_N2 :: Dict (K.Div 'K.RoundDown (N 1) (N 2) ~ Z)
_test_Div_RoundDown_N1_N2 =  Dict
_test_Rem_RoundDown_N1_N2 :: Dict (K.Rem 'K.RoundDown (N 1) (N 2) ~ N 1)
_test_Rem_RoundDown_N1_N2 =  Dict
_test_Div_RoundDown_N1_N3 :: Dict (K.Div 'K.RoundDown (N 1) (N 3) ~ Z)
_test_Div_RoundDown_N1_N3 =  Dict
_test_Rem_RoundDown_N1_N3 :: Dict (K.Rem 'K.RoundDown (N 1) (N 3) ~ N 1)
_test_Rem_RoundDown_N1_N3 =  Dict
_test_Div_RoundDown_N1_N4 :: Dict (K.Div 'K.RoundDown (N 1) (N 4) ~ Z)
_test_Div_RoundDown_N1_N4 =  Dict
_test_Rem_RoundDown_N1_N4 :: Dict (K.Rem 'K.RoundDown (N 1) (N 4) ~ N 1)
_test_Rem_RoundDown_N1_N4 =  Dict
_test_Div_RoundDown_N1_P1 :: Dict (K.Div 'K.RoundDown (N 1) (P 1) ~ N 1)
_test_Div_RoundDown_N1_P1 =  Dict
_test_Rem_RoundDown_N1_P1 :: Dict (K.Rem 'K.RoundDown (N 1) (P 1) ~ Z)
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
_test_Rem_RoundDown_N2_N1 :: Dict (K.Rem 'K.RoundDown (N 2) (N 1) ~ Z)
_test_Rem_RoundDown_N2_N1 =  Dict
_test_Div_RoundDown_N2_N2 :: Dict (K.Div 'K.RoundDown (N 2) (N 2) ~ P 1)
_test_Div_RoundDown_N2_N2 =  Dict
_test_Rem_RoundDown_N2_N2 :: Dict (K.Rem 'K.RoundDown (N 2) (N 2) ~ Z)
_test_Rem_RoundDown_N2_N2 =  Dict
_test_Div_RoundDown_N2_N3 :: Dict (K.Div 'K.RoundDown (N 2) (N 3) ~ Z)
_test_Div_RoundDown_N2_N3 =  Dict
_test_Rem_RoundDown_N2_N3 :: Dict (K.Rem 'K.RoundDown (N 2) (N 3) ~ N 2)
_test_Rem_RoundDown_N2_N3 =  Dict
_test_Div_RoundDown_N2_N4 :: Dict (K.Div 'K.RoundDown (N 2) (N 4) ~ Z)
_test_Div_RoundDown_N2_N4 =  Dict
_test_Rem_RoundDown_N2_N4 :: Dict (K.Rem 'K.RoundDown (N 2) (N 4) ~ N 2)
_test_Rem_RoundDown_N2_N4 =  Dict
_test_Div_RoundDown_N2_P1 :: Dict (K.Div 'K.RoundDown (N 2) (P 1) ~ N 2)
_test_Div_RoundDown_N2_P1 =  Dict
_test_Rem_RoundDown_N2_P1 :: Dict (K.Rem 'K.RoundDown (N 2) (P 1) ~ Z)
_test_Rem_RoundDown_N2_P1 =  Dict
_test_Div_RoundDown_N2_P2 :: Dict (K.Div 'K.RoundDown (N 2) (P 2) ~ N 1)
_test_Div_RoundDown_N2_P2 =  Dict
_test_Rem_RoundDown_N2_P2 :: Dict (K.Rem 'K.RoundDown (N 2) (P 2) ~ Z)
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
_test_Rem_RoundDown_N3_N1 :: Dict (K.Rem 'K.RoundDown (N 3) (N 1) ~ Z)
_test_Rem_RoundDown_N3_N1 =  Dict
_test_Div_RoundDown_N3_N2 :: Dict (K.Div 'K.RoundDown (N 3) (N 2) ~ P 1)
_test_Div_RoundDown_N3_N2 =  Dict
_test_Rem_RoundDown_N3_N2 :: Dict (K.Rem 'K.RoundDown (N 3) (N 2) ~ N 1)
_test_Rem_RoundDown_N3_N2 =  Dict
_test_Div_RoundDown_N3_N3 :: Dict (K.Div 'K.RoundDown (N 3) (N 3) ~ P 1)
_test_Div_RoundDown_N3_N3 =  Dict
_test_Rem_RoundDown_N3_N3 :: Dict (K.Rem 'K.RoundDown (N 3) (N 3) ~ Z)
_test_Rem_RoundDown_N3_N3 =  Dict
_test_Div_RoundDown_N3_N4 :: Dict (K.Div 'K.RoundDown (N 3) (N 4) ~ Z)
_test_Div_RoundDown_N3_N4 =  Dict
_test_Rem_RoundDown_N3_N4 :: Dict (K.Rem 'K.RoundDown (N 3) (N 4) ~ N 3)
_test_Rem_RoundDown_N3_N4 =  Dict
_test_Div_RoundDown_N3_P1 :: Dict (K.Div 'K.RoundDown (N 3) (P 1) ~ N 3)
_test_Div_RoundDown_N3_P1 =  Dict
_test_Rem_RoundDown_N3_P1 :: Dict (K.Rem 'K.RoundDown (N 3) (P 1) ~ Z)
_test_Rem_RoundDown_N3_P1 =  Dict
_test_Div_RoundDown_N3_P2 :: Dict (K.Div 'K.RoundDown (N 3) (P 2) ~ N 2)
_test_Div_RoundDown_N3_P2 =  Dict
_test_Rem_RoundDown_N3_P2 :: Dict (K.Rem 'K.RoundDown (N 3) (P 2) ~ P 1)
_test_Rem_RoundDown_N3_P2 =  Dict
_test_Div_RoundDown_N3_P3 :: Dict (K.Div 'K.RoundDown (N 3) (P 3) ~ N 1)
_test_Div_RoundDown_N3_P3 =  Dict
_test_Rem_RoundDown_N3_P3 :: Dict (K.Rem 'K.RoundDown (N 3) (P 3) ~ Z)
_test_Rem_RoundDown_N3_P3 =  Dict
_test_Div_RoundDown_N3_P4 :: Dict (K.Div 'K.RoundDown (N 3) (P 4) ~ N 1)
_test_Div_RoundDown_N3_P4 =  Dict
_test_Rem_RoundDown_N3_P4 :: Dict (K.Rem 'K.RoundDown (N 3) (P 4) ~ P 1)
_test_Rem_RoundDown_N3_P4 =  Dict
_test_Div_RoundDown_N4_N1 :: Dict (K.Div 'K.RoundDown (N 4) (N 1) ~ P 4)
_test_Div_RoundDown_N4_N1 =  Dict
_test_Rem_RoundDown_N4_N1 :: Dict (K.Rem 'K.RoundDown (N 4) (N 1) ~ Z)
_test_Rem_RoundDown_N4_N1 =  Dict
_test_Div_RoundDown_N4_N2 :: Dict (K.Div 'K.RoundDown (N 4) (N 2) ~ P 2)
_test_Div_RoundDown_N4_N2 =  Dict
_test_Rem_RoundDown_N4_N2 :: Dict (K.Rem 'K.RoundDown (N 4) (N 2) ~ Z)
_test_Rem_RoundDown_N4_N2 =  Dict
_test_Div_RoundDown_N4_N3 :: Dict (K.Div 'K.RoundDown (N 4) (N 3) ~ P 1)
_test_Div_RoundDown_N4_N3 =  Dict
_test_Rem_RoundDown_N4_N3 :: Dict (K.Rem 'K.RoundDown (N 4) (N 3) ~ N 1)
_test_Rem_RoundDown_N4_N3 =  Dict
_test_Div_RoundDown_N4_N4 :: Dict (K.Div 'K.RoundDown (N 4) (N 4) ~ P 1)
_test_Div_RoundDown_N4_N4 =  Dict
_test_Rem_RoundDown_N4_N4 :: Dict (K.Rem 'K.RoundDown (N 4) (N 4) ~ Z)
_test_Rem_RoundDown_N4_N4 =  Dict
_test_Div_RoundDown_N4_P1 :: Dict (K.Div 'K.RoundDown (N 4) (P 1) ~ N 4)
_test_Div_RoundDown_N4_P1 =  Dict
_test_Rem_RoundDown_N4_P1 :: Dict (K.Rem 'K.RoundDown (N 4) (P 1) ~ Z)
_test_Rem_RoundDown_N4_P1 =  Dict
_test_Div_RoundDown_N4_P2 :: Dict (K.Div 'K.RoundDown (N 4) (P 2) ~ N 2)
_test_Div_RoundDown_N4_P2 =  Dict
_test_Rem_RoundDown_N4_P2 :: Dict (K.Rem 'K.RoundDown (N 4) (P 2) ~ Z)
_test_Rem_RoundDown_N4_P2 =  Dict
_test_Div_RoundDown_N4_P3 :: Dict (K.Div 'K.RoundDown (N 4) (P 3) ~ N 2)
_test_Div_RoundDown_N4_P3 =  Dict
_test_Rem_RoundDown_N4_P3 :: Dict (K.Rem 'K.RoundDown (N 4) (P 3) ~ P 2)
_test_Rem_RoundDown_N4_P3 =  Dict
_test_Div_RoundDown_N4_P4 :: Dict (K.Div 'K.RoundDown (N 4) (P 4) ~ N 1)
_test_Div_RoundDown_N4_P4 =  Dict
_test_Rem_RoundDown_N4_P4 :: Dict (K.Rem 'K.RoundDown (N 4) (P 4) ~ Z)
_test_Rem_RoundDown_N4_P4 =  Dict
_test_Rem_RoundDown_Z_N1 :: Dict (K.Rem 'K.RoundDown Z (N 1) ~ Z)
_test_Rem_RoundDown_Z_N1 =  Dict
_test_Rem_RoundDown_Z_N2 :: Dict (K.Rem 'K.RoundDown Z (N 2) ~ Z)
_test_Rem_RoundDown_Z_N2 =  Dict
_test_Rem_RoundDown_Z_N3 :: Dict (K.Rem 'K.RoundDown Z (N 3) ~ Z)
_test_Rem_RoundDown_Z_N3 =  Dict
_test_Rem_RoundDown_Z_N4 :: Dict (K.Rem 'K.RoundDown Z (N 4) ~ Z)
_test_Rem_RoundDown_Z_N4 =  Dict
_test_Rem_RoundDown_Z_P1 :: Dict (K.Rem 'K.RoundDown Z (P 1) ~ Z)
_test_Rem_RoundDown_Z_P1 =  Dict
_test_Rem_RoundDown_Z_P2 :: Dict (K.Rem 'K.RoundDown Z (P 2) ~ Z)
_test_Rem_RoundDown_Z_P2 =  Dict
_test_Rem_RoundDown_Z_P3 :: Dict (K.Rem 'K.RoundDown Z (P 3) ~ Z)
_test_Rem_RoundDown_Z_P3 =  Dict
_test_Rem_RoundDown_Z_P4 :: Dict (K.Rem 'K.RoundDown Z (P 4) ~ Z)
_test_Rem_RoundDown_Z_P4 =  Dict
_test_Rem_RoundDown_P1_N1 :: Dict (K.Rem 'K.RoundDown (P 1) (N 1) ~ Z)
_test_Rem_RoundDown_P1_N1 =  Dict
_test_Rem_RoundDown_P1_N2 :: Dict (K.Rem 'K.RoundDown (P 1) (N 2) ~ N 1)
_test_Rem_RoundDown_P1_N2 =  Dict
_test_Rem_RoundDown_P1_N3 :: Dict (K.Rem 'K.RoundDown (P 1) (N 3) ~ N 2)
_test_Rem_RoundDown_P1_N3 =  Dict
_test_Rem_RoundDown_P1_N4 :: Dict (K.Rem 'K.RoundDown (P 1) (N 4) ~ N 3)
_test_Rem_RoundDown_P1_N4 =  Dict
_test_Div_RoundDown_P1_P1 :: Dict (K.Div 'K.RoundDown (P 1) (P 1) ~ P 1)
_test_Div_RoundDown_P1_P1 =  Dict
_test_Rem_RoundDown_P1_P1 :: Dict (K.Rem 'K.RoundDown (P 1) (P 1) ~ Z)
_test_Rem_RoundDown_P1_P1 =  Dict
_test_Div_RoundDown_P1_P2 :: Dict (K.Div 'K.RoundDown (P 1) (P 2) ~ Z)
_test_Div_RoundDown_P1_P2 =  Dict
_test_Rem_RoundDown_P1_P2 :: Dict (K.Rem 'K.RoundDown (P 1) (P 2) ~ P 1)
_test_Rem_RoundDown_P1_P2 =  Dict
_test_Div_RoundDown_P1_P3 :: Dict (K.Div 'K.RoundDown (P 1) (P 3) ~ Z)
_test_Div_RoundDown_P1_P3 =  Dict
_test_Rem_RoundDown_P1_P3 :: Dict (K.Rem 'K.RoundDown (P 1) (P 3) ~ P 1)
_test_Rem_RoundDown_P1_P3 =  Dict
_test_Div_RoundDown_P1_P4 :: Dict (K.Div 'K.RoundDown (P 1) (P 4) ~ Z)
_test_Div_RoundDown_P1_P4 =  Dict
_test_Rem_RoundDown_P1_P4 :: Dict (K.Rem 'K.RoundDown (P 1) (P 4) ~ P 1)
_test_Rem_RoundDown_P1_P4 =  Dict
_test_Div_RoundDown_P2_N1 :: Dict (K.Div 'K.RoundDown (P 2) (N 1) ~ N 2)
_test_Div_RoundDown_P2_N1 =  Dict
_test_Rem_RoundDown_P2_N1 :: Dict (K.Rem 'K.RoundDown (P 2) (N 1) ~ Z)
_test_Rem_RoundDown_P2_N1 =  Dict
_test_Div_RoundDown_P2_N2 :: Dict (K.Div 'K.RoundDown (P 2) (N 2) ~ N 1)
_test_Div_RoundDown_P2_N2 =  Dict
_test_Rem_RoundDown_P2_N2 :: Dict (K.Rem 'K.RoundDown (P 2) (N 2) ~ Z)
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
_test_Rem_RoundDown_P2_P1 :: Dict (K.Rem 'K.RoundDown (P 2) (P 1) ~ Z)
_test_Rem_RoundDown_P2_P1 =  Dict
_test_Div_RoundDown_P2_P2 :: Dict (K.Div 'K.RoundDown (P 2) (P 2) ~ P 1)
_test_Div_RoundDown_P2_P2 =  Dict
_test_Rem_RoundDown_P2_P2 :: Dict (K.Rem 'K.RoundDown (P 2) (P 2) ~ Z)
_test_Rem_RoundDown_P2_P2 =  Dict
_test_Div_RoundDown_P2_P3 :: Dict (K.Div 'K.RoundDown (P 2) (P 3) ~ Z)
_test_Div_RoundDown_P2_P3 =  Dict
_test_Rem_RoundDown_P2_P3 :: Dict (K.Rem 'K.RoundDown (P 2) (P 3) ~ P 2)
_test_Rem_RoundDown_P2_P3 =  Dict
_test_Div_RoundDown_P2_P4 :: Dict (K.Div 'K.RoundDown (P 2) (P 4) ~ Z)
_test_Div_RoundDown_P2_P4 =  Dict
_test_Rem_RoundDown_P2_P4 :: Dict (K.Rem 'K.RoundDown (P 2) (P 4) ~ P 2)
_test_Rem_RoundDown_P2_P4 =  Dict
_test_Div_RoundDown_P3_N1 :: Dict (K.Div 'K.RoundDown (P 3) (N 1) ~ N 3)
_test_Div_RoundDown_P3_N1 =  Dict
_test_Rem_RoundDown_P3_N1 :: Dict (K.Rem 'K.RoundDown (P 3) (N 1) ~ Z)
_test_Rem_RoundDown_P3_N1 =  Dict
_test_Div_RoundDown_P3_N2 :: Dict (K.Div 'K.RoundDown (P 3) (N 2) ~ N 2)
_test_Div_RoundDown_P3_N2 =  Dict
_test_Rem_RoundDown_P3_N2 :: Dict (K.Rem 'K.RoundDown (P 3) (N 2) ~ N 1)
_test_Rem_RoundDown_P3_N2 =  Dict
_test_Div_RoundDown_P3_N3 :: Dict (K.Div 'K.RoundDown (P 3) (N 3) ~ N 1)
_test_Div_RoundDown_P3_N3 =  Dict
_test_Rem_RoundDown_P3_N3 :: Dict (K.Rem 'K.RoundDown (P 3) (N 3) ~ Z)
_test_Rem_RoundDown_P3_N3 =  Dict
_test_Div_RoundDown_P3_N4 :: Dict (K.Div 'K.RoundDown (P 3) (N 4) ~ N 1)
_test_Div_RoundDown_P3_N4 =  Dict
_test_Rem_RoundDown_P3_N4 :: Dict (K.Rem 'K.RoundDown (P 3) (N 4) ~ N 1)
_test_Rem_RoundDown_P3_N4 =  Dict
_test_Div_RoundDown_P3_P1 :: Dict (K.Div 'K.RoundDown (P 3) (P 1) ~ P 3)
_test_Div_RoundDown_P3_P1 =  Dict
_test_Rem_RoundDown_P3_P1 :: Dict (K.Rem 'K.RoundDown (P 3) (P 1) ~ Z)
_test_Rem_RoundDown_P3_P1 =  Dict
_test_Div_RoundDown_P3_P2 :: Dict (K.Div 'K.RoundDown (P 3) (P 2) ~ P 1)
_test_Div_RoundDown_P3_P2 =  Dict
_test_Rem_RoundDown_P3_P2 :: Dict (K.Rem 'K.RoundDown (P 3) (P 2) ~ P 1)
_test_Rem_RoundDown_P3_P2 =  Dict
_test_Div_RoundDown_P3_P3 :: Dict (K.Div 'K.RoundDown (P 3) (P 3) ~ P 1)
_test_Div_RoundDown_P3_P3 =  Dict
_test_Rem_RoundDown_P3_P3 :: Dict (K.Rem 'K.RoundDown (P 3) (P 3) ~ Z)
_test_Rem_RoundDown_P3_P3 =  Dict
_test_Div_RoundDown_P3_P4 :: Dict (K.Div 'K.RoundDown (P 3) (P 4) ~ Z)
_test_Div_RoundDown_P3_P4 =  Dict
_test_Rem_RoundDown_P3_P4 :: Dict (K.Rem 'K.RoundDown (P 3) (P 4) ~ P 3)
_test_Rem_RoundDown_P3_P4 =  Dict
_test_Div_RoundDown_P4_N1 :: Dict (K.Div 'K.RoundDown (P 4) (N 1) ~ N 4)
_test_Div_RoundDown_P4_N1 =  Dict
_test_Rem_RoundDown_P4_N1 :: Dict (K.Rem 'K.RoundDown (P 4) (N 1) ~ Z)
_test_Rem_RoundDown_P4_N1 =  Dict
_test_Div_RoundDown_P4_N2 :: Dict (K.Div 'K.RoundDown (P 4) (N 2) ~ N 2)
_test_Div_RoundDown_P4_N2 =  Dict
_test_Rem_RoundDown_P4_N2 :: Dict (K.Rem 'K.RoundDown (P 4) (N 2) ~ Z)
_test_Rem_RoundDown_P4_N2 =  Dict
_test_Div_RoundDown_P4_N3 :: Dict (K.Div 'K.RoundDown (P 4) (N 3) ~ N 2)
_test_Div_RoundDown_P4_N3 =  Dict
_test_Rem_RoundDown_P4_N3 :: Dict (K.Rem 'K.RoundDown (P 4) (N 3) ~ N 2)
_test_Rem_RoundDown_P4_N3 =  Dict
_test_Div_RoundDown_P4_N4 :: Dict (K.Div 'K.RoundDown (P 4) (N 4) ~ N 1)
_test_Div_RoundDown_P4_N4 =  Dict
_test_Rem_RoundDown_P4_N4 :: Dict (K.Rem 'K.RoundDown (P 4) (N 4) ~ Z)
_test_Rem_RoundDown_P4_N4 =  Dict
_test_Div_RoundDown_P4_P1 :: Dict (K.Div 'K.RoundDown (P 4) (P 1) ~ P 4)
_test_Div_RoundDown_P4_P1 =  Dict
_test_Rem_RoundDown_P4_P1 :: Dict (K.Rem 'K.RoundDown (P 4) (P 1) ~ Z)
_test_Rem_RoundDown_P4_P1 =  Dict
_test_Div_RoundDown_P4_P2 :: Dict (K.Div 'K.RoundDown (P 4) (P 2) ~ P 2)
_test_Div_RoundDown_P4_P2 =  Dict
_test_Rem_RoundDown_P4_P2 :: Dict (K.Rem 'K.RoundDown (P 4) (P 2) ~ Z)
_test_Rem_RoundDown_P4_P2 =  Dict
_test_Div_RoundDown_P4_P3 :: Dict (K.Div 'K.RoundDown (P 4) (P 3) ~ P 1)
_test_Div_RoundDown_P4_P3 =  Dict
_test_Rem_RoundDown_P4_P3 :: Dict (K.Rem 'K.RoundDown (P 4) (P 3) ~ P 1)
_test_Rem_RoundDown_P4_P3 =  Dict
_test_Div_RoundDown_P4_P4 :: Dict (K.Div 'K.RoundDown (P 4) (P 4) ~ P 1)
_test_Div_RoundDown_P4_P4 =  Dict
_test_Rem_RoundDown_P4_P4 :: Dict (K.Rem 'K.RoundDown (P 4) (P 4) ~ Z)
_test_Rem_RoundDown_P4_P4 =  Dict
_test_Div_RoundHalfAway_N1_N1 :: Dict (K.Div 'K.RoundHalfAway (N 1) (N 1) ~ P 1)
_test_Div_RoundHalfAway_N1_N1 =  Dict
_test_Rem_RoundHalfAway_N1_N1 :: Dict (K.Rem 'K.RoundHalfAway (N 1) (N 1) ~ Z)
_test_Rem_RoundHalfAway_N1_N1 =  Dict
_test_Div_RoundHalfAway_N1_N2 :: Dict (K.Div 'K.RoundHalfAway (N 1) (N 2) ~ P 1)
_test_Div_RoundHalfAway_N1_N2 =  Dict
_test_Rem_RoundHalfAway_N1_N2 :: Dict (K.Rem 'K.RoundHalfAway (N 1) (N 2) ~ P 1)
_test_Rem_RoundHalfAway_N1_N2 =  Dict
_test_Div_RoundHalfAway_N1_N3 :: Dict (K.Div 'K.RoundHalfAway (N 1) (N 3) ~ Z)
_test_Div_RoundHalfAway_N1_N3 =  Dict
_test_Rem_RoundHalfAway_N1_N3 :: Dict (K.Rem 'K.RoundHalfAway (N 1) (N 3) ~ N 1)
_test_Rem_RoundHalfAway_N1_N3 =  Dict
_test_Div_RoundHalfAway_N1_N4 :: Dict (K.Div 'K.RoundHalfAway (N 1) (N 4) ~ Z)
_test_Div_RoundHalfAway_N1_N4 =  Dict
_test_Rem_RoundHalfAway_N1_N4 :: Dict (K.Rem 'K.RoundHalfAway (N 1) (N 4) ~ N 1)
_test_Rem_RoundHalfAway_N1_N4 =  Dict
_test_Div_RoundHalfAway_N1_P1 :: Dict (K.Div 'K.RoundHalfAway (N 1) (P 1) ~ N 1)
_test_Div_RoundHalfAway_N1_P1 =  Dict
_test_Rem_RoundHalfAway_N1_P1 :: Dict (K.Rem 'K.RoundHalfAway (N 1) (P 1) ~ Z)
_test_Rem_RoundHalfAway_N1_P1 =  Dict
_test_Div_RoundHalfAway_N1_P2 :: Dict (K.Div 'K.RoundHalfAway (N 1) (P 2) ~ N 1)
_test_Div_RoundHalfAway_N1_P2 =  Dict
_test_Rem_RoundHalfAway_N1_P2 :: Dict (K.Rem 'K.RoundHalfAway (N 1) (P 2) ~ P 1)
_test_Rem_RoundHalfAway_N1_P2 =  Dict
_test_Div_RoundHalfAway_N1_P3 :: Dict (K.Div 'K.RoundHalfAway (N 1) (P 3) ~ Z)
_test_Div_RoundHalfAway_N1_P3 =  Dict
_test_Rem_RoundHalfAway_N1_P3 :: Dict (K.Rem 'K.RoundHalfAway (N 1) (P 3) ~ N 1)
_test_Rem_RoundHalfAway_N1_P3 =  Dict
_test_Div_RoundHalfAway_N1_P4 :: Dict (K.Div 'K.RoundHalfAway (N 1) (P 4) ~ Z)
_test_Div_RoundHalfAway_N1_P4 =  Dict
_test_Rem_RoundHalfAway_N1_P4 :: Dict (K.Rem 'K.RoundHalfAway (N 1) (P 4) ~ N 1)
_test_Rem_RoundHalfAway_N1_P4 =  Dict
_test_Div_RoundHalfAway_N2_N1 :: Dict (K.Div 'K.RoundHalfAway (N 2) (N 1) ~ P 2)
_test_Div_RoundHalfAway_N2_N1 =  Dict
_test_Rem_RoundHalfAway_N2_N1 :: Dict (K.Rem 'K.RoundHalfAway (N 2) (N 1) ~ Z)
_test_Rem_RoundHalfAway_N2_N1 =  Dict
_test_Div_RoundHalfAway_N2_N2 :: Dict (K.Div 'K.RoundHalfAway (N 2) (N 2) ~ P 1)
_test_Div_RoundHalfAway_N2_N2 =  Dict
_test_Rem_RoundHalfAway_N2_N2 :: Dict (K.Rem 'K.RoundHalfAway (N 2) (N 2) ~ Z)
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
_test_Rem_RoundHalfAway_N2_P1 :: Dict (K.Rem 'K.RoundHalfAway (N 2) (P 1) ~ Z)
_test_Rem_RoundHalfAway_N2_P1 =  Dict
_test_Div_RoundHalfAway_N2_P2 :: Dict (K.Div 'K.RoundHalfAway (N 2) (P 2) ~ N 1)
_test_Div_RoundHalfAway_N2_P2 =  Dict
_test_Rem_RoundHalfAway_N2_P2 :: Dict (K.Rem 'K.RoundHalfAway (N 2) (P 2) ~ Z)
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
_test_Rem_RoundHalfAway_N3_N1 :: Dict (K.Rem 'K.RoundHalfAway (N 3) (N 1) ~ Z)
_test_Rem_RoundHalfAway_N3_N1 =  Dict
_test_Div_RoundHalfAway_N3_N2 :: Dict (K.Div 'K.RoundHalfAway (N 3) (N 2) ~ P 2)
_test_Div_RoundHalfAway_N3_N2 =  Dict
_test_Rem_RoundHalfAway_N3_N2 :: Dict (K.Rem 'K.RoundHalfAway (N 3) (N 2) ~ P 1)
_test_Rem_RoundHalfAway_N3_N2 =  Dict
_test_Div_RoundHalfAway_N3_N3 :: Dict (K.Div 'K.RoundHalfAway (N 3) (N 3) ~ P 1)
_test_Div_RoundHalfAway_N3_N3 =  Dict
_test_Rem_RoundHalfAway_N3_N3 :: Dict (K.Rem 'K.RoundHalfAway (N 3) (N 3) ~ Z)
_test_Rem_RoundHalfAway_N3_N3 =  Dict
_test_Div_RoundHalfAway_N3_N4 :: Dict (K.Div 'K.RoundHalfAway (N 3) (N 4) ~ P 1)
_test_Div_RoundHalfAway_N3_N4 =  Dict
_test_Rem_RoundHalfAway_N3_N4 :: Dict (K.Rem 'K.RoundHalfAway (N 3) (N 4) ~ P 1)
_test_Rem_RoundHalfAway_N3_N4 =  Dict
_test_Div_RoundHalfAway_N3_P1 :: Dict (K.Div 'K.RoundHalfAway (N 3) (P 1) ~ N 3)
_test_Div_RoundHalfAway_N3_P1 =  Dict
_test_Rem_RoundHalfAway_N3_P1 :: Dict (K.Rem 'K.RoundHalfAway (N 3) (P 1) ~ Z)
_test_Rem_RoundHalfAway_N3_P1 =  Dict
_test_Div_RoundHalfAway_N3_P2 :: Dict (K.Div 'K.RoundHalfAway (N 3) (P 2) ~ N 2)
_test_Div_RoundHalfAway_N3_P2 =  Dict
_test_Rem_RoundHalfAway_N3_P2 :: Dict (K.Rem 'K.RoundHalfAway (N 3) (P 2) ~ P 1)
_test_Rem_RoundHalfAway_N3_P2 =  Dict
_test_Div_RoundHalfAway_N3_P3 :: Dict (K.Div 'K.RoundHalfAway (N 3) (P 3) ~ N 1)
_test_Div_RoundHalfAway_N3_P3 =  Dict
_test_Rem_RoundHalfAway_N3_P3 :: Dict (K.Rem 'K.RoundHalfAway (N 3) (P 3) ~ Z)
_test_Rem_RoundHalfAway_N3_P3 =  Dict
_test_Div_RoundHalfAway_N3_P4 :: Dict (K.Div 'K.RoundHalfAway (N 3) (P 4) ~ N 1)
_test_Div_RoundHalfAway_N3_P4 =  Dict
_test_Rem_RoundHalfAway_N3_P4 :: Dict (K.Rem 'K.RoundHalfAway (N 3) (P 4) ~ P 1)
_test_Rem_RoundHalfAway_N3_P4 =  Dict
_test_Div_RoundHalfAway_N4_N1 :: Dict (K.Div 'K.RoundHalfAway (N 4) (N 1) ~ P 4)
_test_Div_RoundHalfAway_N4_N1 =  Dict
_test_Rem_RoundHalfAway_N4_N1 :: Dict (K.Rem 'K.RoundHalfAway (N 4) (N 1) ~ Z)
_test_Rem_RoundHalfAway_N4_N1 =  Dict
_test_Div_RoundHalfAway_N4_N2 :: Dict (K.Div 'K.RoundHalfAway (N 4) (N 2) ~ P 2)
_test_Div_RoundHalfAway_N4_N2 =  Dict
_test_Rem_RoundHalfAway_N4_N2 :: Dict (K.Rem 'K.RoundHalfAway (N 4) (N 2) ~ Z)
_test_Rem_RoundHalfAway_N4_N2 =  Dict
_test_Div_RoundHalfAway_N4_N3 :: Dict (K.Div 'K.RoundHalfAway (N 4) (N 3) ~ P 1)
_test_Div_RoundHalfAway_N4_N3 =  Dict
_test_Rem_RoundHalfAway_N4_N3 :: Dict (K.Rem 'K.RoundHalfAway (N 4) (N 3) ~ N 1)
_test_Rem_RoundHalfAway_N4_N3 =  Dict
_test_Div_RoundHalfAway_N4_N4 :: Dict (K.Div 'K.RoundHalfAway (N 4) (N 4) ~ P 1)
_test_Div_RoundHalfAway_N4_N4 =  Dict
_test_Rem_RoundHalfAway_N4_N4 :: Dict (K.Rem 'K.RoundHalfAway (N 4) (N 4) ~ Z)
_test_Rem_RoundHalfAway_N4_N4 =  Dict
_test_Div_RoundHalfAway_N4_P1 :: Dict (K.Div 'K.RoundHalfAway (N 4) (P 1) ~ N 4)
_test_Div_RoundHalfAway_N4_P1 =  Dict
_test_Rem_RoundHalfAway_N4_P1 :: Dict (K.Rem 'K.RoundHalfAway (N 4) (P 1) ~ Z)
_test_Rem_RoundHalfAway_N4_P1 =  Dict
_test_Div_RoundHalfAway_N4_P2 :: Dict (K.Div 'K.RoundHalfAway (N 4) (P 2) ~ N 2)
_test_Div_RoundHalfAway_N4_P2 =  Dict
_test_Rem_RoundHalfAway_N4_P2 :: Dict (K.Rem 'K.RoundHalfAway (N 4) (P 2) ~ Z)
_test_Rem_RoundHalfAway_N4_P2 =  Dict
_test_Div_RoundHalfAway_N4_P3 :: Dict (K.Div 'K.RoundHalfAway (N 4) (P 3) ~ N 1)
_test_Div_RoundHalfAway_N4_P3 =  Dict
_test_Rem_RoundHalfAway_N4_P3 :: Dict (K.Rem 'K.RoundHalfAway (N 4) (P 3) ~ N 1)
_test_Rem_RoundHalfAway_N4_P3 =  Dict
_test_Div_RoundHalfAway_N4_P4 :: Dict (K.Div 'K.RoundHalfAway (N 4) (P 4) ~ N 1)
_test_Div_RoundHalfAway_N4_P4 =  Dict
_test_Rem_RoundHalfAway_N4_P4 :: Dict (K.Rem 'K.RoundHalfAway (N 4) (P 4) ~ Z)
_test_Rem_RoundHalfAway_N4_P4 =  Dict
_test_Rem_RoundHalfAway_Z_N1 :: Dict (K.Rem 'K.RoundHalfAway Z (N 1) ~ Z)
_test_Rem_RoundHalfAway_Z_N1 =  Dict
_test_Rem_RoundHalfAway_Z_N2 :: Dict (K.Rem 'K.RoundHalfAway Z (N 2) ~ Z)
_test_Rem_RoundHalfAway_Z_N2 =  Dict
_test_Rem_RoundHalfAway_Z_N3 :: Dict (K.Rem 'K.RoundHalfAway Z (N 3) ~ Z)
_test_Rem_RoundHalfAway_Z_N3 =  Dict
_test_Rem_RoundHalfAway_Z_N4 :: Dict (K.Rem 'K.RoundHalfAway Z (N 4) ~ Z)
_test_Rem_RoundHalfAway_Z_N4 =  Dict
_test_Rem_RoundHalfAway_Z_P1 :: Dict (K.Rem 'K.RoundHalfAway Z (P 1) ~ Z)
_test_Rem_RoundHalfAway_Z_P1 =  Dict
_test_Rem_RoundHalfAway_Z_P2 :: Dict (K.Rem 'K.RoundHalfAway Z (P 2) ~ Z)
_test_Rem_RoundHalfAway_Z_P2 =  Dict
_test_Rem_RoundHalfAway_Z_P3 :: Dict (K.Rem 'K.RoundHalfAway Z (P 3) ~ Z)
_test_Rem_RoundHalfAway_Z_P3 =  Dict
_test_Rem_RoundHalfAway_Z_P4 :: Dict (K.Rem 'K.RoundHalfAway Z (P 4) ~ Z)
_test_Rem_RoundHalfAway_Z_P4 =  Dict
_test_Div_RoundHalfAway_P1_N1 :: Dict (K.Div 'K.RoundHalfAway (P 1) (N 1) ~ N 1)
_test_Div_RoundHalfAway_P1_N1 =  Dict
_test_Rem_RoundHalfAway_P1_N1 :: Dict (K.Rem 'K.RoundHalfAway (P 1) (N 1) ~ Z)
_test_Rem_RoundHalfAway_P1_N1 =  Dict
_test_Div_RoundHalfAway_P1_N2 :: Dict (K.Div 'K.RoundHalfAway (P 1) (N 2) ~ N 1)
_test_Div_RoundHalfAway_P1_N2 =  Dict
_test_Rem_RoundHalfAway_P1_N2 :: Dict (K.Rem 'K.RoundHalfAway (P 1) (N 2) ~ N 1)
_test_Rem_RoundHalfAway_P1_N2 =  Dict
_test_Div_RoundHalfAway_P1_N3 :: Dict (K.Div 'K.RoundHalfAway (P 1) (N 3) ~ Z)
_test_Div_RoundHalfAway_P1_N3 =  Dict
_test_Rem_RoundHalfAway_P1_N3 :: Dict (K.Rem 'K.RoundHalfAway (P 1) (N 3) ~ P 1)
_test_Rem_RoundHalfAway_P1_N3 =  Dict
_test_Div_RoundHalfAway_P1_N4 :: Dict (K.Div 'K.RoundHalfAway (P 1) (N 4) ~ Z)
_test_Div_RoundHalfAway_P1_N4 =  Dict
_test_Rem_RoundHalfAway_P1_N4 :: Dict (K.Rem 'K.RoundHalfAway (P 1) (N 4) ~ P 1)
_test_Rem_RoundHalfAway_P1_N4 =  Dict
_test_Div_RoundHalfAway_P1_P1 :: Dict (K.Div 'K.RoundHalfAway (P 1) (P 1) ~ P 1)
_test_Div_RoundHalfAway_P1_P1 =  Dict
_test_Rem_RoundHalfAway_P1_P1 :: Dict (K.Rem 'K.RoundHalfAway (P 1) (P 1) ~ Z)
_test_Rem_RoundHalfAway_P1_P1 =  Dict
_test_Div_RoundHalfAway_P1_P2 :: Dict (K.Div 'K.RoundHalfAway (P 1) (P 2) ~ P 1)
_test_Div_RoundHalfAway_P1_P2 =  Dict
_test_Rem_RoundHalfAway_P1_P2 :: Dict (K.Rem 'K.RoundHalfAway (P 1) (P 2) ~ N 1)
_test_Rem_RoundHalfAway_P1_P2 =  Dict
_test_Div_RoundHalfAway_P1_P3 :: Dict (K.Div 'K.RoundHalfAway (P 1) (P 3) ~ Z)
_test_Div_RoundHalfAway_P1_P3 =  Dict
_test_Rem_RoundHalfAway_P1_P3 :: Dict (K.Rem 'K.RoundHalfAway (P 1) (P 3) ~ P 1)
_test_Rem_RoundHalfAway_P1_P3 =  Dict
_test_Div_RoundHalfAway_P1_P4 :: Dict (K.Div 'K.RoundHalfAway (P 1) (P 4) ~ Z)
_test_Div_RoundHalfAway_P1_P4 =  Dict
_test_Rem_RoundHalfAway_P1_P4 :: Dict (K.Rem 'K.RoundHalfAway (P 1) (P 4) ~ P 1)
_test_Rem_RoundHalfAway_P1_P4 =  Dict
_test_Div_RoundHalfAway_P2_N1 :: Dict (K.Div 'K.RoundHalfAway (P 2) (N 1) ~ N 2)
_test_Div_RoundHalfAway_P2_N1 =  Dict
_test_Rem_RoundHalfAway_P2_N1 :: Dict (K.Rem 'K.RoundHalfAway (P 2) (N 1) ~ Z)
_test_Rem_RoundHalfAway_P2_N1 =  Dict
_test_Div_RoundHalfAway_P2_N2 :: Dict (K.Div 'K.RoundHalfAway (P 2) (N 2) ~ N 1)
_test_Div_RoundHalfAway_P2_N2 =  Dict
_test_Rem_RoundHalfAway_P2_N2 :: Dict (K.Rem 'K.RoundHalfAway (P 2) (N 2) ~ Z)
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
_test_Rem_RoundHalfAway_P2_P1 :: Dict (K.Rem 'K.RoundHalfAway (P 2) (P 1) ~ Z)
_test_Rem_RoundHalfAway_P2_P1 =  Dict
_test_Div_RoundHalfAway_P2_P2 :: Dict (K.Div 'K.RoundHalfAway (P 2) (P 2) ~ P 1)
_test_Div_RoundHalfAway_P2_P2 =  Dict
_test_Rem_RoundHalfAway_P2_P2 :: Dict (K.Rem 'K.RoundHalfAway (P 2) (P 2) ~ Z)
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
_test_Rem_RoundHalfAway_P3_N1 :: Dict (K.Rem 'K.RoundHalfAway (P 3) (N 1) ~ Z)
_test_Rem_RoundHalfAway_P3_N1 =  Dict
_test_Div_RoundHalfAway_P3_N2 :: Dict (K.Div 'K.RoundHalfAway (P 3) (N 2) ~ N 2)
_test_Div_RoundHalfAway_P3_N2 =  Dict
_test_Rem_RoundHalfAway_P3_N2 :: Dict (K.Rem 'K.RoundHalfAway (P 3) (N 2) ~ N 1)
_test_Rem_RoundHalfAway_P3_N2 =  Dict
_test_Div_RoundHalfAway_P3_N3 :: Dict (K.Div 'K.RoundHalfAway (P 3) (N 3) ~ N 1)
_test_Div_RoundHalfAway_P3_N3 =  Dict
_test_Rem_RoundHalfAway_P3_N3 :: Dict (K.Rem 'K.RoundHalfAway (P 3) (N 3) ~ Z)
_test_Rem_RoundHalfAway_P3_N3 =  Dict
_test_Div_RoundHalfAway_P3_N4 :: Dict (K.Div 'K.RoundHalfAway (P 3) (N 4) ~ N 1)
_test_Div_RoundHalfAway_P3_N4 =  Dict
_test_Rem_RoundHalfAway_P3_N4 :: Dict (K.Rem 'K.RoundHalfAway (P 3) (N 4) ~ N 1)
_test_Rem_RoundHalfAway_P3_N4 =  Dict
_test_Div_RoundHalfAway_P3_P1 :: Dict (K.Div 'K.RoundHalfAway (P 3) (P 1) ~ P 3)
_test_Div_RoundHalfAway_P3_P1 =  Dict
_test_Rem_RoundHalfAway_P3_P1 :: Dict (K.Rem 'K.RoundHalfAway (P 3) (P 1) ~ Z)
_test_Rem_RoundHalfAway_P3_P1 =  Dict
_test_Div_RoundHalfAway_P3_P2 :: Dict (K.Div 'K.RoundHalfAway (P 3) (P 2) ~ P 2)
_test_Div_RoundHalfAway_P3_P2 =  Dict
_test_Rem_RoundHalfAway_P3_P2 :: Dict (K.Rem 'K.RoundHalfAway (P 3) (P 2) ~ N 1)
_test_Rem_RoundHalfAway_P3_P2 =  Dict
_test_Div_RoundHalfAway_P3_P3 :: Dict (K.Div 'K.RoundHalfAway (P 3) (P 3) ~ P 1)
_test_Div_RoundHalfAway_P3_P3 =  Dict
_test_Rem_RoundHalfAway_P3_P3 :: Dict (K.Rem 'K.RoundHalfAway (P 3) (P 3) ~ Z)
_test_Rem_RoundHalfAway_P3_P3 =  Dict
_test_Div_RoundHalfAway_P3_P4 :: Dict (K.Div 'K.RoundHalfAway (P 3) (P 4) ~ P 1)
_test_Div_RoundHalfAway_P3_P4 =  Dict
_test_Rem_RoundHalfAway_P3_P4 :: Dict (K.Rem 'K.RoundHalfAway (P 3) (P 4) ~ N 1)
_test_Rem_RoundHalfAway_P3_P4 =  Dict
_test_Div_RoundHalfAway_P4_N1 :: Dict (K.Div 'K.RoundHalfAway (P 4) (N 1) ~ N 4)
_test_Div_RoundHalfAway_P4_N1 =  Dict
_test_Rem_RoundHalfAway_P4_N1 :: Dict (K.Rem 'K.RoundHalfAway (P 4) (N 1) ~ Z)
_test_Rem_RoundHalfAway_P4_N1 =  Dict
_test_Div_RoundHalfAway_P4_N2 :: Dict (K.Div 'K.RoundHalfAway (P 4) (N 2) ~ N 2)
_test_Div_RoundHalfAway_P4_N2 =  Dict
_test_Rem_RoundHalfAway_P4_N2 :: Dict (K.Rem 'K.RoundHalfAway (P 4) (N 2) ~ Z)
_test_Rem_RoundHalfAway_P4_N2 =  Dict
_test_Div_RoundHalfAway_P4_N3 :: Dict (K.Div 'K.RoundHalfAway (P 4) (N 3) ~ N 1)
_test_Div_RoundHalfAway_P4_N3 =  Dict
_test_Rem_RoundHalfAway_P4_N3 :: Dict (K.Rem 'K.RoundHalfAway (P 4) (N 3) ~ P 1)
_test_Rem_RoundHalfAway_P4_N3 =  Dict
_test_Div_RoundHalfAway_P4_N4 :: Dict (K.Div 'K.RoundHalfAway (P 4) (N 4) ~ N 1)
_test_Div_RoundHalfAway_P4_N4 =  Dict
_test_Rem_RoundHalfAway_P4_N4 :: Dict (K.Rem 'K.RoundHalfAway (P 4) (N 4) ~ Z)
_test_Rem_RoundHalfAway_P4_N4 =  Dict
_test_Div_RoundHalfAway_P4_P1 :: Dict (K.Div 'K.RoundHalfAway (P 4) (P 1) ~ P 4)
_test_Div_RoundHalfAway_P4_P1 =  Dict
_test_Rem_RoundHalfAway_P4_P1 :: Dict (K.Rem 'K.RoundHalfAway (P 4) (P 1) ~ Z)
_test_Rem_RoundHalfAway_P4_P1 =  Dict
_test_Div_RoundHalfAway_P4_P2 :: Dict (K.Div 'K.RoundHalfAway (P 4) (P 2) ~ P 2)
_test_Div_RoundHalfAway_P4_P2 =  Dict
_test_Rem_RoundHalfAway_P4_P2 :: Dict (K.Rem 'K.RoundHalfAway (P 4) (P 2) ~ Z)
_test_Rem_RoundHalfAway_P4_P2 =  Dict
_test_Div_RoundHalfAway_P4_P3 :: Dict (K.Div 'K.RoundHalfAway (P 4) (P 3) ~ P 1)
_test_Div_RoundHalfAway_P4_P3 =  Dict
_test_Rem_RoundHalfAway_P4_P3 :: Dict (K.Rem 'K.RoundHalfAway (P 4) (P 3) ~ P 1)
_test_Rem_RoundHalfAway_P4_P3 =  Dict
_test_Div_RoundHalfAway_P4_P4 :: Dict (K.Div 'K.RoundHalfAway (P 4) (P 4) ~ P 1)
_test_Div_RoundHalfAway_P4_P4 =  Dict
_test_Rem_RoundHalfAway_P4_P4 :: Dict (K.Rem 'K.RoundHalfAway (P 4) (P 4) ~ Z)
_test_Rem_RoundHalfAway_P4_P4 =  Dict
_test_Div_RoundHalfDown_N1_N1 :: Dict (K.Div 'K.RoundHalfDown (N 1) (N 1) ~ P 1)
_test_Div_RoundHalfDown_N1_N1 =  Dict
_test_Rem_RoundHalfDown_N1_N1 :: Dict (K.Rem 'K.RoundHalfDown (N 1) (N 1) ~ Z)
_test_Rem_RoundHalfDown_N1_N1 =  Dict
_test_Div_RoundHalfDown_N1_N2 :: Dict (K.Div 'K.RoundHalfDown (N 1) (N 2) ~ Z)
_test_Div_RoundHalfDown_N1_N2 =  Dict
_test_Rem_RoundHalfDown_N1_N2 :: Dict (K.Rem 'K.RoundHalfDown (N 1) (N 2) ~ N 1)
_test_Rem_RoundHalfDown_N1_N2 =  Dict
_test_Div_RoundHalfDown_N1_N3 :: Dict (K.Div 'K.RoundHalfDown (N 1) (N 3) ~ Z)
_test_Div_RoundHalfDown_N1_N3 =  Dict
_test_Rem_RoundHalfDown_N1_N3 :: Dict (K.Rem 'K.RoundHalfDown (N 1) (N 3) ~ N 1)
_test_Rem_RoundHalfDown_N1_N3 =  Dict
_test_Div_RoundHalfDown_N1_N4 :: Dict (K.Div 'K.RoundHalfDown (N 1) (N 4) ~ Z)
_test_Div_RoundHalfDown_N1_N4 =  Dict
_test_Rem_RoundHalfDown_N1_N4 :: Dict (K.Rem 'K.RoundHalfDown (N 1) (N 4) ~ N 1)
_test_Rem_RoundHalfDown_N1_N4 =  Dict
_test_Div_RoundHalfDown_N1_P1 :: Dict (K.Div 'K.RoundHalfDown (N 1) (P 1) ~ N 1)
_test_Div_RoundHalfDown_N1_P1 =  Dict
_test_Rem_RoundHalfDown_N1_P1 :: Dict (K.Rem 'K.RoundHalfDown (N 1) (P 1) ~ Z)
_test_Rem_RoundHalfDown_N1_P1 =  Dict
_test_Div_RoundHalfDown_N1_P2 :: Dict (K.Div 'K.RoundHalfDown (N 1) (P 2) ~ N 1)
_test_Div_RoundHalfDown_N1_P2 =  Dict
_test_Rem_RoundHalfDown_N1_P2 :: Dict (K.Rem 'K.RoundHalfDown (N 1) (P 2) ~ P 1)
_test_Rem_RoundHalfDown_N1_P2 =  Dict
_test_Div_RoundHalfDown_N1_P3 :: Dict (K.Div 'K.RoundHalfDown (N 1) (P 3) ~ Z)
_test_Div_RoundHalfDown_N1_P3 =  Dict
_test_Rem_RoundHalfDown_N1_P3 :: Dict (K.Rem 'K.RoundHalfDown (N 1) (P 3) ~ N 1)
_test_Rem_RoundHalfDown_N1_P3 =  Dict
_test_Div_RoundHalfDown_N1_P4 :: Dict (K.Div 'K.RoundHalfDown (N 1) (P 4) ~ Z)
_test_Div_RoundHalfDown_N1_P4 =  Dict
_test_Rem_RoundHalfDown_N1_P4 :: Dict (K.Rem 'K.RoundHalfDown (N 1) (P 4) ~ N 1)
_test_Rem_RoundHalfDown_N1_P4 =  Dict
_test_Div_RoundHalfDown_N2_N1 :: Dict (K.Div 'K.RoundHalfDown (N 2) (N 1) ~ P 2)
_test_Div_RoundHalfDown_N2_N1 =  Dict
_test_Rem_RoundHalfDown_N2_N1 :: Dict (K.Rem 'K.RoundHalfDown (N 2) (N 1) ~ Z)
_test_Rem_RoundHalfDown_N2_N1 =  Dict
_test_Div_RoundHalfDown_N2_N2 :: Dict (K.Div 'K.RoundHalfDown (N 2) (N 2) ~ P 1)
_test_Div_RoundHalfDown_N2_N2 =  Dict
_test_Rem_RoundHalfDown_N2_N2 :: Dict (K.Rem 'K.RoundHalfDown (N 2) (N 2) ~ Z)
_test_Rem_RoundHalfDown_N2_N2 =  Dict
_test_Div_RoundHalfDown_N2_N3 :: Dict (K.Div 'K.RoundHalfDown (N 2) (N 3) ~ P 1)
_test_Div_RoundHalfDown_N2_N3 =  Dict
_test_Rem_RoundHalfDown_N2_N3 :: Dict (K.Rem 'K.RoundHalfDown (N 2) (N 3) ~ P 1)
_test_Rem_RoundHalfDown_N2_N3 =  Dict
_test_Div_RoundHalfDown_N2_N4 :: Dict (K.Div 'K.RoundHalfDown (N 2) (N 4) ~ Z)
_test_Div_RoundHalfDown_N2_N4 =  Dict
_test_Rem_RoundHalfDown_N2_N4 :: Dict (K.Rem 'K.RoundHalfDown (N 2) (N 4) ~ N 2)
_test_Rem_RoundHalfDown_N2_N4 =  Dict
_test_Div_RoundHalfDown_N2_P1 :: Dict (K.Div 'K.RoundHalfDown (N 2) (P 1) ~ N 2)
_test_Div_RoundHalfDown_N2_P1 =  Dict
_test_Rem_RoundHalfDown_N2_P1 :: Dict (K.Rem 'K.RoundHalfDown (N 2) (P 1) ~ Z)
_test_Rem_RoundHalfDown_N2_P1 =  Dict
_test_Div_RoundHalfDown_N2_P2 :: Dict (K.Div 'K.RoundHalfDown (N 2) (P 2) ~ N 1)
_test_Div_RoundHalfDown_N2_P2 =  Dict
_test_Rem_RoundHalfDown_N2_P2 :: Dict (K.Rem 'K.RoundHalfDown (N 2) (P 2) ~ Z)
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
_test_Rem_RoundHalfDown_N3_N1 :: Dict (K.Rem 'K.RoundHalfDown (N 3) (N 1) ~ Z)
_test_Rem_RoundHalfDown_N3_N1 =  Dict
_test_Div_RoundHalfDown_N3_N2 :: Dict (K.Div 'K.RoundHalfDown (N 3) (N 2) ~ P 1)
_test_Div_RoundHalfDown_N3_N2 =  Dict
_test_Rem_RoundHalfDown_N3_N2 :: Dict (K.Rem 'K.RoundHalfDown (N 3) (N 2) ~ N 1)
_test_Rem_RoundHalfDown_N3_N2 =  Dict
_test_Div_RoundHalfDown_N3_N3 :: Dict (K.Div 'K.RoundHalfDown (N 3) (N 3) ~ P 1)
_test_Div_RoundHalfDown_N3_N3 =  Dict
_test_Rem_RoundHalfDown_N3_N3 :: Dict (K.Rem 'K.RoundHalfDown (N 3) (N 3) ~ Z)
_test_Rem_RoundHalfDown_N3_N3 =  Dict
_test_Div_RoundHalfDown_N3_N4 :: Dict (K.Div 'K.RoundHalfDown (N 3) (N 4) ~ P 1)
_test_Div_RoundHalfDown_N3_N4 =  Dict
_test_Rem_RoundHalfDown_N3_N4 :: Dict (K.Rem 'K.RoundHalfDown (N 3) (N 4) ~ P 1)
_test_Rem_RoundHalfDown_N3_N4 =  Dict
_test_Div_RoundHalfDown_N3_P1 :: Dict (K.Div 'K.RoundHalfDown (N 3) (P 1) ~ N 3)
_test_Div_RoundHalfDown_N3_P1 =  Dict
_test_Rem_RoundHalfDown_N3_P1 :: Dict (K.Rem 'K.RoundHalfDown (N 3) (P 1) ~ Z)
_test_Rem_RoundHalfDown_N3_P1 =  Dict
_test_Div_RoundHalfDown_N3_P2 :: Dict (K.Div 'K.RoundHalfDown (N 3) (P 2) ~ N 2)
_test_Div_RoundHalfDown_N3_P2 =  Dict
_test_Rem_RoundHalfDown_N3_P2 :: Dict (K.Rem 'K.RoundHalfDown (N 3) (P 2) ~ P 1)
_test_Rem_RoundHalfDown_N3_P2 =  Dict
_test_Div_RoundHalfDown_N3_P3 :: Dict (K.Div 'K.RoundHalfDown (N 3) (P 3) ~ N 1)
_test_Div_RoundHalfDown_N3_P3 =  Dict
_test_Rem_RoundHalfDown_N3_P3 :: Dict (K.Rem 'K.RoundHalfDown (N 3) (P 3) ~ Z)
_test_Rem_RoundHalfDown_N3_P3 =  Dict
_test_Div_RoundHalfDown_N3_P4 :: Dict (K.Div 'K.RoundHalfDown (N 3) (P 4) ~ N 1)
_test_Div_RoundHalfDown_N3_P4 =  Dict
_test_Rem_RoundHalfDown_N3_P4 :: Dict (K.Rem 'K.RoundHalfDown (N 3) (P 4) ~ P 1)
_test_Rem_RoundHalfDown_N3_P4 =  Dict
_test_Div_RoundHalfDown_N4_N1 :: Dict (K.Div 'K.RoundHalfDown (N 4) (N 1) ~ P 4)
_test_Div_RoundHalfDown_N4_N1 =  Dict
_test_Rem_RoundHalfDown_N4_N1 :: Dict (K.Rem 'K.RoundHalfDown (N 4) (N 1) ~ Z)
_test_Rem_RoundHalfDown_N4_N1 =  Dict
_test_Div_RoundHalfDown_N4_N2 :: Dict (K.Div 'K.RoundHalfDown (N 4) (N 2) ~ P 2)
_test_Div_RoundHalfDown_N4_N2 =  Dict
_test_Rem_RoundHalfDown_N4_N2 :: Dict (K.Rem 'K.RoundHalfDown (N 4) (N 2) ~ Z)
_test_Rem_RoundHalfDown_N4_N2 =  Dict
_test_Div_RoundHalfDown_N4_N3 :: Dict (K.Div 'K.RoundHalfDown (N 4) (N 3) ~ P 1)
_test_Div_RoundHalfDown_N4_N3 =  Dict
_test_Rem_RoundHalfDown_N4_N3 :: Dict (K.Rem 'K.RoundHalfDown (N 4) (N 3) ~ N 1)
_test_Rem_RoundHalfDown_N4_N3 =  Dict
_test_Div_RoundHalfDown_N4_N4 :: Dict (K.Div 'K.RoundHalfDown (N 4) (N 4) ~ P 1)
_test_Div_RoundHalfDown_N4_N4 =  Dict
_test_Rem_RoundHalfDown_N4_N4 :: Dict (K.Rem 'K.RoundHalfDown (N 4) (N 4) ~ Z)
_test_Rem_RoundHalfDown_N4_N4 =  Dict
_test_Div_RoundHalfDown_N4_P1 :: Dict (K.Div 'K.RoundHalfDown (N 4) (P 1) ~ N 4)
_test_Div_RoundHalfDown_N4_P1 =  Dict
_test_Rem_RoundHalfDown_N4_P1 :: Dict (K.Rem 'K.RoundHalfDown (N 4) (P 1) ~ Z)
_test_Rem_RoundHalfDown_N4_P1 =  Dict
_test_Div_RoundHalfDown_N4_P2 :: Dict (K.Div 'K.RoundHalfDown (N 4) (P 2) ~ N 2)
_test_Div_RoundHalfDown_N4_P2 =  Dict
_test_Rem_RoundHalfDown_N4_P2 :: Dict (K.Rem 'K.RoundHalfDown (N 4) (P 2) ~ Z)
_test_Rem_RoundHalfDown_N4_P2 =  Dict
_test_Div_RoundHalfDown_N4_P3 :: Dict (K.Div 'K.RoundHalfDown (N 4) (P 3) ~ N 1)
_test_Div_RoundHalfDown_N4_P3 =  Dict
_test_Rem_RoundHalfDown_N4_P3 :: Dict (K.Rem 'K.RoundHalfDown (N 4) (P 3) ~ N 1)
_test_Rem_RoundHalfDown_N4_P3 =  Dict
_test_Div_RoundHalfDown_N4_P4 :: Dict (K.Div 'K.RoundHalfDown (N 4) (P 4) ~ N 1)
_test_Div_RoundHalfDown_N4_P4 =  Dict
_test_Rem_RoundHalfDown_N4_P4 :: Dict (K.Rem 'K.RoundHalfDown (N 4) (P 4) ~ Z)
_test_Rem_RoundHalfDown_N4_P4 =  Dict
_test_Rem_RoundHalfDown_Z_N1 :: Dict (K.Rem 'K.RoundHalfDown Z (N 1) ~ Z)
_test_Rem_RoundHalfDown_Z_N1 =  Dict
_test_Rem_RoundHalfDown_Z_N2 :: Dict (K.Rem 'K.RoundHalfDown Z (N 2) ~ Z)
_test_Rem_RoundHalfDown_Z_N2 =  Dict
_test_Rem_RoundHalfDown_Z_N3 :: Dict (K.Rem 'K.RoundHalfDown Z (N 3) ~ Z)
_test_Rem_RoundHalfDown_Z_N3 =  Dict
_test_Rem_RoundHalfDown_Z_N4 :: Dict (K.Rem 'K.RoundHalfDown Z (N 4) ~ Z)
_test_Rem_RoundHalfDown_Z_N4 =  Dict
_test_Rem_RoundHalfDown_Z_P1 :: Dict (K.Rem 'K.RoundHalfDown Z (P 1) ~ Z)
_test_Rem_RoundHalfDown_Z_P1 =  Dict
_test_Rem_RoundHalfDown_Z_P2 :: Dict (K.Rem 'K.RoundHalfDown Z (P 2) ~ Z)
_test_Rem_RoundHalfDown_Z_P2 =  Dict
_test_Rem_RoundHalfDown_Z_P3 :: Dict (K.Rem 'K.RoundHalfDown Z (P 3) ~ Z)
_test_Rem_RoundHalfDown_Z_P3 =  Dict
_test_Rem_RoundHalfDown_Z_P4 :: Dict (K.Rem 'K.RoundHalfDown Z (P 4) ~ Z)
_test_Rem_RoundHalfDown_Z_P4 =  Dict
_test_Div_RoundHalfDown_P1_N1 :: Dict (K.Div 'K.RoundHalfDown (P 1) (N 1) ~ N 1)
_test_Div_RoundHalfDown_P1_N1 =  Dict
_test_Rem_RoundHalfDown_P1_N1 :: Dict (K.Rem 'K.RoundHalfDown (P 1) (N 1) ~ Z)
_test_Rem_RoundHalfDown_P1_N1 =  Dict
_test_Div_RoundHalfDown_P1_N2 :: Dict (K.Div 'K.RoundHalfDown (P 1) (N 2) ~ N 1)
_test_Div_RoundHalfDown_P1_N2 =  Dict
_test_Rem_RoundHalfDown_P1_N2 :: Dict (K.Rem 'K.RoundHalfDown (P 1) (N 2) ~ N 1)
_test_Rem_RoundHalfDown_P1_N2 =  Dict
_test_Div_RoundHalfDown_P1_N3 :: Dict (K.Div 'K.RoundHalfDown (P 1) (N 3) ~ Z)
_test_Div_RoundHalfDown_P1_N3 =  Dict
_test_Rem_RoundHalfDown_P1_N3 :: Dict (K.Rem 'K.RoundHalfDown (P 1) (N 3) ~ P 1)
_test_Rem_RoundHalfDown_P1_N3 =  Dict
_test_Div_RoundHalfDown_P1_N4 :: Dict (K.Div 'K.RoundHalfDown (P 1) (N 4) ~ Z)
_test_Div_RoundHalfDown_P1_N4 =  Dict
_test_Rem_RoundHalfDown_P1_N4 :: Dict (K.Rem 'K.RoundHalfDown (P 1) (N 4) ~ P 1)
_test_Rem_RoundHalfDown_P1_N4 =  Dict
_test_Div_RoundHalfDown_P1_P1 :: Dict (K.Div 'K.RoundHalfDown (P 1) (P 1) ~ P 1)
_test_Div_RoundHalfDown_P1_P1 =  Dict
_test_Rem_RoundHalfDown_P1_P1 :: Dict (K.Rem 'K.RoundHalfDown (P 1) (P 1) ~ Z)
_test_Rem_RoundHalfDown_P1_P1 =  Dict
_test_Div_RoundHalfDown_P1_P2 :: Dict (K.Div 'K.RoundHalfDown (P 1) (P 2) ~ Z)
_test_Div_RoundHalfDown_P1_P2 =  Dict
_test_Rem_RoundHalfDown_P1_P2 :: Dict (K.Rem 'K.RoundHalfDown (P 1) (P 2) ~ P 1)
_test_Rem_RoundHalfDown_P1_P2 =  Dict
_test_Div_RoundHalfDown_P1_P3 :: Dict (K.Div 'K.RoundHalfDown (P 1) (P 3) ~ Z)
_test_Div_RoundHalfDown_P1_P3 =  Dict
_test_Rem_RoundHalfDown_P1_P3 :: Dict (K.Rem 'K.RoundHalfDown (P 1) (P 3) ~ P 1)
_test_Rem_RoundHalfDown_P1_P3 =  Dict
_test_Div_RoundHalfDown_P1_P4 :: Dict (K.Div 'K.RoundHalfDown (P 1) (P 4) ~ Z)
_test_Div_RoundHalfDown_P1_P4 =  Dict
_test_Rem_RoundHalfDown_P1_P4 :: Dict (K.Rem 'K.RoundHalfDown (P 1) (P 4) ~ P 1)
_test_Rem_RoundHalfDown_P1_P4 =  Dict
_test_Div_RoundHalfDown_P2_N1 :: Dict (K.Div 'K.RoundHalfDown (P 2) (N 1) ~ N 2)
_test_Div_RoundHalfDown_P2_N1 =  Dict
_test_Rem_RoundHalfDown_P2_N1 :: Dict (K.Rem 'K.RoundHalfDown (P 2) (N 1) ~ Z)
_test_Rem_RoundHalfDown_P2_N1 =  Dict
_test_Div_RoundHalfDown_P2_N2 :: Dict (K.Div 'K.RoundHalfDown (P 2) (N 2) ~ N 1)
_test_Div_RoundHalfDown_P2_N2 =  Dict
_test_Rem_RoundHalfDown_P2_N2 :: Dict (K.Rem 'K.RoundHalfDown (P 2) (N 2) ~ Z)
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
_test_Rem_RoundHalfDown_P2_P1 :: Dict (K.Rem 'K.RoundHalfDown (P 2) (P 1) ~ Z)
_test_Rem_RoundHalfDown_P2_P1 =  Dict
_test_Div_RoundHalfDown_P2_P2 :: Dict (K.Div 'K.RoundHalfDown (P 2) (P 2) ~ P 1)
_test_Div_RoundHalfDown_P2_P2 =  Dict
_test_Rem_RoundHalfDown_P2_P2 :: Dict (K.Rem 'K.RoundHalfDown (P 2) (P 2) ~ Z)
_test_Rem_RoundHalfDown_P2_P2 =  Dict
_test_Div_RoundHalfDown_P2_P3 :: Dict (K.Div 'K.RoundHalfDown (P 2) (P 3) ~ P 1)
_test_Div_RoundHalfDown_P2_P3 =  Dict
_test_Rem_RoundHalfDown_P2_P3 :: Dict (K.Rem 'K.RoundHalfDown (P 2) (P 3) ~ N 1)
_test_Rem_RoundHalfDown_P2_P3 =  Dict
_test_Div_RoundHalfDown_P2_P4 :: Dict (K.Div 'K.RoundHalfDown (P 2) (P 4) ~ Z)
_test_Div_RoundHalfDown_P2_P4 =  Dict
_test_Rem_RoundHalfDown_P2_P4 :: Dict (K.Rem 'K.RoundHalfDown (P 2) (P 4) ~ P 2)
_test_Rem_RoundHalfDown_P2_P4 =  Dict
_test_Div_RoundHalfDown_P3_N1 :: Dict (K.Div 'K.RoundHalfDown (P 3) (N 1) ~ N 3)
_test_Div_RoundHalfDown_P3_N1 =  Dict
_test_Rem_RoundHalfDown_P3_N1 :: Dict (K.Rem 'K.RoundHalfDown (P 3) (N 1) ~ Z)
_test_Rem_RoundHalfDown_P3_N1 =  Dict
_test_Div_RoundHalfDown_P3_N2 :: Dict (K.Div 'K.RoundHalfDown (P 3) (N 2) ~ N 2)
_test_Div_RoundHalfDown_P3_N2 =  Dict
_test_Rem_RoundHalfDown_P3_N2 :: Dict (K.Rem 'K.RoundHalfDown (P 3) (N 2) ~ N 1)
_test_Rem_RoundHalfDown_P3_N2 =  Dict
_test_Div_RoundHalfDown_P3_N3 :: Dict (K.Div 'K.RoundHalfDown (P 3) (N 3) ~ N 1)
_test_Div_RoundHalfDown_P3_N3 =  Dict
_test_Rem_RoundHalfDown_P3_N3 :: Dict (K.Rem 'K.RoundHalfDown (P 3) (N 3) ~ Z)
_test_Rem_RoundHalfDown_P3_N3 =  Dict
_test_Div_RoundHalfDown_P3_N4 :: Dict (K.Div 'K.RoundHalfDown (P 3) (N 4) ~ N 1)
_test_Div_RoundHalfDown_P3_N4 =  Dict
_test_Rem_RoundHalfDown_P3_N4 :: Dict (K.Rem 'K.RoundHalfDown (P 3) (N 4) ~ N 1)
_test_Rem_RoundHalfDown_P3_N4 =  Dict
_test_Div_RoundHalfDown_P3_P1 :: Dict (K.Div 'K.RoundHalfDown (P 3) (P 1) ~ P 3)
_test_Div_RoundHalfDown_P3_P1 =  Dict
_test_Rem_RoundHalfDown_P3_P1 :: Dict (K.Rem 'K.RoundHalfDown (P 3) (P 1) ~ Z)
_test_Rem_RoundHalfDown_P3_P1 =  Dict
_test_Div_RoundHalfDown_P3_P2 :: Dict (K.Div 'K.RoundHalfDown (P 3) (P 2) ~ P 1)
_test_Div_RoundHalfDown_P3_P2 =  Dict
_test_Rem_RoundHalfDown_P3_P2 :: Dict (K.Rem 'K.RoundHalfDown (P 3) (P 2) ~ P 1)
_test_Rem_RoundHalfDown_P3_P2 =  Dict
_test_Div_RoundHalfDown_P3_P3 :: Dict (K.Div 'K.RoundHalfDown (P 3) (P 3) ~ P 1)
_test_Div_RoundHalfDown_P3_P3 =  Dict
_test_Rem_RoundHalfDown_P3_P3 :: Dict (K.Rem 'K.RoundHalfDown (P 3) (P 3) ~ Z)
_test_Rem_RoundHalfDown_P3_P3 =  Dict
_test_Div_RoundHalfDown_P3_P4 :: Dict (K.Div 'K.RoundHalfDown (P 3) (P 4) ~ P 1)
_test_Div_RoundHalfDown_P3_P4 =  Dict
_test_Rem_RoundHalfDown_P3_P4 :: Dict (K.Rem 'K.RoundHalfDown (P 3) (P 4) ~ N 1)
_test_Rem_RoundHalfDown_P3_P4 =  Dict
_test_Div_RoundHalfDown_P4_N1 :: Dict (K.Div 'K.RoundHalfDown (P 4) (N 1) ~ N 4)
_test_Div_RoundHalfDown_P4_N1 =  Dict
_test_Rem_RoundHalfDown_P4_N1 :: Dict (K.Rem 'K.RoundHalfDown (P 4) (N 1) ~ Z)
_test_Rem_RoundHalfDown_P4_N1 =  Dict
_test_Div_RoundHalfDown_P4_N2 :: Dict (K.Div 'K.RoundHalfDown (P 4) (N 2) ~ N 2)
_test_Div_RoundHalfDown_P4_N2 =  Dict
_test_Rem_RoundHalfDown_P4_N2 :: Dict (K.Rem 'K.RoundHalfDown (P 4) (N 2) ~ Z)
_test_Rem_RoundHalfDown_P4_N2 =  Dict
_test_Div_RoundHalfDown_P4_N3 :: Dict (K.Div 'K.RoundHalfDown (P 4) (N 3) ~ N 1)
_test_Div_RoundHalfDown_P4_N3 =  Dict
_test_Rem_RoundHalfDown_P4_N3 :: Dict (K.Rem 'K.RoundHalfDown (P 4) (N 3) ~ P 1)
_test_Rem_RoundHalfDown_P4_N3 =  Dict
_test_Div_RoundHalfDown_P4_N4 :: Dict (K.Div 'K.RoundHalfDown (P 4) (N 4) ~ N 1)
_test_Div_RoundHalfDown_P4_N4 =  Dict
_test_Rem_RoundHalfDown_P4_N4 :: Dict (K.Rem 'K.RoundHalfDown (P 4) (N 4) ~ Z)
_test_Rem_RoundHalfDown_P4_N4 =  Dict
_test_Div_RoundHalfDown_P4_P1 :: Dict (K.Div 'K.RoundHalfDown (P 4) (P 1) ~ P 4)
_test_Div_RoundHalfDown_P4_P1 =  Dict
_test_Rem_RoundHalfDown_P4_P1 :: Dict (K.Rem 'K.RoundHalfDown (P 4) (P 1) ~ Z)
_test_Rem_RoundHalfDown_P4_P1 =  Dict
_test_Div_RoundHalfDown_P4_P2 :: Dict (K.Div 'K.RoundHalfDown (P 4) (P 2) ~ P 2)
_test_Div_RoundHalfDown_P4_P2 =  Dict
_test_Rem_RoundHalfDown_P4_P2 :: Dict (K.Rem 'K.RoundHalfDown (P 4) (P 2) ~ Z)
_test_Rem_RoundHalfDown_P4_P2 =  Dict
_test_Div_RoundHalfDown_P4_P3 :: Dict (K.Div 'K.RoundHalfDown (P 4) (P 3) ~ P 1)
_test_Div_RoundHalfDown_P4_P3 =  Dict
_test_Rem_RoundHalfDown_P4_P3 :: Dict (K.Rem 'K.RoundHalfDown (P 4) (P 3) ~ P 1)
_test_Rem_RoundHalfDown_P4_P3 =  Dict
_test_Div_RoundHalfDown_P4_P4 :: Dict (K.Div 'K.RoundHalfDown (P 4) (P 4) ~ P 1)
_test_Div_RoundHalfDown_P4_P4 =  Dict
_test_Rem_RoundHalfDown_P4_P4 :: Dict (K.Rem 'K.RoundHalfDown (P 4) (P 4) ~ Z)
_test_Rem_RoundHalfDown_P4_P4 =  Dict
_test_Div_RoundHalfEven_N1_N1 :: Dict (K.Div 'K.RoundHalfEven (N 1) (N 1) ~ P 1)
_test_Div_RoundHalfEven_N1_N1 =  Dict
_test_Rem_RoundHalfEven_N1_N1 :: Dict (K.Rem 'K.RoundHalfEven (N 1) (N 1) ~ Z)
_test_Rem_RoundHalfEven_N1_N1 =  Dict
_test_Div_RoundHalfEven_N1_N2 :: Dict (K.Div 'K.RoundHalfEven (N 1) (N 2) ~ Z)
_test_Div_RoundHalfEven_N1_N2 =  Dict
_test_Rem_RoundHalfEven_N1_N2 :: Dict (K.Rem 'K.RoundHalfEven (N 1) (N 2) ~ N 1)
_test_Rem_RoundHalfEven_N1_N2 =  Dict
_test_Div_RoundHalfEven_N1_N3 :: Dict (K.Div 'K.RoundHalfEven (N 1) (N 3) ~ Z)
_test_Div_RoundHalfEven_N1_N3 =  Dict
_test_Rem_RoundHalfEven_N1_N3 :: Dict (K.Rem 'K.RoundHalfEven (N 1) (N 3) ~ N 1)
_test_Rem_RoundHalfEven_N1_N3 =  Dict
_test_Div_RoundHalfEven_N1_N4 :: Dict (K.Div 'K.RoundHalfEven (N 1) (N 4) ~ Z)
_test_Div_RoundHalfEven_N1_N4 =  Dict
_test_Rem_RoundHalfEven_N1_N4 :: Dict (K.Rem 'K.RoundHalfEven (N 1) (N 4) ~ N 1)
_test_Rem_RoundHalfEven_N1_N4 =  Dict
_test_Div_RoundHalfEven_N1_P1 :: Dict (K.Div 'K.RoundHalfEven (N 1) (P 1) ~ N 1)
_test_Div_RoundHalfEven_N1_P1 =  Dict
_test_Rem_RoundHalfEven_N1_P1 :: Dict (K.Rem 'K.RoundHalfEven (N 1) (P 1) ~ Z)
_test_Rem_RoundHalfEven_N1_P1 =  Dict
_test_Div_RoundHalfEven_N1_P2 :: Dict (K.Div 'K.RoundHalfEven (N 1) (P 2) ~ Z)
_test_Div_RoundHalfEven_N1_P2 =  Dict
_test_Rem_RoundHalfEven_N1_P2 :: Dict (K.Rem 'K.RoundHalfEven (N 1) (P 2) ~ N 1)
_test_Rem_RoundHalfEven_N1_P2 =  Dict
_test_Div_RoundHalfEven_N1_P3 :: Dict (K.Div 'K.RoundHalfEven (N 1) (P 3) ~ Z)
_test_Div_RoundHalfEven_N1_P3 =  Dict
_test_Rem_RoundHalfEven_N1_P3 :: Dict (K.Rem 'K.RoundHalfEven (N 1) (P 3) ~ N 1)
_test_Rem_RoundHalfEven_N1_P3 =  Dict
_test_Div_RoundHalfEven_N1_P4 :: Dict (K.Div 'K.RoundHalfEven (N 1) (P 4) ~ Z)
_test_Div_RoundHalfEven_N1_P4 =  Dict
_test_Rem_RoundHalfEven_N1_P4 :: Dict (K.Rem 'K.RoundHalfEven (N 1) (P 4) ~ N 1)
_test_Rem_RoundHalfEven_N1_P4 =  Dict
_test_Div_RoundHalfEven_N2_N1 :: Dict (K.Div 'K.RoundHalfEven (N 2) (N 1) ~ P 2)
_test_Div_RoundHalfEven_N2_N1 =  Dict
_test_Rem_RoundHalfEven_N2_N1 :: Dict (K.Rem 'K.RoundHalfEven (N 2) (N 1) ~ Z)
_test_Rem_RoundHalfEven_N2_N1 =  Dict
_test_Div_RoundHalfEven_N2_N2 :: Dict (K.Div 'K.RoundHalfEven (N 2) (N 2) ~ P 1)
_test_Div_RoundHalfEven_N2_N2 =  Dict
_test_Rem_RoundHalfEven_N2_N2 :: Dict (K.Rem 'K.RoundHalfEven (N 2) (N 2) ~ Z)
_test_Rem_RoundHalfEven_N2_N2 =  Dict
_test_Div_RoundHalfEven_N2_N3 :: Dict (K.Div 'K.RoundHalfEven (N 2) (N 3) ~ P 1)
_test_Div_RoundHalfEven_N2_N3 =  Dict
_test_Rem_RoundHalfEven_N2_N3 :: Dict (K.Rem 'K.RoundHalfEven (N 2) (N 3) ~ P 1)
_test_Rem_RoundHalfEven_N2_N3 =  Dict
_test_Div_RoundHalfEven_N2_N4 :: Dict (K.Div 'K.RoundHalfEven (N 2) (N 4) ~ Z)
_test_Div_RoundHalfEven_N2_N4 =  Dict
_test_Rem_RoundHalfEven_N2_N4 :: Dict (K.Rem 'K.RoundHalfEven (N 2) (N 4) ~ N 2)
_test_Rem_RoundHalfEven_N2_N4 =  Dict
_test_Div_RoundHalfEven_N2_P1 :: Dict (K.Div 'K.RoundHalfEven (N 2) (P 1) ~ N 2)
_test_Div_RoundHalfEven_N2_P1 =  Dict
_test_Rem_RoundHalfEven_N2_P1 :: Dict (K.Rem 'K.RoundHalfEven (N 2) (P 1) ~ Z)
_test_Rem_RoundHalfEven_N2_P1 =  Dict
_test_Div_RoundHalfEven_N2_P2 :: Dict (K.Div 'K.RoundHalfEven (N 2) (P 2) ~ N 1)
_test_Div_RoundHalfEven_N2_P2 =  Dict
_test_Rem_RoundHalfEven_N2_P2 :: Dict (K.Rem 'K.RoundHalfEven (N 2) (P 2) ~ Z)
_test_Rem_RoundHalfEven_N2_P2 =  Dict
_test_Div_RoundHalfEven_N2_P3 :: Dict (K.Div 'K.RoundHalfEven (N 2) (P 3) ~ N 1)
_test_Div_RoundHalfEven_N2_P3 =  Dict
_test_Rem_RoundHalfEven_N2_P3 :: Dict (K.Rem 'K.RoundHalfEven (N 2) (P 3) ~ P 1)
_test_Rem_RoundHalfEven_N2_P3 =  Dict
_test_Div_RoundHalfEven_N2_P4 :: Dict (K.Div 'K.RoundHalfEven (N 2) (P 4) ~ Z)
_test_Div_RoundHalfEven_N2_P4 =  Dict
_test_Rem_RoundHalfEven_N2_P4 :: Dict (K.Rem 'K.RoundHalfEven (N 2) (P 4) ~ N 2)
_test_Rem_RoundHalfEven_N2_P4 =  Dict
_test_Div_RoundHalfEven_N3_N1 :: Dict (K.Div 'K.RoundHalfEven (N 3) (N 1) ~ P 3)
_test_Div_RoundHalfEven_N3_N1 =  Dict
_test_Rem_RoundHalfEven_N3_N1 :: Dict (K.Rem 'K.RoundHalfEven (N 3) (N 1) ~ Z)
_test_Rem_RoundHalfEven_N3_N1 =  Dict
_test_Div_RoundHalfEven_N3_N2 :: Dict (K.Div 'K.RoundHalfEven (N 3) (N 2) ~ P 2)
_test_Div_RoundHalfEven_N3_N2 =  Dict
_test_Rem_RoundHalfEven_N3_N2 :: Dict (K.Rem 'K.RoundHalfEven (N 3) (N 2) ~ P 1)
_test_Rem_RoundHalfEven_N3_N2 =  Dict
_test_Div_RoundHalfEven_N3_N3 :: Dict (K.Div 'K.RoundHalfEven (N 3) (N 3) ~ P 1)
_test_Div_RoundHalfEven_N3_N3 =  Dict
_test_Rem_RoundHalfEven_N3_N3 :: Dict (K.Rem 'K.RoundHalfEven (N 3) (N 3) ~ Z)
_test_Rem_RoundHalfEven_N3_N3 =  Dict
_test_Div_RoundHalfEven_N3_N4 :: Dict (K.Div 'K.RoundHalfEven (N 3) (N 4) ~ P 1)
_test_Div_RoundHalfEven_N3_N4 =  Dict
_test_Rem_RoundHalfEven_N3_N4 :: Dict (K.Rem 'K.RoundHalfEven (N 3) (N 4) ~ P 1)
_test_Rem_RoundHalfEven_N3_N4 =  Dict
_test_Div_RoundHalfEven_N3_P1 :: Dict (K.Div 'K.RoundHalfEven (N 3) (P 1) ~ N 3)
_test_Div_RoundHalfEven_N3_P1 =  Dict
_test_Rem_RoundHalfEven_N3_P1 :: Dict (K.Rem 'K.RoundHalfEven (N 3) (P 1) ~ Z)
_test_Rem_RoundHalfEven_N3_P1 =  Dict
_test_Div_RoundHalfEven_N3_P2 :: Dict (K.Div 'K.RoundHalfEven (N 3) (P 2) ~ N 2)
_test_Div_RoundHalfEven_N3_P2 =  Dict
_test_Rem_RoundHalfEven_N3_P2 :: Dict (K.Rem 'K.RoundHalfEven (N 3) (P 2) ~ P 1)
_test_Rem_RoundHalfEven_N3_P2 =  Dict
_test_Div_RoundHalfEven_N3_P3 :: Dict (K.Div 'K.RoundHalfEven (N 3) (P 3) ~ N 1)
_test_Div_RoundHalfEven_N3_P3 =  Dict
_test_Rem_RoundHalfEven_N3_P3 :: Dict (K.Rem 'K.RoundHalfEven (N 3) (P 3) ~ Z)
_test_Rem_RoundHalfEven_N3_P3 =  Dict
_test_Div_RoundHalfEven_N3_P4 :: Dict (K.Div 'K.RoundHalfEven (N 3) (P 4) ~ N 1)
_test_Div_RoundHalfEven_N3_P4 =  Dict
_test_Rem_RoundHalfEven_N3_P4 :: Dict (K.Rem 'K.RoundHalfEven (N 3) (P 4) ~ P 1)
_test_Rem_RoundHalfEven_N3_P4 =  Dict
_test_Div_RoundHalfEven_N4_N1 :: Dict (K.Div 'K.RoundHalfEven (N 4) (N 1) ~ P 4)
_test_Div_RoundHalfEven_N4_N1 =  Dict
_test_Rem_RoundHalfEven_N4_N1 :: Dict (K.Rem 'K.RoundHalfEven (N 4) (N 1) ~ Z)
_test_Rem_RoundHalfEven_N4_N1 =  Dict
_test_Div_RoundHalfEven_N4_N2 :: Dict (K.Div 'K.RoundHalfEven (N 4) (N 2) ~ P 2)
_test_Div_RoundHalfEven_N4_N2 =  Dict
_test_Rem_RoundHalfEven_N4_N2 :: Dict (K.Rem 'K.RoundHalfEven (N 4) (N 2) ~ Z)
_test_Rem_RoundHalfEven_N4_N2 =  Dict
_test_Div_RoundHalfEven_N4_N3 :: Dict (K.Div 'K.RoundHalfEven (N 4) (N 3) ~ P 1)
_test_Div_RoundHalfEven_N4_N3 =  Dict
_test_Rem_RoundHalfEven_N4_N3 :: Dict (K.Rem 'K.RoundHalfEven (N 4) (N 3) ~ N 1)
_test_Rem_RoundHalfEven_N4_N3 =  Dict
_test_Div_RoundHalfEven_N4_N4 :: Dict (K.Div 'K.RoundHalfEven (N 4) (N 4) ~ P 1)
_test_Div_RoundHalfEven_N4_N4 =  Dict
_test_Rem_RoundHalfEven_N4_N4 :: Dict (K.Rem 'K.RoundHalfEven (N 4) (N 4) ~ Z)
_test_Rem_RoundHalfEven_N4_N4 =  Dict
_test_Div_RoundHalfEven_N4_P1 :: Dict (K.Div 'K.RoundHalfEven (N 4) (P 1) ~ N 4)
_test_Div_RoundHalfEven_N4_P1 =  Dict
_test_Rem_RoundHalfEven_N4_P1 :: Dict (K.Rem 'K.RoundHalfEven (N 4) (P 1) ~ Z)
_test_Rem_RoundHalfEven_N4_P1 =  Dict
_test_Div_RoundHalfEven_N4_P2 :: Dict (K.Div 'K.RoundHalfEven (N 4) (P 2) ~ N 2)
_test_Div_RoundHalfEven_N4_P2 =  Dict
_test_Rem_RoundHalfEven_N4_P2 :: Dict (K.Rem 'K.RoundHalfEven (N 4) (P 2) ~ Z)
_test_Rem_RoundHalfEven_N4_P2 =  Dict
_test_Div_RoundHalfEven_N4_P3 :: Dict (K.Div 'K.RoundHalfEven (N 4) (P 3) ~ N 1)
_test_Div_RoundHalfEven_N4_P3 =  Dict
_test_Rem_RoundHalfEven_N4_P3 :: Dict (K.Rem 'K.RoundHalfEven (N 4) (P 3) ~ N 1)
_test_Rem_RoundHalfEven_N4_P3 =  Dict
_test_Div_RoundHalfEven_N4_P4 :: Dict (K.Div 'K.RoundHalfEven (N 4) (P 4) ~ N 1)
_test_Div_RoundHalfEven_N4_P4 =  Dict
_test_Rem_RoundHalfEven_N4_P4 :: Dict (K.Rem 'K.RoundHalfEven (N 4) (P 4) ~ Z)
_test_Rem_RoundHalfEven_N4_P4 =  Dict
_test_Rem_RoundHalfEven_Z_N1 :: Dict (K.Rem 'K.RoundHalfEven Z (N 1) ~ Z)
_test_Rem_RoundHalfEven_Z_N1 =  Dict
_test_Rem_RoundHalfEven_Z_N2 :: Dict (K.Rem 'K.RoundHalfEven Z (N 2) ~ Z)
_test_Rem_RoundHalfEven_Z_N2 =  Dict
_test_Rem_RoundHalfEven_Z_N3 :: Dict (K.Rem 'K.RoundHalfEven Z (N 3) ~ Z)
_test_Rem_RoundHalfEven_Z_N3 =  Dict
_test_Rem_RoundHalfEven_Z_N4 :: Dict (K.Rem 'K.RoundHalfEven Z (N 4) ~ Z)
_test_Rem_RoundHalfEven_Z_N4 =  Dict
_test_Rem_RoundHalfEven_Z_P1 :: Dict (K.Rem 'K.RoundHalfEven Z (P 1) ~ Z)
_test_Rem_RoundHalfEven_Z_P1 =  Dict
_test_Rem_RoundHalfEven_Z_P2 :: Dict (K.Rem 'K.RoundHalfEven Z (P 2) ~ Z)
_test_Rem_RoundHalfEven_Z_P2 =  Dict
_test_Rem_RoundHalfEven_Z_P3 :: Dict (K.Rem 'K.RoundHalfEven Z (P 3) ~ Z)
_test_Rem_RoundHalfEven_Z_P3 =  Dict
_test_Rem_RoundHalfEven_Z_P4 :: Dict (K.Rem 'K.RoundHalfEven Z (P 4) ~ Z)
_test_Rem_RoundHalfEven_Z_P4 =  Dict
_test_Div_RoundHalfEven_P1_N1 :: Dict (K.Div 'K.RoundHalfEven (P 1) (N 1) ~ N 1)
_test_Div_RoundHalfEven_P1_N1 =  Dict
_test_Rem_RoundHalfEven_P1_N1 :: Dict (K.Rem 'K.RoundHalfEven (P 1) (N 1) ~ Z)
_test_Rem_RoundHalfEven_P1_N1 =  Dict
_test_Div_RoundHalfEven_P1_N2 :: Dict (K.Div 'K.RoundHalfEven (P 1) (N 2) ~ Z)
_test_Div_RoundHalfEven_P1_N2 =  Dict
_test_Rem_RoundHalfEven_P1_N2 :: Dict (K.Rem 'K.RoundHalfEven (P 1) (N 2) ~ P 1)
_test_Rem_RoundHalfEven_P1_N2 =  Dict
_test_Div_RoundHalfEven_P1_N3 :: Dict (K.Div 'K.RoundHalfEven (P 1) (N 3) ~ Z)
_test_Div_RoundHalfEven_P1_N3 =  Dict
_test_Rem_RoundHalfEven_P1_N3 :: Dict (K.Rem 'K.RoundHalfEven (P 1) (N 3) ~ P 1)
_test_Rem_RoundHalfEven_P1_N3 =  Dict
_test_Div_RoundHalfEven_P1_N4 :: Dict (K.Div 'K.RoundHalfEven (P 1) (N 4) ~ Z)
_test_Div_RoundHalfEven_P1_N4 =  Dict
_test_Rem_RoundHalfEven_P1_N4 :: Dict (K.Rem 'K.RoundHalfEven (P 1) (N 4) ~ P 1)
_test_Rem_RoundHalfEven_P1_N4 =  Dict
_test_Div_RoundHalfEven_P1_P1 :: Dict (K.Div 'K.RoundHalfEven (P 1) (P 1) ~ P 1)
_test_Div_RoundHalfEven_P1_P1 =  Dict
_test_Rem_RoundHalfEven_P1_P1 :: Dict (K.Rem 'K.RoundHalfEven (P 1) (P 1) ~ Z)
_test_Rem_RoundHalfEven_P1_P1 =  Dict
_test_Div_RoundHalfEven_P1_P2 :: Dict (K.Div 'K.RoundHalfEven (P 1) (P 2) ~ Z)
_test_Div_RoundHalfEven_P1_P2 =  Dict
_test_Rem_RoundHalfEven_P1_P2 :: Dict (K.Rem 'K.RoundHalfEven (P 1) (P 2) ~ P 1)
_test_Rem_RoundHalfEven_P1_P2 =  Dict
_test_Div_RoundHalfEven_P1_P3 :: Dict (K.Div 'K.RoundHalfEven (P 1) (P 3) ~ Z)
_test_Div_RoundHalfEven_P1_P3 =  Dict
_test_Rem_RoundHalfEven_P1_P3 :: Dict (K.Rem 'K.RoundHalfEven (P 1) (P 3) ~ P 1)
_test_Rem_RoundHalfEven_P1_P3 =  Dict
_test_Div_RoundHalfEven_P1_P4 :: Dict (K.Div 'K.RoundHalfEven (P 1) (P 4) ~ Z)
_test_Div_RoundHalfEven_P1_P4 =  Dict
_test_Rem_RoundHalfEven_P1_P4 :: Dict (K.Rem 'K.RoundHalfEven (P 1) (P 4) ~ P 1)
_test_Rem_RoundHalfEven_P1_P4 =  Dict
_test_Div_RoundHalfEven_P2_N1 :: Dict (K.Div 'K.RoundHalfEven (P 2) (N 1) ~ N 2)
_test_Div_RoundHalfEven_P2_N1 =  Dict
_test_Rem_RoundHalfEven_P2_N1 :: Dict (K.Rem 'K.RoundHalfEven (P 2) (N 1) ~ Z)
_test_Rem_RoundHalfEven_P2_N1 =  Dict
_test_Div_RoundHalfEven_P2_N2 :: Dict (K.Div 'K.RoundHalfEven (P 2) (N 2) ~ N 1)
_test_Div_RoundHalfEven_P2_N2 =  Dict
_test_Rem_RoundHalfEven_P2_N2 :: Dict (K.Rem 'K.RoundHalfEven (P 2) (N 2) ~ Z)
_test_Rem_RoundHalfEven_P2_N2 =  Dict
_test_Div_RoundHalfEven_P2_N3 :: Dict (K.Div 'K.RoundHalfEven (P 2) (N 3) ~ N 1)
_test_Div_RoundHalfEven_P2_N3 =  Dict
_test_Rem_RoundHalfEven_P2_N3 :: Dict (K.Rem 'K.RoundHalfEven (P 2) (N 3) ~ N 1)
_test_Rem_RoundHalfEven_P2_N3 =  Dict
_test_Div_RoundHalfEven_P2_N4 :: Dict (K.Div 'K.RoundHalfEven (P 2) (N 4) ~ Z)
_test_Div_RoundHalfEven_P2_N4 =  Dict
_test_Rem_RoundHalfEven_P2_N4 :: Dict (K.Rem 'K.RoundHalfEven (P 2) (N 4) ~ P 2)
_test_Rem_RoundHalfEven_P2_N4 =  Dict
_test_Div_RoundHalfEven_P2_P1 :: Dict (K.Div 'K.RoundHalfEven (P 2) (P 1) ~ P 2)
_test_Div_RoundHalfEven_P2_P1 =  Dict
_test_Rem_RoundHalfEven_P2_P1 :: Dict (K.Rem 'K.RoundHalfEven (P 2) (P 1) ~ Z)
_test_Rem_RoundHalfEven_P2_P1 =  Dict
_test_Div_RoundHalfEven_P2_P2 :: Dict (K.Div 'K.RoundHalfEven (P 2) (P 2) ~ P 1)
_test_Div_RoundHalfEven_P2_P2 =  Dict
_test_Rem_RoundHalfEven_P2_P2 :: Dict (K.Rem 'K.RoundHalfEven (P 2) (P 2) ~ Z)
_test_Rem_RoundHalfEven_P2_P2 =  Dict
_test_Div_RoundHalfEven_P2_P3 :: Dict (K.Div 'K.RoundHalfEven (P 2) (P 3) ~ P 1)
_test_Div_RoundHalfEven_P2_P3 =  Dict
_test_Rem_RoundHalfEven_P2_P3 :: Dict (K.Rem 'K.RoundHalfEven (P 2) (P 3) ~ N 1)
_test_Rem_RoundHalfEven_P2_P3 =  Dict
_test_Div_RoundHalfEven_P2_P4 :: Dict (K.Div 'K.RoundHalfEven (P 2) (P 4) ~ Z)
_test_Div_RoundHalfEven_P2_P4 =  Dict
_test_Rem_RoundHalfEven_P2_P4 :: Dict (K.Rem 'K.RoundHalfEven (P 2) (P 4) ~ P 2)
_test_Rem_RoundHalfEven_P2_P4 =  Dict
_test_Div_RoundHalfEven_P3_N1 :: Dict (K.Div 'K.RoundHalfEven (P 3) (N 1) ~ N 3)
_test_Div_RoundHalfEven_P3_N1 =  Dict
_test_Rem_RoundHalfEven_P3_N1 :: Dict (K.Rem 'K.RoundHalfEven (P 3) (N 1) ~ Z)
_test_Rem_RoundHalfEven_P3_N1 =  Dict
_test_Div_RoundHalfEven_P3_N2 :: Dict (K.Div 'K.RoundHalfEven (P 3) (N 2) ~ N 2)
_test_Div_RoundHalfEven_P3_N2 =  Dict
_test_Rem_RoundHalfEven_P3_N2 :: Dict (K.Rem 'K.RoundHalfEven (P 3) (N 2) ~ N 1)
_test_Rem_RoundHalfEven_P3_N2 =  Dict
_test_Div_RoundHalfEven_P3_N3 :: Dict (K.Div 'K.RoundHalfEven (P 3) (N 3) ~ N 1)
_test_Div_RoundHalfEven_P3_N3 =  Dict
_test_Rem_RoundHalfEven_P3_N3 :: Dict (K.Rem 'K.RoundHalfEven (P 3) (N 3) ~ Z)
_test_Rem_RoundHalfEven_P3_N3 =  Dict
_test_Div_RoundHalfEven_P3_N4 :: Dict (K.Div 'K.RoundHalfEven (P 3) (N 4) ~ N 1)
_test_Div_RoundHalfEven_P3_N4 =  Dict
_test_Rem_RoundHalfEven_P3_N4 :: Dict (K.Rem 'K.RoundHalfEven (P 3) (N 4) ~ N 1)
_test_Rem_RoundHalfEven_P3_N4 =  Dict
_test_Div_RoundHalfEven_P3_P1 :: Dict (K.Div 'K.RoundHalfEven (P 3) (P 1) ~ P 3)
_test_Div_RoundHalfEven_P3_P1 =  Dict
_test_Rem_RoundHalfEven_P3_P1 :: Dict (K.Rem 'K.RoundHalfEven (P 3) (P 1) ~ Z)
_test_Rem_RoundHalfEven_P3_P1 =  Dict
_test_Div_RoundHalfEven_P3_P2 :: Dict (K.Div 'K.RoundHalfEven (P 3) (P 2) ~ P 2)
_test_Div_RoundHalfEven_P3_P2 =  Dict
_test_Rem_RoundHalfEven_P3_P2 :: Dict (K.Rem 'K.RoundHalfEven (P 3) (P 2) ~ N 1)
_test_Rem_RoundHalfEven_P3_P2 =  Dict
_test_Div_RoundHalfEven_P3_P3 :: Dict (K.Div 'K.RoundHalfEven (P 3) (P 3) ~ P 1)
_test_Div_RoundHalfEven_P3_P3 =  Dict
_test_Rem_RoundHalfEven_P3_P3 :: Dict (K.Rem 'K.RoundHalfEven (P 3) (P 3) ~ Z)
_test_Rem_RoundHalfEven_P3_P3 =  Dict
_test_Div_RoundHalfEven_P3_P4 :: Dict (K.Div 'K.RoundHalfEven (P 3) (P 4) ~ P 1)
_test_Div_RoundHalfEven_P3_P4 =  Dict
_test_Rem_RoundHalfEven_P3_P4 :: Dict (K.Rem 'K.RoundHalfEven (P 3) (P 4) ~ N 1)
_test_Rem_RoundHalfEven_P3_P4 =  Dict
_test_Div_RoundHalfEven_P4_N1 :: Dict (K.Div 'K.RoundHalfEven (P 4) (N 1) ~ N 4)
_test_Div_RoundHalfEven_P4_N1 =  Dict
_test_Rem_RoundHalfEven_P4_N1 :: Dict (K.Rem 'K.RoundHalfEven (P 4) (N 1) ~ Z)
_test_Rem_RoundHalfEven_P4_N1 =  Dict
_test_Div_RoundHalfEven_P4_N2 :: Dict (K.Div 'K.RoundHalfEven (P 4) (N 2) ~ N 2)
_test_Div_RoundHalfEven_P4_N2 =  Dict
_test_Rem_RoundHalfEven_P4_N2 :: Dict (K.Rem 'K.RoundHalfEven (P 4) (N 2) ~ Z)
_test_Rem_RoundHalfEven_P4_N2 =  Dict
_test_Div_RoundHalfEven_P4_N3 :: Dict (K.Div 'K.RoundHalfEven (P 4) (N 3) ~ N 1)
_test_Div_RoundHalfEven_P4_N3 =  Dict
_test_Rem_RoundHalfEven_P4_N3 :: Dict (K.Rem 'K.RoundHalfEven (P 4) (N 3) ~ P 1)
_test_Rem_RoundHalfEven_P4_N3 =  Dict
_test_Div_RoundHalfEven_P4_N4 :: Dict (K.Div 'K.RoundHalfEven (P 4) (N 4) ~ N 1)
_test_Div_RoundHalfEven_P4_N4 =  Dict
_test_Rem_RoundHalfEven_P4_N4 :: Dict (K.Rem 'K.RoundHalfEven (P 4) (N 4) ~ Z)
_test_Rem_RoundHalfEven_P4_N4 =  Dict
_test_Div_RoundHalfEven_P4_P1 :: Dict (K.Div 'K.RoundHalfEven (P 4) (P 1) ~ P 4)
_test_Div_RoundHalfEven_P4_P1 =  Dict
_test_Rem_RoundHalfEven_P4_P1 :: Dict (K.Rem 'K.RoundHalfEven (P 4) (P 1) ~ Z)
_test_Rem_RoundHalfEven_P4_P1 =  Dict
_test_Div_RoundHalfEven_P4_P2 :: Dict (K.Div 'K.RoundHalfEven (P 4) (P 2) ~ P 2)
_test_Div_RoundHalfEven_P4_P2 =  Dict
_test_Rem_RoundHalfEven_P4_P2 :: Dict (K.Rem 'K.RoundHalfEven (P 4) (P 2) ~ Z)
_test_Rem_RoundHalfEven_P4_P2 =  Dict
_test_Div_RoundHalfEven_P4_P3 :: Dict (K.Div 'K.RoundHalfEven (P 4) (P 3) ~ P 1)
_test_Div_RoundHalfEven_P4_P3 =  Dict
_test_Rem_RoundHalfEven_P4_P3 :: Dict (K.Rem 'K.RoundHalfEven (P 4) (P 3) ~ P 1)
_test_Rem_RoundHalfEven_P4_P3 =  Dict
_test_Div_RoundHalfEven_P4_P4 :: Dict (K.Div 'K.RoundHalfEven (P 4) (P 4) ~ P 1)
_test_Div_RoundHalfEven_P4_P4 =  Dict
_test_Rem_RoundHalfEven_P4_P4 :: Dict (K.Rem 'K.RoundHalfEven (P 4) (P 4) ~ Z)
_test_Rem_RoundHalfEven_P4_P4 =  Dict
_test_Div_RoundHalfOdd_N1_N1 :: Dict (K.Div 'K.RoundHalfOdd (N 1) (N 1) ~ P 1)
_test_Div_RoundHalfOdd_N1_N1 =  Dict
_test_Rem_RoundHalfOdd_N1_N1 :: Dict (K.Rem 'K.RoundHalfOdd (N 1) (N 1) ~ Z)
_test_Rem_RoundHalfOdd_N1_N1 =  Dict
_test_Div_RoundHalfOdd_N1_N2 :: Dict (K.Div 'K.RoundHalfOdd (N 1) (N 2) ~ P 1)
_test_Div_RoundHalfOdd_N1_N2 =  Dict
_test_Rem_RoundHalfOdd_N1_N2 :: Dict (K.Rem 'K.RoundHalfOdd (N 1) (N 2) ~ P 1)
_test_Rem_RoundHalfOdd_N1_N2 =  Dict
_test_Div_RoundHalfOdd_N1_N3 :: Dict (K.Div 'K.RoundHalfOdd (N 1) (N 3) ~ Z)
_test_Div_RoundHalfOdd_N1_N3 =  Dict
_test_Rem_RoundHalfOdd_N1_N3 :: Dict (K.Rem 'K.RoundHalfOdd (N 1) (N 3) ~ N 1)
_test_Rem_RoundHalfOdd_N1_N3 =  Dict
_test_Div_RoundHalfOdd_N1_N4 :: Dict (K.Div 'K.RoundHalfOdd (N 1) (N 4) ~ Z)
_test_Div_RoundHalfOdd_N1_N4 =  Dict
_test_Rem_RoundHalfOdd_N1_N4 :: Dict (K.Rem 'K.RoundHalfOdd (N 1) (N 4) ~ N 1)
_test_Rem_RoundHalfOdd_N1_N4 =  Dict
_test_Div_RoundHalfOdd_N1_P1 :: Dict (K.Div 'K.RoundHalfOdd (N 1) (P 1) ~ N 1)
_test_Div_RoundHalfOdd_N1_P1 =  Dict
_test_Rem_RoundHalfOdd_N1_P1 :: Dict (K.Rem 'K.RoundHalfOdd (N 1) (P 1) ~ Z)
_test_Rem_RoundHalfOdd_N1_P1 =  Dict
_test_Div_RoundHalfOdd_N1_P2 :: Dict (K.Div 'K.RoundHalfOdd (N 1) (P 2) ~ N 1)
_test_Div_RoundHalfOdd_N1_P2 =  Dict
_test_Rem_RoundHalfOdd_N1_P2 :: Dict (K.Rem 'K.RoundHalfOdd (N 1) (P 2) ~ P 1)
_test_Rem_RoundHalfOdd_N1_P2 =  Dict
_test_Div_RoundHalfOdd_N1_P3 :: Dict (K.Div 'K.RoundHalfOdd (N 1) (P 3) ~ Z)
_test_Div_RoundHalfOdd_N1_P3 =  Dict
_test_Rem_RoundHalfOdd_N1_P3 :: Dict (K.Rem 'K.RoundHalfOdd (N 1) (P 3) ~ N 1)
_test_Rem_RoundHalfOdd_N1_P3 =  Dict
_test_Div_RoundHalfOdd_N1_P4 :: Dict (K.Div 'K.RoundHalfOdd (N 1) (P 4) ~ Z)
_test_Div_RoundHalfOdd_N1_P4 =  Dict
_test_Rem_RoundHalfOdd_N1_P4 :: Dict (K.Rem 'K.RoundHalfOdd (N 1) (P 4) ~ N 1)
_test_Rem_RoundHalfOdd_N1_P4 =  Dict
_test_Div_RoundHalfOdd_N2_N1 :: Dict (K.Div 'K.RoundHalfOdd (N 2) (N 1) ~ P 2)
_test_Div_RoundHalfOdd_N2_N1 =  Dict
_test_Rem_RoundHalfOdd_N2_N1 :: Dict (K.Rem 'K.RoundHalfOdd (N 2) (N 1) ~ Z)
_test_Rem_RoundHalfOdd_N2_N1 =  Dict
_test_Div_RoundHalfOdd_N2_N2 :: Dict (K.Div 'K.RoundHalfOdd (N 2) (N 2) ~ P 1)
_test_Div_RoundHalfOdd_N2_N2 =  Dict
_test_Rem_RoundHalfOdd_N2_N2 :: Dict (K.Rem 'K.RoundHalfOdd (N 2) (N 2) ~ Z)
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
_test_Rem_RoundHalfOdd_N2_P1 :: Dict (K.Rem 'K.RoundHalfOdd (N 2) (P 1) ~ Z)
_test_Rem_RoundHalfOdd_N2_P1 =  Dict
_test_Div_RoundHalfOdd_N2_P2 :: Dict (K.Div 'K.RoundHalfOdd (N 2) (P 2) ~ N 1)
_test_Div_RoundHalfOdd_N2_P2 =  Dict
_test_Rem_RoundHalfOdd_N2_P2 :: Dict (K.Rem 'K.RoundHalfOdd (N 2) (P 2) ~ Z)
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
_test_Rem_RoundHalfOdd_N3_N1 :: Dict (K.Rem 'K.RoundHalfOdd (N 3) (N 1) ~ Z)
_test_Rem_RoundHalfOdd_N3_N1 =  Dict
_test_Div_RoundHalfOdd_N3_N2 :: Dict (K.Div 'K.RoundHalfOdd (N 3) (N 2) ~ P 1)
_test_Div_RoundHalfOdd_N3_N2 =  Dict
_test_Rem_RoundHalfOdd_N3_N2 :: Dict (K.Rem 'K.RoundHalfOdd (N 3) (N 2) ~ N 1)
_test_Rem_RoundHalfOdd_N3_N2 =  Dict
_test_Div_RoundHalfOdd_N3_N3 :: Dict (K.Div 'K.RoundHalfOdd (N 3) (N 3) ~ P 1)
_test_Div_RoundHalfOdd_N3_N3 =  Dict
_test_Rem_RoundHalfOdd_N3_N3 :: Dict (K.Rem 'K.RoundHalfOdd (N 3) (N 3) ~ Z)
_test_Rem_RoundHalfOdd_N3_N3 =  Dict
_test_Div_RoundHalfOdd_N3_N4 :: Dict (K.Div 'K.RoundHalfOdd (N 3) (N 4) ~ P 1)
_test_Div_RoundHalfOdd_N3_N4 =  Dict
_test_Rem_RoundHalfOdd_N3_N4 :: Dict (K.Rem 'K.RoundHalfOdd (N 3) (N 4) ~ P 1)
_test_Rem_RoundHalfOdd_N3_N4 =  Dict
_test_Div_RoundHalfOdd_N3_P1 :: Dict (K.Div 'K.RoundHalfOdd (N 3) (P 1) ~ N 3)
_test_Div_RoundHalfOdd_N3_P1 =  Dict
_test_Rem_RoundHalfOdd_N3_P1 :: Dict (K.Rem 'K.RoundHalfOdd (N 3) (P 1) ~ Z)
_test_Rem_RoundHalfOdd_N3_P1 =  Dict
_test_Div_RoundHalfOdd_N3_P2 :: Dict (K.Div 'K.RoundHalfOdd (N 3) (P 2) ~ N 1)
_test_Div_RoundHalfOdd_N3_P2 =  Dict
_test_Rem_RoundHalfOdd_N3_P2 :: Dict (K.Rem 'K.RoundHalfOdd (N 3) (P 2) ~ N 1)
_test_Rem_RoundHalfOdd_N3_P2 =  Dict
_test_Div_RoundHalfOdd_N3_P3 :: Dict (K.Div 'K.RoundHalfOdd (N 3) (P 3) ~ N 1)
_test_Div_RoundHalfOdd_N3_P3 =  Dict
_test_Rem_RoundHalfOdd_N3_P3 :: Dict (K.Rem 'K.RoundHalfOdd (N 3) (P 3) ~ Z)
_test_Rem_RoundHalfOdd_N3_P3 =  Dict
_test_Div_RoundHalfOdd_N3_P4 :: Dict (K.Div 'K.RoundHalfOdd (N 3) (P 4) ~ N 1)
_test_Div_RoundHalfOdd_N3_P4 =  Dict
_test_Rem_RoundHalfOdd_N3_P4 :: Dict (K.Rem 'K.RoundHalfOdd (N 3) (P 4) ~ P 1)
_test_Rem_RoundHalfOdd_N3_P4 =  Dict
_test_Div_RoundHalfOdd_N4_N1 :: Dict (K.Div 'K.RoundHalfOdd (N 4) (N 1) ~ P 4)
_test_Div_RoundHalfOdd_N4_N1 =  Dict
_test_Rem_RoundHalfOdd_N4_N1 :: Dict (K.Rem 'K.RoundHalfOdd (N 4) (N 1) ~ Z)
_test_Rem_RoundHalfOdd_N4_N1 =  Dict
_test_Div_RoundHalfOdd_N4_N2 :: Dict (K.Div 'K.RoundHalfOdd (N 4) (N 2) ~ P 2)
_test_Div_RoundHalfOdd_N4_N2 =  Dict
_test_Rem_RoundHalfOdd_N4_N2 :: Dict (K.Rem 'K.RoundHalfOdd (N 4) (N 2) ~ Z)
_test_Rem_RoundHalfOdd_N4_N2 =  Dict
_test_Div_RoundHalfOdd_N4_N3 :: Dict (K.Div 'K.RoundHalfOdd (N 4) (N 3) ~ P 1)
_test_Div_RoundHalfOdd_N4_N3 =  Dict
_test_Rem_RoundHalfOdd_N4_N3 :: Dict (K.Rem 'K.RoundHalfOdd (N 4) (N 3) ~ N 1)
_test_Rem_RoundHalfOdd_N4_N3 =  Dict
_test_Div_RoundHalfOdd_N4_N4 :: Dict (K.Div 'K.RoundHalfOdd (N 4) (N 4) ~ P 1)
_test_Div_RoundHalfOdd_N4_N4 =  Dict
_test_Rem_RoundHalfOdd_N4_N4 :: Dict (K.Rem 'K.RoundHalfOdd (N 4) (N 4) ~ Z)
_test_Rem_RoundHalfOdd_N4_N4 =  Dict
_test_Div_RoundHalfOdd_N4_P1 :: Dict (K.Div 'K.RoundHalfOdd (N 4) (P 1) ~ N 4)
_test_Div_RoundHalfOdd_N4_P1 =  Dict
_test_Rem_RoundHalfOdd_N4_P1 :: Dict (K.Rem 'K.RoundHalfOdd (N 4) (P 1) ~ Z)
_test_Rem_RoundHalfOdd_N4_P1 =  Dict
_test_Div_RoundHalfOdd_N4_P2 :: Dict (K.Div 'K.RoundHalfOdd (N 4) (P 2) ~ N 2)
_test_Div_RoundHalfOdd_N4_P2 =  Dict
_test_Rem_RoundHalfOdd_N4_P2 :: Dict (K.Rem 'K.RoundHalfOdd (N 4) (P 2) ~ Z)
_test_Rem_RoundHalfOdd_N4_P2 =  Dict
_test_Div_RoundHalfOdd_N4_P3 :: Dict (K.Div 'K.RoundHalfOdd (N 4) (P 3) ~ N 1)
_test_Div_RoundHalfOdd_N4_P3 =  Dict
_test_Rem_RoundHalfOdd_N4_P3 :: Dict (K.Rem 'K.RoundHalfOdd (N 4) (P 3) ~ N 1)
_test_Rem_RoundHalfOdd_N4_P3 =  Dict
_test_Div_RoundHalfOdd_N4_P4 :: Dict (K.Div 'K.RoundHalfOdd (N 4) (P 4) ~ N 1)
_test_Div_RoundHalfOdd_N4_P4 =  Dict
_test_Rem_RoundHalfOdd_N4_P4 :: Dict (K.Rem 'K.RoundHalfOdd (N 4) (P 4) ~ Z)
_test_Rem_RoundHalfOdd_N4_P4 =  Dict
_test_Rem_RoundHalfOdd_Z_N1 :: Dict (K.Rem 'K.RoundHalfOdd Z (N 1) ~ Z)
_test_Rem_RoundHalfOdd_Z_N1 =  Dict
_test_Rem_RoundHalfOdd_Z_N2 :: Dict (K.Rem 'K.RoundHalfOdd Z (N 2) ~ Z)
_test_Rem_RoundHalfOdd_Z_N2 =  Dict
_test_Rem_RoundHalfOdd_Z_N3 :: Dict (K.Rem 'K.RoundHalfOdd Z (N 3) ~ Z)
_test_Rem_RoundHalfOdd_Z_N3 =  Dict
_test_Rem_RoundHalfOdd_Z_N4 :: Dict (K.Rem 'K.RoundHalfOdd Z (N 4) ~ Z)
_test_Rem_RoundHalfOdd_Z_N4 =  Dict
_test_Rem_RoundHalfOdd_Z_P1 :: Dict (K.Rem 'K.RoundHalfOdd Z (P 1) ~ Z)
_test_Rem_RoundHalfOdd_Z_P1 =  Dict
_test_Rem_RoundHalfOdd_Z_P2 :: Dict (K.Rem 'K.RoundHalfOdd Z (P 2) ~ Z)
_test_Rem_RoundHalfOdd_Z_P2 =  Dict
_test_Rem_RoundHalfOdd_Z_P3 :: Dict (K.Rem 'K.RoundHalfOdd Z (P 3) ~ Z)
_test_Rem_RoundHalfOdd_Z_P3 =  Dict
_test_Rem_RoundHalfOdd_Z_P4 :: Dict (K.Rem 'K.RoundHalfOdd Z (P 4) ~ Z)
_test_Rem_RoundHalfOdd_Z_P4 =  Dict
_test_Div_RoundHalfOdd_P1_N1 :: Dict (K.Div 'K.RoundHalfOdd (P 1) (N 1) ~ N 1)
_test_Div_RoundHalfOdd_P1_N1 =  Dict
_test_Rem_RoundHalfOdd_P1_N1 :: Dict (K.Rem 'K.RoundHalfOdd (P 1) (N 1) ~ Z)
_test_Rem_RoundHalfOdd_P1_N1 =  Dict
_test_Div_RoundHalfOdd_P1_N2 :: Dict (K.Div 'K.RoundHalfOdd (P 1) (N 2) ~ N 1)
_test_Div_RoundHalfOdd_P1_N2 =  Dict
_test_Rem_RoundHalfOdd_P1_N2 :: Dict (K.Rem 'K.RoundHalfOdd (P 1) (N 2) ~ N 1)
_test_Rem_RoundHalfOdd_P1_N2 =  Dict
_test_Div_RoundHalfOdd_P1_N3 :: Dict (K.Div 'K.RoundHalfOdd (P 1) (N 3) ~ Z)
_test_Div_RoundHalfOdd_P1_N3 =  Dict
_test_Rem_RoundHalfOdd_P1_N3 :: Dict (K.Rem 'K.RoundHalfOdd (P 1) (N 3) ~ P 1)
_test_Rem_RoundHalfOdd_P1_N3 =  Dict
_test_Div_RoundHalfOdd_P1_N4 :: Dict (K.Div 'K.RoundHalfOdd (P 1) (N 4) ~ Z)
_test_Div_RoundHalfOdd_P1_N4 =  Dict
_test_Rem_RoundHalfOdd_P1_N4 :: Dict (K.Rem 'K.RoundHalfOdd (P 1) (N 4) ~ P 1)
_test_Rem_RoundHalfOdd_P1_N4 =  Dict
_test_Div_RoundHalfOdd_P1_P1 :: Dict (K.Div 'K.RoundHalfOdd (P 1) (P 1) ~ P 1)
_test_Div_RoundHalfOdd_P1_P1 =  Dict
_test_Rem_RoundHalfOdd_P1_P1 :: Dict (K.Rem 'K.RoundHalfOdd (P 1) (P 1) ~ Z)
_test_Rem_RoundHalfOdd_P1_P1 =  Dict
_test_Div_RoundHalfOdd_P1_P2 :: Dict (K.Div 'K.RoundHalfOdd (P 1) (P 2) ~ P 1)
_test_Div_RoundHalfOdd_P1_P2 =  Dict
_test_Rem_RoundHalfOdd_P1_P2 :: Dict (K.Rem 'K.RoundHalfOdd (P 1) (P 2) ~ N 1)
_test_Rem_RoundHalfOdd_P1_P2 =  Dict
_test_Div_RoundHalfOdd_P1_P3 :: Dict (K.Div 'K.RoundHalfOdd (P 1) (P 3) ~ Z)
_test_Div_RoundHalfOdd_P1_P3 =  Dict
_test_Rem_RoundHalfOdd_P1_P3 :: Dict (K.Rem 'K.RoundHalfOdd (P 1) (P 3) ~ P 1)
_test_Rem_RoundHalfOdd_P1_P3 =  Dict
_test_Div_RoundHalfOdd_P1_P4 :: Dict (K.Div 'K.RoundHalfOdd (P 1) (P 4) ~ Z)
_test_Div_RoundHalfOdd_P1_P4 =  Dict
_test_Rem_RoundHalfOdd_P1_P4 :: Dict (K.Rem 'K.RoundHalfOdd (P 1) (P 4) ~ P 1)
_test_Rem_RoundHalfOdd_P1_P4 =  Dict
_test_Div_RoundHalfOdd_P2_N1 :: Dict (K.Div 'K.RoundHalfOdd (P 2) (N 1) ~ N 2)
_test_Div_RoundHalfOdd_P2_N1 =  Dict
_test_Rem_RoundHalfOdd_P2_N1 :: Dict (K.Rem 'K.RoundHalfOdd (P 2) (N 1) ~ Z)
_test_Rem_RoundHalfOdd_P2_N1 =  Dict
_test_Div_RoundHalfOdd_P2_N2 :: Dict (K.Div 'K.RoundHalfOdd (P 2) (N 2) ~ N 1)
_test_Div_RoundHalfOdd_P2_N2 =  Dict
_test_Rem_RoundHalfOdd_P2_N2 :: Dict (K.Rem 'K.RoundHalfOdd (P 2) (N 2) ~ Z)
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
_test_Rem_RoundHalfOdd_P2_P1 :: Dict (K.Rem 'K.RoundHalfOdd (P 2) (P 1) ~ Z)
_test_Rem_RoundHalfOdd_P2_P1 =  Dict
_test_Div_RoundHalfOdd_P2_P2 :: Dict (K.Div 'K.RoundHalfOdd (P 2) (P 2) ~ P 1)
_test_Div_RoundHalfOdd_P2_P2 =  Dict
_test_Rem_RoundHalfOdd_P2_P2 :: Dict (K.Rem 'K.RoundHalfOdd (P 2) (P 2) ~ Z)
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
_test_Rem_RoundHalfOdd_P3_N1 :: Dict (K.Rem 'K.RoundHalfOdd (P 3) (N 1) ~ Z)
_test_Rem_RoundHalfOdd_P3_N1 =  Dict
_test_Div_RoundHalfOdd_P3_N2 :: Dict (K.Div 'K.RoundHalfOdd (P 3) (N 2) ~ N 1)
_test_Div_RoundHalfOdd_P3_N2 =  Dict
_test_Rem_RoundHalfOdd_P3_N2 :: Dict (K.Rem 'K.RoundHalfOdd (P 3) (N 2) ~ P 1)
_test_Rem_RoundHalfOdd_P3_N2 =  Dict
_test_Div_RoundHalfOdd_P3_N3 :: Dict (K.Div 'K.RoundHalfOdd (P 3) (N 3) ~ N 1)
_test_Div_RoundHalfOdd_P3_N3 =  Dict
_test_Rem_RoundHalfOdd_P3_N3 :: Dict (K.Rem 'K.RoundHalfOdd (P 3) (N 3) ~ Z)
_test_Rem_RoundHalfOdd_P3_N3 =  Dict
_test_Div_RoundHalfOdd_P3_N4 :: Dict (K.Div 'K.RoundHalfOdd (P 3) (N 4) ~ N 1)
_test_Div_RoundHalfOdd_P3_N4 =  Dict
_test_Rem_RoundHalfOdd_P3_N4 :: Dict (K.Rem 'K.RoundHalfOdd (P 3) (N 4) ~ N 1)
_test_Rem_RoundHalfOdd_P3_N4 =  Dict
_test_Div_RoundHalfOdd_P3_P1 :: Dict (K.Div 'K.RoundHalfOdd (P 3) (P 1) ~ P 3)
_test_Div_RoundHalfOdd_P3_P1 =  Dict
_test_Rem_RoundHalfOdd_P3_P1 :: Dict (K.Rem 'K.RoundHalfOdd (P 3) (P 1) ~ Z)
_test_Rem_RoundHalfOdd_P3_P1 =  Dict
_test_Div_RoundHalfOdd_P3_P2 :: Dict (K.Div 'K.RoundHalfOdd (P 3) (P 2) ~ P 1)
_test_Div_RoundHalfOdd_P3_P2 =  Dict
_test_Rem_RoundHalfOdd_P3_P2 :: Dict (K.Rem 'K.RoundHalfOdd (P 3) (P 2) ~ P 1)
_test_Rem_RoundHalfOdd_P3_P2 =  Dict
_test_Div_RoundHalfOdd_P3_P3 :: Dict (K.Div 'K.RoundHalfOdd (P 3) (P 3) ~ P 1)
_test_Div_RoundHalfOdd_P3_P3 =  Dict
_test_Rem_RoundHalfOdd_P3_P3 :: Dict (K.Rem 'K.RoundHalfOdd (P 3) (P 3) ~ Z)
_test_Rem_RoundHalfOdd_P3_P3 =  Dict
_test_Div_RoundHalfOdd_P3_P4 :: Dict (K.Div 'K.RoundHalfOdd (P 3) (P 4) ~ P 1)
_test_Div_RoundHalfOdd_P3_P4 =  Dict
_test_Rem_RoundHalfOdd_P3_P4 :: Dict (K.Rem 'K.RoundHalfOdd (P 3) (P 4) ~ N 1)
_test_Rem_RoundHalfOdd_P3_P4 =  Dict
_test_Div_RoundHalfOdd_P4_N1 :: Dict (K.Div 'K.RoundHalfOdd (P 4) (N 1) ~ N 4)
_test_Div_RoundHalfOdd_P4_N1 =  Dict
_test_Rem_RoundHalfOdd_P4_N1 :: Dict (K.Rem 'K.RoundHalfOdd (P 4) (N 1) ~ Z)
_test_Rem_RoundHalfOdd_P4_N1 =  Dict
_test_Div_RoundHalfOdd_P4_N2 :: Dict (K.Div 'K.RoundHalfOdd (P 4) (N 2) ~ N 2)
_test_Div_RoundHalfOdd_P4_N2 =  Dict
_test_Rem_RoundHalfOdd_P4_N2 :: Dict (K.Rem 'K.RoundHalfOdd (P 4) (N 2) ~ Z)
_test_Rem_RoundHalfOdd_P4_N2 =  Dict
_test_Div_RoundHalfOdd_P4_N3 :: Dict (K.Div 'K.RoundHalfOdd (P 4) (N 3) ~ N 1)
_test_Div_RoundHalfOdd_P4_N3 =  Dict
_test_Rem_RoundHalfOdd_P4_N3 :: Dict (K.Rem 'K.RoundHalfOdd (P 4) (N 3) ~ P 1)
_test_Rem_RoundHalfOdd_P4_N3 =  Dict
_test_Div_RoundHalfOdd_P4_N4 :: Dict (K.Div 'K.RoundHalfOdd (P 4) (N 4) ~ N 1)
_test_Div_RoundHalfOdd_P4_N4 =  Dict
_test_Rem_RoundHalfOdd_P4_N4 :: Dict (K.Rem 'K.RoundHalfOdd (P 4) (N 4) ~ Z)
_test_Rem_RoundHalfOdd_P4_N4 =  Dict
_test_Div_RoundHalfOdd_P4_P1 :: Dict (K.Div 'K.RoundHalfOdd (P 4) (P 1) ~ P 4)
_test_Div_RoundHalfOdd_P4_P1 =  Dict
_test_Rem_RoundHalfOdd_P4_P1 :: Dict (K.Rem 'K.RoundHalfOdd (P 4) (P 1) ~ Z)
_test_Rem_RoundHalfOdd_P4_P1 =  Dict
_test_Div_RoundHalfOdd_P4_P2 :: Dict (K.Div 'K.RoundHalfOdd (P 4) (P 2) ~ P 2)
_test_Div_RoundHalfOdd_P4_P2 =  Dict
_test_Rem_RoundHalfOdd_P4_P2 :: Dict (K.Rem 'K.RoundHalfOdd (P 4) (P 2) ~ Z)
_test_Rem_RoundHalfOdd_P4_P2 =  Dict
_test_Div_RoundHalfOdd_P4_P3 :: Dict (K.Div 'K.RoundHalfOdd (P 4) (P 3) ~ P 1)
_test_Div_RoundHalfOdd_P4_P3 =  Dict
_test_Rem_RoundHalfOdd_P4_P3 :: Dict (K.Rem 'K.RoundHalfOdd (P 4) (P 3) ~ P 1)
_test_Rem_RoundHalfOdd_P4_P3 =  Dict
_test_Div_RoundHalfOdd_P4_P4 :: Dict (K.Div 'K.RoundHalfOdd (P 4) (P 4) ~ P 1)
_test_Div_RoundHalfOdd_P4_P4 =  Dict
_test_Rem_RoundHalfOdd_P4_P4 :: Dict (K.Rem 'K.RoundHalfOdd (P 4) (P 4) ~ Z)
_test_Rem_RoundHalfOdd_P4_P4 =  Dict
_test_Div_RoundHalfUp_N1_N1 :: Dict (K.Div 'K.RoundHalfUp (N 1) (N 1) ~ P 1)
_test_Div_RoundHalfUp_N1_N1 =  Dict
_test_Rem_RoundHalfUp_N1_N1 :: Dict (K.Rem 'K.RoundHalfUp (N 1) (N 1) ~ Z)
_test_Rem_RoundHalfUp_N1_N1 =  Dict
_test_Div_RoundHalfUp_N1_N2 :: Dict (K.Div 'K.RoundHalfUp (N 1) (N 2) ~ P 1)
_test_Div_RoundHalfUp_N1_N2 =  Dict
_test_Rem_RoundHalfUp_N1_N2 :: Dict (K.Rem 'K.RoundHalfUp (N 1) (N 2) ~ P 1)
_test_Rem_RoundHalfUp_N1_N2 =  Dict
_test_Div_RoundHalfUp_N1_N3 :: Dict (K.Div 'K.RoundHalfUp (N 1) (N 3) ~ Z)
_test_Div_RoundHalfUp_N1_N3 =  Dict
_test_Rem_RoundHalfUp_N1_N3 :: Dict (K.Rem 'K.RoundHalfUp (N 1) (N 3) ~ N 1)
_test_Rem_RoundHalfUp_N1_N3 =  Dict
_test_Div_RoundHalfUp_N1_N4 :: Dict (K.Div 'K.RoundHalfUp (N 1) (N 4) ~ Z)
_test_Div_RoundHalfUp_N1_N4 =  Dict
_test_Rem_RoundHalfUp_N1_N4 :: Dict (K.Rem 'K.RoundHalfUp (N 1) (N 4) ~ N 1)
_test_Rem_RoundHalfUp_N1_N4 =  Dict
_test_Div_RoundHalfUp_N1_P1 :: Dict (K.Div 'K.RoundHalfUp (N 1) (P 1) ~ N 1)
_test_Div_RoundHalfUp_N1_P1 =  Dict
_test_Rem_RoundHalfUp_N1_P1 :: Dict (K.Rem 'K.RoundHalfUp (N 1) (P 1) ~ Z)
_test_Rem_RoundHalfUp_N1_P1 =  Dict
_test_Div_RoundHalfUp_N1_P2 :: Dict (K.Div 'K.RoundHalfUp (N 1) (P 2) ~ Z)
_test_Div_RoundHalfUp_N1_P2 =  Dict
_test_Rem_RoundHalfUp_N1_P2 :: Dict (K.Rem 'K.RoundHalfUp (N 1) (P 2) ~ N 1)
_test_Rem_RoundHalfUp_N1_P2 =  Dict
_test_Div_RoundHalfUp_N1_P3 :: Dict (K.Div 'K.RoundHalfUp (N 1) (P 3) ~ Z)
_test_Div_RoundHalfUp_N1_P3 =  Dict
_test_Rem_RoundHalfUp_N1_P3 :: Dict (K.Rem 'K.RoundHalfUp (N 1) (P 3) ~ N 1)
_test_Rem_RoundHalfUp_N1_P3 =  Dict
_test_Div_RoundHalfUp_N1_P4 :: Dict (K.Div 'K.RoundHalfUp (N 1) (P 4) ~ Z)
_test_Div_RoundHalfUp_N1_P4 =  Dict
_test_Rem_RoundHalfUp_N1_P4 :: Dict (K.Rem 'K.RoundHalfUp (N 1) (P 4) ~ N 1)
_test_Rem_RoundHalfUp_N1_P4 =  Dict
_test_Div_RoundHalfUp_N2_N1 :: Dict (K.Div 'K.RoundHalfUp (N 2) (N 1) ~ P 2)
_test_Div_RoundHalfUp_N2_N1 =  Dict
_test_Rem_RoundHalfUp_N2_N1 :: Dict (K.Rem 'K.RoundHalfUp (N 2) (N 1) ~ Z)
_test_Rem_RoundHalfUp_N2_N1 =  Dict
_test_Div_RoundHalfUp_N2_N2 :: Dict (K.Div 'K.RoundHalfUp (N 2) (N 2) ~ P 1)
_test_Div_RoundHalfUp_N2_N2 =  Dict
_test_Rem_RoundHalfUp_N2_N2 :: Dict (K.Rem 'K.RoundHalfUp (N 2) (N 2) ~ Z)
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
_test_Rem_RoundHalfUp_N2_P1 :: Dict (K.Rem 'K.RoundHalfUp (N 2) (P 1) ~ Z)
_test_Rem_RoundHalfUp_N2_P1 =  Dict
_test_Div_RoundHalfUp_N2_P2 :: Dict (K.Div 'K.RoundHalfUp (N 2) (P 2) ~ N 1)
_test_Div_RoundHalfUp_N2_P2 =  Dict
_test_Rem_RoundHalfUp_N2_P2 :: Dict (K.Rem 'K.RoundHalfUp (N 2) (P 2) ~ Z)
_test_Rem_RoundHalfUp_N2_P2 =  Dict
_test_Div_RoundHalfUp_N2_P3 :: Dict (K.Div 'K.RoundHalfUp (N 2) (P 3) ~ N 1)
_test_Div_RoundHalfUp_N2_P3 =  Dict
_test_Rem_RoundHalfUp_N2_P3 :: Dict (K.Rem 'K.RoundHalfUp (N 2) (P 3) ~ P 1)
_test_Rem_RoundHalfUp_N2_P3 =  Dict
_test_Div_RoundHalfUp_N2_P4 :: Dict (K.Div 'K.RoundHalfUp (N 2) (P 4) ~ Z)
_test_Div_RoundHalfUp_N2_P4 =  Dict
_test_Rem_RoundHalfUp_N2_P4 :: Dict (K.Rem 'K.RoundHalfUp (N 2) (P 4) ~ N 2)
_test_Rem_RoundHalfUp_N2_P4 =  Dict
_test_Div_RoundHalfUp_N3_N1 :: Dict (K.Div 'K.RoundHalfUp (N 3) (N 1) ~ P 3)
_test_Div_RoundHalfUp_N3_N1 =  Dict
_test_Rem_RoundHalfUp_N3_N1 :: Dict (K.Rem 'K.RoundHalfUp (N 3) (N 1) ~ Z)
_test_Rem_RoundHalfUp_N3_N1 =  Dict
_test_Div_RoundHalfUp_N3_N2 :: Dict (K.Div 'K.RoundHalfUp (N 3) (N 2) ~ P 2)
_test_Div_RoundHalfUp_N3_N2 =  Dict
_test_Rem_RoundHalfUp_N3_N2 :: Dict (K.Rem 'K.RoundHalfUp (N 3) (N 2) ~ P 1)
_test_Rem_RoundHalfUp_N3_N2 =  Dict
_test_Div_RoundHalfUp_N3_N3 :: Dict (K.Div 'K.RoundHalfUp (N 3) (N 3) ~ P 1)
_test_Div_RoundHalfUp_N3_N3 =  Dict
_test_Rem_RoundHalfUp_N3_N3 :: Dict (K.Rem 'K.RoundHalfUp (N 3) (N 3) ~ Z)
_test_Rem_RoundHalfUp_N3_N3 =  Dict
_test_Div_RoundHalfUp_N3_N4 :: Dict (K.Div 'K.RoundHalfUp (N 3) (N 4) ~ P 1)
_test_Div_RoundHalfUp_N3_N4 =  Dict
_test_Rem_RoundHalfUp_N3_N4 :: Dict (K.Rem 'K.RoundHalfUp (N 3) (N 4) ~ P 1)
_test_Rem_RoundHalfUp_N3_N4 =  Dict
_test_Div_RoundHalfUp_N3_P1 :: Dict (K.Div 'K.RoundHalfUp (N 3) (P 1) ~ N 3)
_test_Div_RoundHalfUp_N3_P1 =  Dict
_test_Rem_RoundHalfUp_N3_P1 :: Dict (K.Rem 'K.RoundHalfUp (N 3) (P 1) ~ Z)
_test_Rem_RoundHalfUp_N3_P1 =  Dict
_test_Div_RoundHalfUp_N3_P2 :: Dict (K.Div 'K.RoundHalfUp (N 3) (P 2) ~ N 1)
_test_Div_RoundHalfUp_N3_P2 =  Dict
_test_Rem_RoundHalfUp_N3_P2 :: Dict (K.Rem 'K.RoundHalfUp (N 3) (P 2) ~ N 1)
_test_Rem_RoundHalfUp_N3_P2 =  Dict
_test_Div_RoundHalfUp_N3_P3 :: Dict (K.Div 'K.RoundHalfUp (N 3) (P 3) ~ N 1)
_test_Div_RoundHalfUp_N3_P3 =  Dict
_test_Rem_RoundHalfUp_N3_P3 :: Dict (K.Rem 'K.RoundHalfUp (N 3) (P 3) ~ Z)
_test_Rem_RoundHalfUp_N3_P3 =  Dict
_test_Div_RoundHalfUp_N3_P4 :: Dict (K.Div 'K.RoundHalfUp (N 3) (P 4) ~ N 1)
_test_Div_RoundHalfUp_N3_P4 =  Dict
_test_Rem_RoundHalfUp_N3_P4 :: Dict (K.Rem 'K.RoundHalfUp (N 3) (P 4) ~ P 1)
_test_Rem_RoundHalfUp_N3_P4 =  Dict
_test_Div_RoundHalfUp_N4_N1 :: Dict (K.Div 'K.RoundHalfUp (N 4) (N 1) ~ P 4)
_test_Div_RoundHalfUp_N4_N1 =  Dict
_test_Rem_RoundHalfUp_N4_N1 :: Dict (K.Rem 'K.RoundHalfUp (N 4) (N 1) ~ Z)
_test_Rem_RoundHalfUp_N4_N1 =  Dict
_test_Div_RoundHalfUp_N4_N2 :: Dict (K.Div 'K.RoundHalfUp (N 4) (N 2) ~ P 2)
_test_Div_RoundHalfUp_N4_N2 =  Dict
_test_Rem_RoundHalfUp_N4_N2 :: Dict (K.Rem 'K.RoundHalfUp (N 4) (N 2) ~ Z)
_test_Rem_RoundHalfUp_N4_N2 =  Dict
_test_Div_RoundHalfUp_N4_N3 :: Dict (K.Div 'K.RoundHalfUp (N 4) (N 3) ~ P 1)
_test_Div_RoundHalfUp_N4_N3 =  Dict
_test_Rem_RoundHalfUp_N4_N3 :: Dict (K.Rem 'K.RoundHalfUp (N 4) (N 3) ~ N 1)
_test_Rem_RoundHalfUp_N4_N3 =  Dict
_test_Div_RoundHalfUp_N4_N4 :: Dict (K.Div 'K.RoundHalfUp (N 4) (N 4) ~ P 1)
_test_Div_RoundHalfUp_N4_N4 =  Dict
_test_Rem_RoundHalfUp_N4_N4 :: Dict (K.Rem 'K.RoundHalfUp (N 4) (N 4) ~ Z)
_test_Rem_RoundHalfUp_N4_N4 =  Dict
_test_Div_RoundHalfUp_N4_P1 :: Dict (K.Div 'K.RoundHalfUp (N 4) (P 1) ~ N 4)
_test_Div_RoundHalfUp_N4_P1 =  Dict
_test_Rem_RoundHalfUp_N4_P1 :: Dict (K.Rem 'K.RoundHalfUp (N 4) (P 1) ~ Z)
_test_Rem_RoundHalfUp_N4_P1 =  Dict
_test_Div_RoundHalfUp_N4_P2 :: Dict (K.Div 'K.RoundHalfUp (N 4) (P 2) ~ N 2)
_test_Div_RoundHalfUp_N4_P2 =  Dict
_test_Rem_RoundHalfUp_N4_P2 :: Dict (K.Rem 'K.RoundHalfUp (N 4) (P 2) ~ Z)
_test_Rem_RoundHalfUp_N4_P2 =  Dict
_test_Div_RoundHalfUp_N4_P3 :: Dict (K.Div 'K.RoundHalfUp (N 4) (P 3) ~ N 1)
_test_Div_RoundHalfUp_N4_P3 =  Dict
_test_Rem_RoundHalfUp_N4_P3 :: Dict (K.Rem 'K.RoundHalfUp (N 4) (P 3) ~ N 1)
_test_Rem_RoundHalfUp_N4_P3 =  Dict
_test_Div_RoundHalfUp_N4_P4 :: Dict (K.Div 'K.RoundHalfUp (N 4) (P 4) ~ N 1)
_test_Div_RoundHalfUp_N4_P4 =  Dict
_test_Rem_RoundHalfUp_N4_P4 :: Dict (K.Rem 'K.RoundHalfUp (N 4) (P 4) ~ Z)
_test_Rem_RoundHalfUp_N4_P4 =  Dict
_test_Rem_RoundHalfUp_Z_N1 :: Dict (K.Rem 'K.RoundHalfUp Z (N 1) ~ Z)
_test_Rem_RoundHalfUp_Z_N1 =  Dict
_test_Rem_RoundHalfUp_Z_N2 :: Dict (K.Rem 'K.RoundHalfUp Z (N 2) ~ Z)
_test_Rem_RoundHalfUp_Z_N2 =  Dict
_test_Rem_RoundHalfUp_Z_N3 :: Dict (K.Rem 'K.RoundHalfUp Z (N 3) ~ Z)
_test_Rem_RoundHalfUp_Z_N3 =  Dict
_test_Rem_RoundHalfUp_Z_N4 :: Dict (K.Rem 'K.RoundHalfUp Z (N 4) ~ Z)
_test_Rem_RoundHalfUp_Z_N4 =  Dict
_test_Rem_RoundHalfUp_Z_P1 :: Dict (K.Rem 'K.RoundHalfUp Z (P 1) ~ Z)
_test_Rem_RoundHalfUp_Z_P1 =  Dict
_test_Rem_RoundHalfUp_Z_P2 :: Dict (K.Rem 'K.RoundHalfUp Z (P 2) ~ Z)
_test_Rem_RoundHalfUp_Z_P2 =  Dict
_test_Rem_RoundHalfUp_Z_P3 :: Dict (K.Rem 'K.RoundHalfUp Z (P 3) ~ Z)
_test_Rem_RoundHalfUp_Z_P3 =  Dict
_test_Rem_RoundHalfUp_Z_P4 :: Dict (K.Rem 'K.RoundHalfUp Z (P 4) ~ Z)
_test_Rem_RoundHalfUp_Z_P4 =  Dict
_test_Div_RoundHalfUp_P1_N1 :: Dict (K.Div 'K.RoundHalfUp (P 1) (N 1) ~ N 1)
_test_Div_RoundHalfUp_P1_N1 =  Dict
_test_Rem_RoundHalfUp_P1_N1 :: Dict (K.Rem 'K.RoundHalfUp (P 1) (N 1) ~ Z)
_test_Rem_RoundHalfUp_P1_N1 =  Dict
_test_Div_RoundHalfUp_P1_N2 :: Dict (K.Div 'K.RoundHalfUp (P 1) (N 2) ~ Z)
_test_Div_RoundHalfUp_P1_N2 =  Dict
_test_Rem_RoundHalfUp_P1_N2 :: Dict (K.Rem 'K.RoundHalfUp (P 1) (N 2) ~ P 1)
_test_Rem_RoundHalfUp_P1_N2 =  Dict
_test_Div_RoundHalfUp_P1_N3 :: Dict (K.Div 'K.RoundHalfUp (P 1) (N 3) ~ Z)
_test_Div_RoundHalfUp_P1_N3 =  Dict
_test_Rem_RoundHalfUp_P1_N3 :: Dict (K.Rem 'K.RoundHalfUp (P 1) (N 3) ~ P 1)
_test_Rem_RoundHalfUp_P1_N3 =  Dict
_test_Div_RoundHalfUp_P1_N4 :: Dict (K.Div 'K.RoundHalfUp (P 1) (N 4) ~ Z)
_test_Div_RoundHalfUp_P1_N4 =  Dict
_test_Rem_RoundHalfUp_P1_N4 :: Dict (K.Rem 'K.RoundHalfUp (P 1) (N 4) ~ P 1)
_test_Rem_RoundHalfUp_P1_N4 =  Dict
_test_Div_RoundHalfUp_P1_P1 :: Dict (K.Div 'K.RoundHalfUp (P 1) (P 1) ~ P 1)
_test_Div_RoundHalfUp_P1_P1 =  Dict
_test_Rem_RoundHalfUp_P1_P1 :: Dict (K.Rem 'K.RoundHalfUp (P 1) (P 1) ~ Z)
_test_Rem_RoundHalfUp_P1_P1 =  Dict
_test_Div_RoundHalfUp_P1_P2 :: Dict (K.Div 'K.RoundHalfUp (P 1) (P 2) ~ P 1)
_test_Div_RoundHalfUp_P1_P2 =  Dict
_test_Rem_RoundHalfUp_P1_P2 :: Dict (K.Rem 'K.RoundHalfUp (P 1) (P 2) ~ N 1)
_test_Rem_RoundHalfUp_P1_P2 =  Dict
_test_Div_RoundHalfUp_P1_P3 :: Dict (K.Div 'K.RoundHalfUp (P 1) (P 3) ~ Z)
_test_Div_RoundHalfUp_P1_P3 =  Dict
_test_Rem_RoundHalfUp_P1_P3 :: Dict (K.Rem 'K.RoundHalfUp (P 1) (P 3) ~ P 1)
_test_Rem_RoundHalfUp_P1_P3 =  Dict
_test_Div_RoundHalfUp_P1_P4 :: Dict (K.Div 'K.RoundHalfUp (P 1) (P 4) ~ Z)
_test_Div_RoundHalfUp_P1_P4 =  Dict
_test_Rem_RoundHalfUp_P1_P4 :: Dict (K.Rem 'K.RoundHalfUp (P 1) (P 4) ~ P 1)
_test_Rem_RoundHalfUp_P1_P4 =  Dict
_test_Div_RoundHalfUp_P2_N1 :: Dict (K.Div 'K.RoundHalfUp (P 2) (N 1) ~ N 2)
_test_Div_RoundHalfUp_P2_N1 =  Dict
_test_Rem_RoundHalfUp_P2_N1 :: Dict (K.Rem 'K.RoundHalfUp (P 2) (N 1) ~ Z)
_test_Rem_RoundHalfUp_P2_N1 =  Dict
_test_Div_RoundHalfUp_P2_N2 :: Dict (K.Div 'K.RoundHalfUp (P 2) (N 2) ~ N 1)
_test_Div_RoundHalfUp_P2_N2 =  Dict
_test_Rem_RoundHalfUp_P2_N2 :: Dict (K.Rem 'K.RoundHalfUp (P 2) (N 2) ~ Z)
_test_Rem_RoundHalfUp_P2_N2 =  Dict
_test_Div_RoundHalfUp_P2_N3 :: Dict (K.Div 'K.RoundHalfUp (P 2) (N 3) ~ N 1)
_test_Div_RoundHalfUp_P2_N3 =  Dict
_test_Rem_RoundHalfUp_P2_N3 :: Dict (K.Rem 'K.RoundHalfUp (P 2) (N 3) ~ N 1)
_test_Rem_RoundHalfUp_P2_N3 =  Dict
_test_Div_RoundHalfUp_P2_N4 :: Dict (K.Div 'K.RoundHalfUp (P 2) (N 4) ~ Z)
_test_Div_RoundHalfUp_P2_N4 =  Dict
_test_Rem_RoundHalfUp_P2_N4 :: Dict (K.Rem 'K.RoundHalfUp (P 2) (N 4) ~ P 2)
_test_Rem_RoundHalfUp_P2_N4 =  Dict
_test_Div_RoundHalfUp_P2_P1 :: Dict (K.Div 'K.RoundHalfUp (P 2) (P 1) ~ P 2)
_test_Div_RoundHalfUp_P2_P1 =  Dict
_test_Rem_RoundHalfUp_P2_P1 :: Dict (K.Rem 'K.RoundHalfUp (P 2) (P 1) ~ Z)
_test_Rem_RoundHalfUp_P2_P1 =  Dict
_test_Div_RoundHalfUp_P2_P2 :: Dict (K.Div 'K.RoundHalfUp (P 2) (P 2) ~ P 1)
_test_Div_RoundHalfUp_P2_P2 =  Dict
_test_Rem_RoundHalfUp_P2_P2 :: Dict (K.Rem 'K.RoundHalfUp (P 2) (P 2) ~ Z)
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
_test_Rem_RoundHalfUp_P3_N1 :: Dict (K.Rem 'K.RoundHalfUp (P 3) (N 1) ~ Z)
_test_Rem_RoundHalfUp_P3_N1 =  Dict
_test_Div_RoundHalfUp_P3_N2 :: Dict (K.Div 'K.RoundHalfUp (P 3) (N 2) ~ N 1)
_test_Div_RoundHalfUp_P3_N2 =  Dict
_test_Rem_RoundHalfUp_P3_N2 :: Dict (K.Rem 'K.RoundHalfUp (P 3) (N 2) ~ P 1)
_test_Rem_RoundHalfUp_P3_N2 =  Dict
_test_Div_RoundHalfUp_P3_N3 :: Dict (K.Div 'K.RoundHalfUp (P 3) (N 3) ~ N 1)
_test_Div_RoundHalfUp_P3_N3 =  Dict
_test_Rem_RoundHalfUp_P3_N3 :: Dict (K.Rem 'K.RoundHalfUp (P 3) (N 3) ~ Z)
_test_Rem_RoundHalfUp_P3_N3 =  Dict
_test_Div_RoundHalfUp_P3_N4 :: Dict (K.Div 'K.RoundHalfUp (P 3) (N 4) ~ N 1)
_test_Div_RoundHalfUp_P3_N4 =  Dict
_test_Rem_RoundHalfUp_P3_N4 :: Dict (K.Rem 'K.RoundHalfUp (P 3) (N 4) ~ N 1)
_test_Rem_RoundHalfUp_P3_N4 =  Dict
_test_Div_RoundHalfUp_P3_P1 :: Dict (K.Div 'K.RoundHalfUp (P 3) (P 1) ~ P 3)
_test_Div_RoundHalfUp_P3_P1 =  Dict
_test_Rem_RoundHalfUp_P3_P1 :: Dict (K.Rem 'K.RoundHalfUp (P 3) (P 1) ~ Z)
_test_Rem_RoundHalfUp_P3_P1 =  Dict
_test_Div_RoundHalfUp_P3_P2 :: Dict (K.Div 'K.RoundHalfUp (P 3) (P 2) ~ P 2)
_test_Div_RoundHalfUp_P3_P2 =  Dict
_test_Rem_RoundHalfUp_P3_P2 :: Dict (K.Rem 'K.RoundHalfUp (P 3) (P 2) ~ N 1)
_test_Rem_RoundHalfUp_P3_P2 =  Dict
_test_Div_RoundHalfUp_P3_P3 :: Dict (K.Div 'K.RoundHalfUp (P 3) (P 3) ~ P 1)
_test_Div_RoundHalfUp_P3_P3 =  Dict
_test_Rem_RoundHalfUp_P3_P3 :: Dict (K.Rem 'K.RoundHalfUp (P 3) (P 3) ~ Z)
_test_Rem_RoundHalfUp_P3_P3 =  Dict
_test_Div_RoundHalfUp_P3_P4 :: Dict (K.Div 'K.RoundHalfUp (P 3) (P 4) ~ P 1)
_test_Div_RoundHalfUp_P3_P4 =  Dict
_test_Rem_RoundHalfUp_P3_P4 :: Dict (K.Rem 'K.RoundHalfUp (P 3) (P 4) ~ N 1)
_test_Rem_RoundHalfUp_P3_P4 =  Dict
_test_Div_RoundHalfUp_P4_N1 :: Dict (K.Div 'K.RoundHalfUp (P 4) (N 1) ~ N 4)
_test_Div_RoundHalfUp_P4_N1 =  Dict
_test_Rem_RoundHalfUp_P4_N1 :: Dict (K.Rem 'K.RoundHalfUp (P 4) (N 1) ~ Z)
_test_Rem_RoundHalfUp_P4_N1 =  Dict
_test_Div_RoundHalfUp_P4_N2 :: Dict (K.Div 'K.RoundHalfUp (P 4) (N 2) ~ N 2)
_test_Div_RoundHalfUp_P4_N2 =  Dict
_test_Rem_RoundHalfUp_P4_N2 :: Dict (K.Rem 'K.RoundHalfUp (P 4) (N 2) ~ Z)
_test_Rem_RoundHalfUp_P4_N2 =  Dict
_test_Div_RoundHalfUp_P4_N3 :: Dict (K.Div 'K.RoundHalfUp (P 4) (N 3) ~ N 1)
_test_Div_RoundHalfUp_P4_N3 =  Dict
_test_Rem_RoundHalfUp_P4_N3 :: Dict (K.Rem 'K.RoundHalfUp (P 4) (N 3) ~ P 1)
_test_Rem_RoundHalfUp_P4_N3 =  Dict
_test_Div_RoundHalfUp_P4_N4 :: Dict (K.Div 'K.RoundHalfUp (P 4) (N 4) ~ N 1)
_test_Div_RoundHalfUp_P4_N4 =  Dict
_test_Rem_RoundHalfUp_P4_N4 :: Dict (K.Rem 'K.RoundHalfUp (P 4) (N 4) ~ Z)
_test_Rem_RoundHalfUp_P4_N4 =  Dict
_test_Div_RoundHalfUp_P4_P1 :: Dict (K.Div 'K.RoundHalfUp (P 4) (P 1) ~ P 4)
_test_Div_RoundHalfUp_P4_P1 =  Dict
_test_Rem_RoundHalfUp_P4_P1 :: Dict (K.Rem 'K.RoundHalfUp (P 4) (P 1) ~ Z)
_test_Rem_RoundHalfUp_P4_P1 =  Dict
_test_Div_RoundHalfUp_P4_P2 :: Dict (K.Div 'K.RoundHalfUp (P 4) (P 2) ~ P 2)
_test_Div_RoundHalfUp_P4_P2 =  Dict
_test_Rem_RoundHalfUp_P4_P2 :: Dict (K.Rem 'K.RoundHalfUp (P 4) (P 2) ~ Z)
_test_Rem_RoundHalfUp_P4_P2 =  Dict
_test_Div_RoundHalfUp_P4_P3 :: Dict (K.Div 'K.RoundHalfUp (P 4) (P 3) ~ P 1)
_test_Div_RoundHalfUp_P4_P3 =  Dict
_test_Rem_RoundHalfUp_P4_P3 :: Dict (K.Rem 'K.RoundHalfUp (P 4) (P 3) ~ P 1)
_test_Rem_RoundHalfUp_P4_P3 =  Dict
_test_Div_RoundHalfUp_P4_P4 :: Dict (K.Div 'K.RoundHalfUp (P 4) (P 4) ~ P 1)
_test_Div_RoundHalfUp_P4_P4 =  Dict
_test_Rem_RoundHalfUp_P4_P4 :: Dict (K.Rem 'K.RoundHalfUp (P 4) (P 4) ~ Z)
_test_Rem_RoundHalfUp_P4_P4 =  Dict
_test_Div_RoundHalfZero_N1_N1 :: Dict (K.Div 'K.RoundHalfZero (N 1) (N 1) ~ P 1)
_test_Div_RoundHalfZero_N1_N1 =  Dict
_test_Rem_RoundHalfZero_N1_N1 :: Dict (K.Rem 'K.RoundHalfZero (N 1) (N 1) ~ Z)
_test_Rem_RoundHalfZero_N1_N1 =  Dict
_test_Div_RoundHalfZero_N1_N2 :: Dict (K.Div 'K.RoundHalfZero (N 1) (N 2) ~ Z)
_test_Div_RoundHalfZero_N1_N2 =  Dict
_test_Rem_RoundHalfZero_N1_N2 :: Dict (K.Rem 'K.RoundHalfZero (N 1) (N 2) ~ N 1)
_test_Rem_RoundHalfZero_N1_N2 =  Dict
_test_Div_RoundHalfZero_N1_N3 :: Dict (K.Div 'K.RoundHalfZero (N 1) (N 3) ~ Z)
_test_Div_RoundHalfZero_N1_N3 =  Dict
_test_Rem_RoundHalfZero_N1_N3 :: Dict (K.Rem 'K.RoundHalfZero (N 1) (N 3) ~ N 1)
_test_Rem_RoundHalfZero_N1_N3 =  Dict
_test_Div_RoundHalfZero_N1_N4 :: Dict (K.Div 'K.RoundHalfZero (N 1) (N 4) ~ Z)
_test_Div_RoundHalfZero_N1_N4 =  Dict
_test_Rem_RoundHalfZero_N1_N4 :: Dict (K.Rem 'K.RoundHalfZero (N 1) (N 4) ~ N 1)
_test_Rem_RoundHalfZero_N1_N4 =  Dict
_test_Div_RoundHalfZero_N1_P1 :: Dict (K.Div 'K.RoundHalfZero (N 1) (P 1) ~ N 1)
_test_Div_RoundHalfZero_N1_P1 =  Dict
_test_Rem_RoundHalfZero_N1_P1 :: Dict (K.Rem 'K.RoundHalfZero (N 1) (P 1) ~ Z)
_test_Rem_RoundHalfZero_N1_P1 =  Dict
_test_Div_RoundHalfZero_N1_P2 :: Dict (K.Div 'K.RoundHalfZero (N 1) (P 2) ~ Z)
_test_Div_RoundHalfZero_N1_P2 =  Dict
_test_Rem_RoundHalfZero_N1_P2 :: Dict (K.Rem 'K.RoundHalfZero (N 1) (P 2) ~ N 1)
_test_Rem_RoundHalfZero_N1_P2 =  Dict
_test_Div_RoundHalfZero_N1_P3 :: Dict (K.Div 'K.RoundHalfZero (N 1) (P 3) ~ Z)
_test_Div_RoundHalfZero_N1_P3 =  Dict
_test_Rem_RoundHalfZero_N1_P3 :: Dict (K.Rem 'K.RoundHalfZero (N 1) (P 3) ~ N 1)
_test_Rem_RoundHalfZero_N1_P3 =  Dict
_test_Div_RoundHalfZero_N1_P4 :: Dict (K.Div 'K.RoundHalfZero (N 1) (P 4) ~ Z)
_test_Div_RoundHalfZero_N1_P4 =  Dict
_test_Rem_RoundHalfZero_N1_P4 :: Dict (K.Rem 'K.RoundHalfZero (N 1) (P 4) ~ N 1)
_test_Rem_RoundHalfZero_N1_P4 =  Dict
_test_Div_RoundHalfZero_N2_N1 :: Dict (K.Div 'K.RoundHalfZero (N 2) (N 1) ~ P 2)
_test_Div_RoundHalfZero_N2_N1 =  Dict
_test_Rem_RoundHalfZero_N2_N1 :: Dict (K.Rem 'K.RoundHalfZero (N 2) (N 1) ~ Z)
_test_Rem_RoundHalfZero_N2_N1 =  Dict
_test_Div_RoundHalfZero_N2_N2 :: Dict (K.Div 'K.RoundHalfZero (N 2) (N 2) ~ P 1)
_test_Div_RoundHalfZero_N2_N2 =  Dict
_test_Rem_RoundHalfZero_N2_N2 :: Dict (K.Rem 'K.RoundHalfZero (N 2) (N 2) ~ Z)
_test_Rem_RoundHalfZero_N2_N2 =  Dict
_test_Div_RoundHalfZero_N2_N3 :: Dict (K.Div 'K.RoundHalfZero (N 2) (N 3) ~ P 1)
_test_Div_RoundHalfZero_N2_N3 =  Dict
_test_Rem_RoundHalfZero_N2_N3 :: Dict (K.Rem 'K.RoundHalfZero (N 2) (N 3) ~ P 1)
_test_Rem_RoundHalfZero_N2_N3 =  Dict
_test_Div_RoundHalfZero_N2_N4 :: Dict (K.Div 'K.RoundHalfZero (N 2) (N 4) ~ Z)
_test_Div_RoundHalfZero_N2_N4 =  Dict
_test_Rem_RoundHalfZero_N2_N4 :: Dict (K.Rem 'K.RoundHalfZero (N 2) (N 4) ~ N 2)
_test_Rem_RoundHalfZero_N2_N4 =  Dict
_test_Div_RoundHalfZero_N2_P1 :: Dict (K.Div 'K.RoundHalfZero (N 2) (P 1) ~ N 2)
_test_Div_RoundHalfZero_N2_P1 =  Dict
_test_Rem_RoundHalfZero_N2_P1 :: Dict (K.Rem 'K.RoundHalfZero (N 2) (P 1) ~ Z)
_test_Rem_RoundHalfZero_N2_P1 =  Dict
_test_Div_RoundHalfZero_N2_P2 :: Dict (K.Div 'K.RoundHalfZero (N 2) (P 2) ~ N 1)
_test_Div_RoundHalfZero_N2_P2 =  Dict
_test_Rem_RoundHalfZero_N2_P2 :: Dict (K.Rem 'K.RoundHalfZero (N 2) (P 2) ~ Z)
_test_Rem_RoundHalfZero_N2_P2 =  Dict
_test_Div_RoundHalfZero_N2_P3 :: Dict (K.Div 'K.RoundHalfZero (N 2) (P 3) ~ N 1)
_test_Div_RoundHalfZero_N2_P3 =  Dict
_test_Rem_RoundHalfZero_N2_P3 :: Dict (K.Rem 'K.RoundHalfZero (N 2) (P 3) ~ P 1)
_test_Rem_RoundHalfZero_N2_P3 =  Dict
_test_Div_RoundHalfZero_N2_P4 :: Dict (K.Div 'K.RoundHalfZero (N 2) (P 4) ~ Z)
_test_Div_RoundHalfZero_N2_P4 =  Dict
_test_Rem_RoundHalfZero_N2_P4 :: Dict (K.Rem 'K.RoundHalfZero (N 2) (P 4) ~ N 2)
_test_Rem_RoundHalfZero_N2_P4 =  Dict
_test_Div_RoundHalfZero_N3_N1 :: Dict (K.Div 'K.RoundHalfZero (N 3) (N 1) ~ P 3)
_test_Div_RoundHalfZero_N3_N1 =  Dict
_test_Rem_RoundHalfZero_N3_N1 :: Dict (K.Rem 'K.RoundHalfZero (N 3) (N 1) ~ Z)
_test_Rem_RoundHalfZero_N3_N1 =  Dict
_test_Div_RoundHalfZero_N3_N2 :: Dict (K.Div 'K.RoundHalfZero (N 3) (N 2) ~ P 1)
_test_Div_RoundHalfZero_N3_N2 =  Dict
_test_Rem_RoundHalfZero_N3_N2 :: Dict (K.Rem 'K.RoundHalfZero (N 3) (N 2) ~ N 1)
_test_Rem_RoundHalfZero_N3_N2 =  Dict
_test_Div_RoundHalfZero_N3_N3 :: Dict (K.Div 'K.RoundHalfZero (N 3) (N 3) ~ P 1)
_test_Div_RoundHalfZero_N3_N3 =  Dict
_test_Rem_RoundHalfZero_N3_N3 :: Dict (K.Rem 'K.RoundHalfZero (N 3) (N 3) ~ Z)
_test_Rem_RoundHalfZero_N3_N3 =  Dict
_test_Div_RoundHalfZero_N3_N4 :: Dict (K.Div 'K.RoundHalfZero (N 3) (N 4) ~ P 1)
_test_Div_RoundHalfZero_N3_N4 =  Dict
_test_Rem_RoundHalfZero_N3_N4 :: Dict (K.Rem 'K.RoundHalfZero (N 3) (N 4) ~ P 1)
_test_Rem_RoundHalfZero_N3_N4 =  Dict
_test_Div_RoundHalfZero_N3_P1 :: Dict (K.Div 'K.RoundHalfZero (N 3) (P 1) ~ N 3)
_test_Div_RoundHalfZero_N3_P1 =  Dict
_test_Rem_RoundHalfZero_N3_P1 :: Dict (K.Rem 'K.RoundHalfZero (N 3) (P 1) ~ Z)
_test_Rem_RoundHalfZero_N3_P1 =  Dict
_test_Div_RoundHalfZero_N3_P2 :: Dict (K.Div 'K.RoundHalfZero (N 3) (P 2) ~ N 1)
_test_Div_RoundHalfZero_N3_P2 =  Dict
_test_Rem_RoundHalfZero_N3_P2 :: Dict (K.Rem 'K.RoundHalfZero (N 3) (P 2) ~ N 1)
_test_Rem_RoundHalfZero_N3_P2 =  Dict
_test_Div_RoundHalfZero_N3_P3 :: Dict (K.Div 'K.RoundHalfZero (N 3) (P 3) ~ N 1)
_test_Div_RoundHalfZero_N3_P3 =  Dict
_test_Rem_RoundHalfZero_N3_P3 :: Dict (K.Rem 'K.RoundHalfZero (N 3) (P 3) ~ Z)
_test_Rem_RoundHalfZero_N3_P3 =  Dict
_test_Div_RoundHalfZero_N3_P4 :: Dict (K.Div 'K.RoundHalfZero (N 3) (P 4) ~ N 1)
_test_Div_RoundHalfZero_N3_P4 =  Dict
_test_Rem_RoundHalfZero_N3_P4 :: Dict (K.Rem 'K.RoundHalfZero (N 3) (P 4) ~ P 1)
_test_Rem_RoundHalfZero_N3_P4 =  Dict
_test_Div_RoundHalfZero_N4_N1 :: Dict (K.Div 'K.RoundHalfZero (N 4) (N 1) ~ P 4)
_test_Div_RoundHalfZero_N4_N1 =  Dict
_test_Rem_RoundHalfZero_N4_N1 :: Dict (K.Rem 'K.RoundHalfZero (N 4) (N 1) ~ Z)
_test_Rem_RoundHalfZero_N4_N1 =  Dict
_test_Div_RoundHalfZero_N4_N2 :: Dict (K.Div 'K.RoundHalfZero (N 4) (N 2) ~ P 2)
_test_Div_RoundHalfZero_N4_N2 =  Dict
_test_Rem_RoundHalfZero_N4_N2 :: Dict (K.Rem 'K.RoundHalfZero (N 4) (N 2) ~ Z)
_test_Rem_RoundHalfZero_N4_N2 =  Dict
_test_Div_RoundHalfZero_N4_N3 :: Dict (K.Div 'K.RoundHalfZero (N 4) (N 3) ~ P 1)
_test_Div_RoundHalfZero_N4_N3 =  Dict
_test_Rem_RoundHalfZero_N4_N3 :: Dict (K.Rem 'K.RoundHalfZero (N 4) (N 3) ~ N 1)
_test_Rem_RoundHalfZero_N4_N3 =  Dict
_test_Div_RoundHalfZero_N4_N4 :: Dict (K.Div 'K.RoundHalfZero (N 4) (N 4) ~ P 1)
_test_Div_RoundHalfZero_N4_N4 =  Dict
_test_Rem_RoundHalfZero_N4_N4 :: Dict (K.Rem 'K.RoundHalfZero (N 4) (N 4) ~ Z)
_test_Rem_RoundHalfZero_N4_N4 =  Dict
_test_Div_RoundHalfZero_N4_P1 :: Dict (K.Div 'K.RoundHalfZero (N 4) (P 1) ~ N 4)
_test_Div_RoundHalfZero_N4_P1 =  Dict
_test_Rem_RoundHalfZero_N4_P1 :: Dict (K.Rem 'K.RoundHalfZero (N 4) (P 1) ~ Z)
_test_Rem_RoundHalfZero_N4_P1 =  Dict
_test_Div_RoundHalfZero_N4_P2 :: Dict (K.Div 'K.RoundHalfZero (N 4) (P 2) ~ N 2)
_test_Div_RoundHalfZero_N4_P2 =  Dict
_test_Rem_RoundHalfZero_N4_P2 :: Dict (K.Rem 'K.RoundHalfZero (N 4) (P 2) ~ Z)
_test_Rem_RoundHalfZero_N4_P2 =  Dict
_test_Div_RoundHalfZero_N4_P3 :: Dict (K.Div 'K.RoundHalfZero (N 4) (P 3) ~ N 1)
_test_Div_RoundHalfZero_N4_P3 =  Dict
_test_Rem_RoundHalfZero_N4_P3 :: Dict (K.Rem 'K.RoundHalfZero (N 4) (P 3) ~ N 1)
_test_Rem_RoundHalfZero_N4_P3 =  Dict
_test_Div_RoundHalfZero_N4_P4 :: Dict (K.Div 'K.RoundHalfZero (N 4) (P 4) ~ N 1)
_test_Div_RoundHalfZero_N4_P4 =  Dict
_test_Rem_RoundHalfZero_N4_P4 :: Dict (K.Rem 'K.RoundHalfZero (N 4) (P 4) ~ Z)
_test_Rem_RoundHalfZero_N4_P4 =  Dict
_test_Rem_RoundHalfZero_Z_N1 :: Dict (K.Rem 'K.RoundHalfZero Z (N 1) ~ Z)
_test_Rem_RoundHalfZero_Z_N1 =  Dict
_test_Rem_RoundHalfZero_Z_N2 :: Dict (K.Rem 'K.RoundHalfZero Z (N 2) ~ Z)
_test_Rem_RoundHalfZero_Z_N2 =  Dict
_test_Rem_RoundHalfZero_Z_N3 :: Dict (K.Rem 'K.RoundHalfZero Z (N 3) ~ Z)
_test_Rem_RoundHalfZero_Z_N3 =  Dict
_test_Div_RoundHalfZero_Z_N4 :: Dict (K.Div 'K.RoundHalfZero Z (N 4) ~ Z)
_test_Div_RoundHalfZero_Z_N4 =  Dict
_test_Rem_RoundHalfZero_Z_P1 :: Dict (K.Rem 'K.RoundHalfZero Z (P 1) ~ Z)
_test_Rem_RoundHalfZero_Z_P1 =  Dict
_test_Div_RoundHalfZero_Z_P2 :: Dict (K.Div 'K.RoundHalfZero Z (P 2) ~ Z)
_test_Div_RoundHalfZero_Z_P2 =  Dict
_test_Rem_RoundHalfZero_Z_P3 :: Dict (K.Rem 'K.RoundHalfZero Z (P 3) ~ Z)
_test_Rem_RoundHalfZero_Z_P3 =  Dict
_test_Div_RoundHalfZero_Z_P4 :: Dict (K.Div 'K.RoundHalfZero Z (P 4) ~ Z)
_test_Div_RoundHalfZero_Z_P4 =  Dict
_test_Div_RoundHalfZero_P1_N1 :: Dict (K.Div 'K.RoundHalfZero (P 1) (N 1) ~ N 1)
_test_Div_RoundHalfZero_P1_N1 =  Dict
_test_Rem_RoundHalfZero_P1_N1 :: Dict (K.Rem 'K.RoundHalfZero (P 1) (N 1) ~ Z)
_test_Rem_RoundHalfZero_P1_N1 =  Dict
_test_Div_RoundHalfZero_P1_N2 :: Dict (K.Div 'K.RoundHalfZero (P 1) (N 2) ~ Z)
_test_Div_RoundHalfZero_P1_N2 =  Dict
_test_Rem_RoundHalfZero_P1_N2 :: Dict (K.Rem 'K.RoundHalfZero (P 1) (N 2) ~ P 1)
_test_Rem_RoundHalfZero_P1_N2 =  Dict
_test_Div_RoundHalfZero_P1_N3 :: Dict (K.Div 'K.RoundHalfZero (P 1) (N 3) ~ Z)
_test_Div_RoundHalfZero_P1_N3 =  Dict
_test_Rem_RoundHalfZero_P1_N3 :: Dict (K.Rem 'K.RoundHalfZero (P 1) (N 3) ~ P 1)
_test_Rem_RoundHalfZero_P1_N3 =  Dict
_test_Div_RoundHalfZero_P1_N4 :: Dict (K.Div 'K.RoundHalfZero (P 1) (N 4) ~ Z)
_test_Div_RoundHalfZero_P1_N4 =  Dict
_test_Rem_RoundHalfZero_P1_N4 :: Dict (K.Rem 'K.RoundHalfZero (P 1) (N 4) ~ P 1)
_test_Rem_RoundHalfZero_P1_N4 =  Dict
_test_Div_RoundHalfZero_P1_P1 :: Dict (K.Div 'K.RoundHalfZero (P 1) (P 1) ~ P 1)
_test_Div_RoundHalfZero_P1_P1 =  Dict
_test_Rem_RoundHalfZero_P1_P1 :: Dict (K.Rem 'K.RoundHalfZero (P 1) (P 1) ~ Z)
_test_Rem_RoundHalfZero_P1_P1 =  Dict
_test_Div_RoundHalfZero_P1_P2 :: Dict (K.Div 'K.RoundHalfZero (P 1) (P 2) ~ Z)
_test_Div_RoundHalfZero_P1_P2 =  Dict
_test_Rem_RoundHalfZero_P1_P2 :: Dict (K.Rem 'K.RoundHalfZero (P 1) (P 2) ~ P 1)
_test_Rem_RoundHalfZero_P1_P2 =  Dict
_test_Div_RoundHalfZero_P1_P3 :: Dict (K.Div 'K.RoundHalfZero (P 1) (P 3) ~ Z)
_test_Div_RoundHalfZero_P1_P3 =  Dict
_test_Rem_RoundHalfZero_P1_P3 :: Dict (K.Rem 'K.RoundHalfZero (P 1) (P 3) ~ P 1)
_test_Rem_RoundHalfZero_P1_P3 =  Dict
_test_Div_RoundHalfZero_P1_P4 :: Dict (K.Div 'K.RoundHalfZero (P 1) (P 4) ~ Z)
_test_Div_RoundHalfZero_P1_P4 =  Dict
_test_Rem_RoundHalfZero_P1_P4 :: Dict (K.Rem 'K.RoundHalfZero (P 1) (P 4) ~ P 1)
_test_Rem_RoundHalfZero_P1_P4 =  Dict
_test_Div_RoundHalfZero_P2_N1 :: Dict (K.Div 'K.RoundHalfZero (P 2) (N 1) ~ N 2)
_test_Div_RoundHalfZero_P2_N1 =  Dict
_test_Rem_RoundHalfZero_P2_N1 :: Dict (K.Rem 'K.RoundHalfZero (P 2) (N 1) ~ Z)
_test_Rem_RoundHalfZero_P2_N1 =  Dict
_test_Div_RoundHalfZero_P2_N2 :: Dict (K.Div 'K.RoundHalfZero (P 2) (N 2) ~ N 1)
_test_Div_RoundHalfZero_P2_N2 =  Dict
_test_Rem_RoundHalfZero_P2_N2 :: Dict (K.Rem 'K.RoundHalfZero (P 2) (N 2) ~ Z)
_test_Rem_RoundHalfZero_P2_N2 =  Dict
_test_Div_RoundHalfZero_P2_N3 :: Dict (K.Div 'K.RoundHalfZero (P 2) (N 3) ~ N 1)
_test_Div_RoundHalfZero_P2_N3 =  Dict
_test_Rem_RoundHalfZero_P2_N3 :: Dict (K.Rem 'K.RoundHalfZero (P 2) (N 3) ~ N 1)
_test_Rem_RoundHalfZero_P2_N3 =  Dict
_test_Div_RoundHalfZero_P2_N4 :: Dict (K.Div 'K.RoundHalfZero (P 2) (N 4) ~ Z)
_test_Div_RoundHalfZero_P2_N4 =  Dict
_test_Rem_RoundHalfZero_P2_N4 :: Dict (K.Rem 'K.RoundHalfZero (P 2) (N 4) ~ P 2)
_test_Rem_RoundHalfZero_P2_N4 =  Dict
_test_Div_RoundHalfZero_P2_P1 :: Dict (K.Div 'K.RoundHalfZero (P 2) (P 1) ~ P 2)
_test_Div_RoundHalfZero_P2_P1 =  Dict
_test_Rem_RoundHalfZero_P2_P1 :: Dict (K.Rem 'K.RoundHalfZero (P 2) (P 1) ~ Z)
_test_Rem_RoundHalfZero_P2_P1 =  Dict
_test_Div_RoundHalfZero_P2_P2 :: Dict (K.Div 'K.RoundHalfZero (P 2) (P 2) ~ P 1)
_test_Div_RoundHalfZero_P2_P2 =  Dict
_test_Rem_RoundHalfZero_P2_P2 :: Dict (K.Rem 'K.RoundHalfZero (P 2) (P 2) ~ Z)
_test_Rem_RoundHalfZero_P2_P2 =  Dict
_test_Div_RoundHalfZero_P2_P3 :: Dict (K.Div 'K.RoundHalfZero (P 2) (P 3) ~ P 1)
_test_Div_RoundHalfZero_P2_P3 =  Dict
_test_Rem_RoundHalfZero_P2_P3 :: Dict (K.Rem 'K.RoundHalfZero (P 2) (P 3) ~ N 1)
_test_Rem_RoundHalfZero_P2_P3 =  Dict
_test_Div_RoundHalfZero_P2_P4 :: Dict (K.Div 'K.RoundHalfZero (P 2) (P 4) ~ Z)
_test_Div_RoundHalfZero_P2_P4 =  Dict
_test_Rem_RoundHalfZero_P2_P4 :: Dict (K.Rem 'K.RoundHalfZero (P 2) (P 4) ~ P 2)
_test_Rem_RoundHalfZero_P2_P4 =  Dict
_test_Div_RoundHalfZero_P3_N1 :: Dict (K.Div 'K.RoundHalfZero (P 3) (N 1) ~ N 3)
_test_Div_RoundHalfZero_P3_N1 =  Dict
_test_Rem_RoundHalfZero_P3_N1 :: Dict (K.Rem 'K.RoundHalfZero (P 3) (N 1) ~ Z)
_test_Rem_RoundHalfZero_P3_N1 =  Dict
_test_Div_RoundHalfZero_P3_N2 :: Dict (K.Div 'K.RoundHalfZero (P 3) (N 2) ~ N 1)
_test_Div_RoundHalfZero_P3_N2 =  Dict
_test_Rem_RoundHalfZero_P3_N2 :: Dict (K.Rem 'K.RoundHalfZero (P 3) (N 2) ~ P 1)
_test_Rem_RoundHalfZero_P3_N2 =  Dict
_test_Div_RoundHalfZero_P3_N3 :: Dict (K.Div 'K.RoundHalfZero (P 3) (N 3) ~ N 1)
_test_Div_RoundHalfZero_P3_N3 =  Dict
_test_Rem_RoundHalfZero_P3_N3 :: Dict (K.Rem 'K.RoundHalfZero (P 3) (N 3) ~ Z)
_test_Rem_RoundHalfZero_P3_N3 =  Dict
_test_Div_RoundHalfZero_P3_N4 :: Dict (K.Div 'K.RoundHalfZero (P 3) (N 4) ~ N 1)
_test_Div_RoundHalfZero_P3_N4 =  Dict
_test_Rem_RoundHalfZero_P3_N4 :: Dict (K.Rem 'K.RoundHalfZero (P 3) (N 4) ~ N 1)
_test_Rem_RoundHalfZero_P3_N4 =  Dict
_test_Div_RoundHalfZero_P3_P1 :: Dict (K.Div 'K.RoundHalfZero (P 3) (P 1) ~ P 3)
_test_Div_RoundHalfZero_P3_P1 =  Dict
_test_Rem_RoundHalfZero_P3_P1 :: Dict (K.Rem 'K.RoundHalfZero (P 3) (P 1) ~ Z)
_test_Rem_RoundHalfZero_P3_P1 =  Dict
_test_Div_RoundHalfZero_P3_P2 :: Dict (K.Div 'K.RoundHalfZero (P 3) (P 2) ~ P 1)
_test_Div_RoundHalfZero_P3_P2 =  Dict
_test_Rem_RoundHalfZero_P3_P2 :: Dict (K.Rem 'K.RoundHalfZero (P 3) (P 2) ~ P 1)
_test_Rem_RoundHalfZero_P3_P2 =  Dict
_test_Div_RoundHalfZero_P3_P3 :: Dict (K.Div 'K.RoundHalfZero (P 3) (P 3) ~ P 1)
_test_Div_RoundHalfZero_P3_P3 =  Dict
_test_Rem_RoundHalfZero_P3_P3 :: Dict (K.Rem 'K.RoundHalfZero (P 3) (P 3) ~ Z)
_test_Rem_RoundHalfZero_P3_P3 =  Dict
_test_Div_RoundHalfZero_P3_P4 :: Dict (K.Div 'K.RoundHalfZero (P 3) (P 4) ~ P 1)
_test_Div_RoundHalfZero_P3_P4 =  Dict
_test_Rem_RoundHalfZero_P3_P4 :: Dict (K.Rem 'K.RoundHalfZero (P 3) (P 4) ~ N 1)
_test_Rem_RoundHalfZero_P3_P4 =  Dict
_test_Div_RoundHalfZero_P4_N1 :: Dict (K.Div 'K.RoundHalfZero (P 4) (N 1) ~ N 4)
_test_Div_RoundHalfZero_P4_N1 =  Dict
_test_Rem_RoundHalfZero_P4_N1 :: Dict (K.Rem 'K.RoundHalfZero (P 4) (N 1) ~ Z)
_test_Rem_RoundHalfZero_P4_N1 =  Dict
_test_Div_RoundHalfZero_P4_N2 :: Dict (K.Div 'K.RoundHalfZero (P 4) (N 2) ~ N 2)
_test_Div_RoundHalfZero_P4_N2 =  Dict
_test_Rem_RoundHalfZero_P4_N2 :: Dict (K.Rem 'K.RoundHalfZero (P 4) (N 2) ~ Z)
_test_Rem_RoundHalfZero_P4_N2 =  Dict
_test_Div_RoundHalfZero_P4_N3 :: Dict (K.Div 'K.RoundHalfZero (P 4) (N 3) ~ N 1)
_test_Div_RoundHalfZero_P4_N3 =  Dict
_test_Rem_RoundHalfZero_P4_N3 :: Dict (K.Rem 'K.RoundHalfZero (P 4) (N 3) ~ P 1)
_test_Rem_RoundHalfZero_P4_N3 =  Dict
_test_Div_RoundHalfZero_P4_N4 :: Dict (K.Div 'K.RoundHalfZero (P 4) (N 4) ~ N 1)
_test_Div_RoundHalfZero_P4_N4 =  Dict
_test_Rem_RoundHalfZero_P4_N4 :: Dict (K.Rem 'K.RoundHalfZero (P 4) (N 4) ~ Z)
_test_Rem_RoundHalfZero_P4_N4 =  Dict
_test_Div_RoundHalfZero_P4_P1 :: Dict (K.Div 'K.RoundHalfZero (P 4) (P 1) ~ P 4)
_test_Div_RoundHalfZero_P4_P1 =  Dict
_test_Rem_RoundHalfZero_P4_P1 :: Dict (K.Rem 'K.RoundHalfZero (P 4) (P 1) ~ Z)
_test_Rem_RoundHalfZero_P4_P1 =  Dict
_test_Div_RoundHalfZero_P4_P2 :: Dict (K.Div 'K.RoundHalfZero (P 4) (P 2) ~ P 2)
_test_Div_RoundHalfZero_P4_P2 =  Dict
_test_Rem_RoundHalfZero_P4_P2 :: Dict (K.Rem 'K.RoundHalfZero (P 4) (P 2) ~ Z)
_test_Rem_RoundHalfZero_P4_P2 =  Dict
_test_Div_RoundHalfZero_P4_P3 :: Dict (K.Div 'K.RoundHalfZero (P 4) (P 3) ~ P 1)
_test_Div_RoundHalfZero_P4_P3 =  Dict
_test_Rem_RoundHalfZero_P4_P3 :: Dict (K.Rem 'K.RoundHalfZero (P 4) (P 3) ~ P 1)
_test_Rem_RoundHalfZero_P4_P3 =  Dict
_test_Div_RoundHalfZero_P4_P4 :: Dict (K.Div 'K.RoundHalfZero (P 4) (P 4) ~ P 1)
_test_Div_RoundHalfZero_P4_P4 =  Dict
_test_Rem_RoundHalfZero_P4_P4 :: Dict (K.Rem 'K.RoundHalfZero (P 4) (P 4) ~ Z)
_test_Rem_RoundHalfZero_P4_P4 =  Dict
_test_Div_RoundUp_N1_N1 :: Dict (K.Div 'K.RoundUp (N 1) (N 1) ~ P 1)
_test_Div_RoundUp_N1_N1 =  Dict
_test_Rem_RoundUp_N1_N1 :: Dict (K.Rem 'K.RoundUp (N 1) (N 1) ~ Z)
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
_test_Rem_RoundUp_N1_P1 :: Dict (K.Rem 'K.RoundUp (N 1) (P 1) ~ Z)
_test_Rem_RoundUp_N1_P1 =  Dict
_test_Div_RoundUp_N1_P2 :: Dict (K.Div 'K.RoundUp (N 1) (P 2) ~ Z)
_test_Div_RoundUp_N1_P2 =  Dict
_test_Rem_RoundUp_N1_P2 :: Dict (K.Rem 'K.RoundUp (N 1) (P 2) ~ N 1)
_test_Rem_RoundUp_N1_P2 =  Dict
_test_Div_RoundUp_N1_P3 :: Dict (K.Div 'K.RoundUp (N 1) (P 3) ~ Z)
_test_Div_RoundUp_N1_P3 =  Dict
_test_Rem_RoundUp_N1_P3 :: Dict (K.Rem 'K.RoundUp (N 1) (P 3) ~ N 1)
_test_Rem_RoundUp_N1_P3 =  Dict
_test_Div_RoundUp_N1_P4 :: Dict (K.Div 'K.RoundUp (N 1) (P 4) ~ Z)
_test_Div_RoundUp_N1_P4 =  Dict
_test_Rem_RoundUp_N1_P4 :: Dict (K.Rem 'K.RoundUp (N 1) (P 4) ~ N 1)
_test_Rem_RoundUp_N1_P4 =  Dict
_test_Div_RoundUp_N2_N1 :: Dict (K.Div 'K.RoundUp (N 2) (N 1) ~ P 2)
_test_Div_RoundUp_N2_N1 =  Dict
_test_Rem_RoundUp_N2_N1 :: Dict (K.Rem 'K.RoundUp (N 2) (N 1) ~ Z)
_test_Rem_RoundUp_N2_N1 =  Dict
_test_Div_RoundUp_N2_N2 :: Dict (K.Div 'K.RoundUp (N 2) (N 2) ~ P 1)
_test_Div_RoundUp_N2_N2 =  Dict
_test_Rem_RoundUp_N2_N2 :: Dict (K.Rem 'K.RoundUp (N 2) (N 2) ~ Z)
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
_test_Rem_RoundUp_N2_P1 :: Dict (K.Rem 'K.RoundUp (N 2) (P 1) ~ Z)
_test_Rem_RoundUp_N2_P1 =  Dict
_test_Div_RoundUp_N2_P2 :: Dict (K.Div 'K.RoundUp (N 2) (P 2) ~ N 1)
_test_Div_RoundUp_N2_P2 =  Dict
_test_Rem_RoundUp_N2_P2 :: Dict (K.Rem 'K.RoundUp (N 2) (P 2) ~ Z)
_test_Rem_RoundUp_N2_P2 =  Dict
_test_Div_RoundUp_N2_P3 :: Dict (K.Div 'K.RoundUp (N 2) (P 3) ~ Z)
_test_Div_RoundUp_N2_P3 =  Dict
_test_Rem_RoundUp_N2_P3 :: Dict (K.Rem 'K.RoundUp (N 2) (P 3) ~ N 2)
_test_Rem_RoundUp_N2_P3 =  Dict
_test_Div_RoundUp_N2_P4 :: Dict (K.Div 'K.RoundUp (N 2) (P 4) ~ Z)
_test_Div_RoundUp_N2_P4 =  Dict
_test_Rem_RoundUp_N2_P4 :: Dict (K.Rem 'K.RoundUp (N 2) (P 4) ~ N 2)
_test_Rem_RoundUp_N2_P4 =  Dict
_test_Div_RoundUp_N3_N1 :: Dict (K.Div 'K.RoundUp (N 3) (N 1) ~ P 3)
_test_Div_RoundUp_N3_N1 =  Dict
_test_Rem_RoundUp_N3_N1 :: Dict (K.Rem 'K.RoundUp (N 3) (N 1) ~ Z)
_test_Rem_RoundUp_N3_N1 =  Dict
_test_Div_RoundUp_N3_N2 :: Dict (K.Div 'K.RoundUp (N 3) (N 2) ~ P 2)
_test_Div_RoundUp_N3_N2 =  Dict
_test_Rem_RoundUp_N3_N2 :: Dict (K.Rem 'K.RoundUp (N 3) (N 2) ~ P 1)
_test_Rem_RoundUp_N3_N2 =  Dict
_test_Div_RoundUp_N3_N3 :: Dict (K.Div 'K.RoundUp (N 3) (N 3) ~ P 1)
_test_Div_RoundUp_N3_N3 =  Dict
_test_Rem_RoundUp_N3_N3 :: Dict (K.Rem 'K.RoundUp (N 3) (N 3) ~ Z)
_test_Rem_RoundUp_N3_N3 =  Dict
_test_Div_RoundUp_N3_N4 :: Dict (K.Div 'K.RoundUp (N 3) (N 4) ~ P 1)
_test_Div_RoundUp_N3_N4 =  Dict
_test_Rem_RoundUp_N3_N4 :: Dict (K.Rem 'K.RoundUp (N 3) (N 4) ~ P 1)
_test_Rem_RoundUp_N3_N4 =  Dict
_test_Div_RoundUp_N3_P1 :: Dict (K.Div 'K.RoundUp (N 3) (P 1) ~ N 3)
_test_Div_RoundUp_N3_P1 =  Dict
_test_Rem_RoundUp_N3_P1 :: Dict (K.Rem 'K.RoundUp (N 3) (P 1) ~ Z)
_test_Rem_RoundUp_N3_P1 =  Dict
_test_Div_RoundUp_N3_P2 :: Dict (K.Div 'K.RoundUp (N 3) (P 2) ~ N 1)
_test_Div_RoundUp_N3_P2 =  Dict
_test_Rem_RoundUp_N3_P2 :: Dict (K.Rem 'K.RoundUp (N 3) (P 2) ~ N 1)
_test_Rem_RoundUp_N3_P2 =  Dict
_test_Div_RoundUp_N3_P3 :: Dict (K.Div 'K.RoundUp (N 3) (P 3) ~ N 1)
_test_Div_RoundUp_N3_P3 =  Dict
_test_Rem_RoundUp_N3_P3 :: Dict (K.Rem 'K.RoundUp (N 3) (P 3) ~ Z)
_test_Rem_RoundUp_N3_P3 =  Dict
_test_Div_RoundUp_N3_P4 :: Dict (K.Div 'K.RoundUp (N 3) (P 4) ~ Z)
_test_Div_RoundUp_N3_P4 =  Dict
_test_Rem_RoundUp_N3_P4 :: Dict (K.Rem 'K.RoundUp (N 3) (P 4) ~ N 3)
_test_Rem_RoundUp_N3_P4 =  Dict
_test_Div_RoundUp_N4_N1 :: Dict (K.Div 'K.RoundUp (N 4) (N 1) ~ P 4)
_test_Div_RoundUp_N4_N1 =  Dict
_test_Rem_RoundUp_N4_N1 :: Dict (K.Rem 'K.RoundUp (N 4) (N 1) ~ Z)
_test_Rem_RoundUp_N4_N1 =  Dict
_test_Div_RoundUp_N4_N2 :: Dict (K.Div 'K.RoundUp (N 4) (N 2) ~ P 2)
_test_Div_RoundUp_N4_N2 =  Dict
_test_Rem_RoundUp_N4_N2 :: Dict (K.Rem 'K.RoundUp (N 4) (N 2) ~ Z)
_test_Rem_RoundUp_N4_N2 =  Dict
_test_Div_RoundUp_N4_N3 :: Dict (K.Div 'K.RoundUp (N 4) (N 3) ~ P 2)
_test_Div_RoundUp_N4_N3 =  Dict
_test_Rem_RoundUp_N4_N3 :: Dict (K.Rem 'K.RoundUp (N 4) (N 3) ~ P 2)
_test_Rem_RoundUp_N4_N3 =  Dict
_test_Div_RoundUp_N4_N4 :: Dict (K.Div 'K.RoundUp (N 4) (N 4) ~ P 1)
_test_Div_RoundUp_N4_N4 =  Dict
_test_Rem_RoundUp_N4_N4 :: Dict (K.Rem 'K.RoundUp (N 4) (N 4) ~ Z)
_test_Rem_RoundUp_N4_N4 =  Dict
_test_Div_RoundUp_N4_P1 :: Dict (K.Div 'K.RoundUp (N 4) (P 1) ~ N 4)
_test_Div_RoundUp_N4_P1 =  Dict
_test_Rem_RoundUp_N4_P1 :: Dict (K.Rem 'K.RoundUp (N 4) (P 1) ~ Z)
_test_Rem_RoundUp_N4_P1 =  Dict
_test_Div_RoundUp_N4_P2 :: Dict (K.Div 'K.RoundUp (N 4) (P 2) ~ N 2)
_test_Div_RoundUp_N4_P2 =  Dict
_test_Rem_RoundUp_N4_P2 :: Dict (K.Rem 'K.RoundUp (N 4) (P 2) ~ Z)
_test_Rem_RoundUp_N4_P2 =  Dict
_test_Div_RoundUp_N4_P3 :: Dict (K.Div 'K.RoundUp (N 4) (P 3) ~ N 1)
_test_Div_RoundUp_N4_P3 =  Dict
_test_Rem_RoundUp_N4_P3 :: Dict (K.Rem 'K.RoundUp (N 4) (P 3) ~ N 1)
_test_Rem_RoundUp_N4_P3 =  Dict
_test_Div_RoundUp_N4_P4 :: Dict (K.Div 'K.RoundUp (N 4) (P 4) ~ N 1)
_test_Div_RoundUp_N4_P4 =  Dict
_test_Rem_RoundUp_N4_P4 :: Dict (K.Rem 'K.RoundUp (N 4) (P 4) ~ Z)
_test_Rem_RoundUp_N4_P4 =  Dict
_test_Div_RoundUp_Z_N1 :: Dict (K.Div 'K.RoundUp Z (N 1) ~ Z)
_test_Div_RoundUp_Z_N1 =  Dict
_test_Rem_RoundUp_Z_N2 :: Dict (K.Rem 'K.RoundUp Z (N 2) ~ Z)
_test_Rem_RoundUp_Z_N2 =  Dict
_test_Div_RoundUp_Z_N3 :: Dict (K.Div 'K.RoundUp Z (N 3) ~ Z)
_test_Div_RoundUp_Z_N3 =  Dict
_test_Rem_RoundUp_Z_N4 :: Dict (K.Rem 'K.RoundUp Z (N 4) ~ Z)
_test_Rem_RoundUp_Z_N4 =  Dict
_test_Div_RoundUp_Z_P1 :: Dict (K.Div 'K.RoundUp Z (P 1) ~ Z)
_test_Div_RoundUp_Z_P1 =  Dict
_test_Rem_RoundUp_Z_P2 :: Dict (K.Rem 'K.RoundUp Z (P 2) ~ Z)
_test_Rem_RoundUp_Z_P2 =  Dict
_test_Div_RoundUp_Z_P3 :: Dict (K.Div 'K.RoundUp Z (P 3) ~ Z)
_test_Div_RoundUp_Z_P3 =  Dict
_test_Rem_RoundUp_Z_P4 :: Dict (K.Rem 'K.RoundUp Z (P 4) ~ Z)
_test_Rem_RoundUp_Z_P4 =  Dict
_test_Div_RoundUp_P1_N1 :: Dict (K.Div 'K.RoundUp (P 1) (N 1) ~ N 1)
_test_Div_RoundUp_P1_N1 =  Dict
_test_Rem_RoundUp_P1_N1 :: Dict (K.Rem 'K.RoundUp (P 1) (N 1) ~ Z)
_test_Rem_RoundUp_P1_N1 =  Dict
_test_Div_RoundUp_P1_N2 :: Dict (K.Div 'K.RoundUp (P 1) (N 2) ~ Z)
_test_Div_RoundUp_P1_N2 =  Dict
_test_Rem_RoundUp_P1_N2 :: Dict (K.Rem 'K.RoundUp (P 1) (N 2) ~ P 1)
_test_Rem_RoundUp_P1_N2 =  Dict
_test_Div_RoundUp_P1_N3 :: Dict (K.Div 'K.RoundUp (P 1) (N 3) ~ Z)
_test_Div_RoundUp_P1_N3 =  Dict
_test_Rem_RoundUp_P1_N3 :: Dict (K.Rem 'K.RoundUp (P 1) (N 3) ~ P 1)
_test_Rem_RoundUp_P1_N3 =  Dict
_test_Div_RoundUp_P1_N4 :: Dict (K.Div 'K.RoundUp (P 1) (N 4) ~ Z)
_test_Div_RoundUp_P1_N4 =  Dict
_test_Rem_RoundUp_P1_N4 :: Dict (K.Rem 'K.RoundUp (P 1) (N 4) ~ P 1)
_test_Rem_RoundUp_P1_N4 =  Dict
_test_Div_RoundUp_P1_P1 :: Dict (K.Div 'K.RoundUp (P 1) (P 1) ~ P 1)
_test_Div_RoundUp_P1_P1 =  Dict
_test_Rem_RoundUp_P1_P1 :: Dict (K.Rem 'K.RoundUp (P 1) (P 1) ~ Z)
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
_test_Rem_RoundUp_P2_N1 :: Dict (K.Rem 'K.RoundUp (P 2) (N 1) ~ Z)
_test_Rem_RoundUp_P2_N1 =  Dict
_test_Div_RoundUp_P2_N2 :: Dict (K.Div 'K.RoundUp (P 2) (N 2) ~ N 1)
_test_Div_RoundUp_P2_N2 =  Dict
_test_Rem_RoundUp_P2_N2 :: Dict (K.Rem 'K.RoundUp (P 2) (N 2) ~ Z)
_test_Rem_RoundUp_P2_N2 =  Dict
_test_Div_RoundUp_P2_N3 :: Dict (K.Div 'K.RoundUp (P 2) (N 3) ~ Z)
_test_Div_RoundUp_P2_N3 =  Dict
_test_Rem_RoundUp_P2_N3 :: Dict (K.Rem 'K.RoundUp (P 2) (N 3) ~ P 2)
_test_Rem_RoundUp_P2_N3 =  Dict
_test_Div_RoundUp_P2_N4 :: Dict (K.Div 'K.RoundUp (P 2) (N 4) ~ Z)
_test_Div_RoundUp_P2_N4 =  Dict
_test_Rem_RoundUp_P2_N4 :: Dict (K.Rem 'K.RoundUp (P 2) (N 4) ~ P 2)
_test_Rem_RoundUp_P2_N4 =  Dict
_test_Div_RoundUp_P2_P1 :: Dict (K.Div 'K.RoundUp (P 2) (P 1) ~ P 2)
_test_Div_RoundUp_P2_P1 =  Dict
_test_Rem_RoundUp_P2_P1 :: Dict (K.Rem 'K.RoundUp (P 2) (P 1) ~ Z)
_test_Rem_RoundUp_P2_P1 =  Dict
_test_Div_RoundUp_P2_P2 :: Dict (K.Div 'K.RoundUp (P 2) (P 2) ~ P 1)
_test_Div_RoundUp_P2_P2 =  Dict
_test_Rem_RoundUp_P2_P2 :: Dict (K.Rem 'K.RoundUp (P 2) (P 2) ~ Z)
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
_test_Rem_RoundUp_P3_N1 :: Dict (K.Rem 'K.RoundUp (P 3) (N 1) ~ Z)
_test_Rem_RoundUp_P3_N1 =  Dict
_test_Div_RoundUp_P3_N2 :: Dict (K.Div 'K.RoundUp (P 3) (N 2) ~ N 1)
_test_Div_RoundUp_P3_N2 =  Dict
_test_Rem_RoundUp_P3_N2 :: Dict (K.Rem 'K.RoundUp (P 3) (N 2) ~ P 1)
_test_Rem_RoundUp_P3_N2 =  Dict
_test_Div_RoundUp_P3_N3 :: Dict (K.Div 'K.RoundUp (P 3) (N 3) ~ N 1)
_test_Div_RoundUp_P3_N3 =  Dict
_test_Rem_RoundUp_P3_N3 :: Dict (K.Rem 'K.RoundUp (P 3) (N 3) ~ Z)
_test_Rem_RoundUp_P3_N3 =  Dict
_test_Div_RoundUp_P3_N4 :: Dict (K.Div 'K.RoundUp (P 3) (N 4) ~ Z)
_test_Div_RoundUp_P3_N4 =  Dict
_test_Rem_RoundUp_P3_N4 :: Dict (K.Rem 'K.RoundUp (P 3) (N 4) ~ P 3)
_test_Rem_RoundUp_P3_N4 =  Dict
_test_Div_RoundUp_P3_P1 :: Dict (K.Div 'K.RoundUp (P 3) (P 1) ~ P 3)
_test_Div_RoundUp_P3_P1 =  Dict
_test_Rem_RoundUp_P3_P1 :: Dict (K.Rem 'K.RoundUp (P 3) (P 1) ~ Z)
_test_Rem_RoundUp_P3_P1 =  Dict
_test_Div_RoundUp_P3_P2 :: Dict (K.Div 'K.RoundUp (P 3) (P 2) ~ P 2)
_test_Div_RoundUp_P3_P2 =  Dict
_test_Rem_RoundUp_P3_P2 :: Dict (K.Rem 'K.RoundUp (P 3) (P 2) ~ N 1)
_test_Rem_RoundUp_P3_P2 =  Dict
_test_Div_RoundUp_P3_P3 :: Dict (K.Div 'K.RoundUp (P 3) (P 3) ~ P 1)
_test_Div_RoundUp_P3_P3 =  Dict
_test_Rem_RoundUp_P3_P3 :: Dict (K.Rem 'K.RoundUp (P 3) (P 3) ~ Z)
_test_Rem_RoundUp_P3_P3 =  Dict
_test_Div_RoundUp_P3_P4 :: Dict (K.Div 'K.RoundUp (P 3) (P 4) ~ P 1)
_test_Div_RoundUp_P3_P4 =  Dict
_test_Rem_RoundUp_P3_P4 :: Dict (K.Rem 'K.RoundUp (P 3) (P 4) ~ N 1)
_test_Rem_RoundUp_P3_P4 =  Dict
_test_Div_RoundUp_P4_N1 :: Dict (K.Div 'K.RoundUp (P 4) (N 1) ~ N 4)
_test_Div_RoundUp_P4_N1 =  Dict
_test_Rem_RoundUp_P4_N1 :: Dict (K.Rem 'K.RoundUp (P 4) (N 1) ~ Z)
_test_Rem_RoundUp_P4_N1 =  Dict
_test_Div_RoundUp_P4_N2 :: Dict (K.Div 'K.RoundUp (P 4) (N 2) ~ N 2)
_test_Div_RoundUp_P4_N2 =  Dict
_test_Rem_RoundUp_P4_N2 :: Dict (K.Rem 'K.RoundUp (P 4) (N 2) ~ Z)
_test_Rem_RoundUp_P4_N2 =  Dict
_test_Div_RoundUp_P4_N3 :: Dict (K.Div 'K.RoundUp (P 4) (N 3) ~ N 1)
_test_Div_RoundUp_P4_N3 =  Dict
_test_Rem_RoundUp_P4_N3 :: Dict (K.Rem 'K.RoundUp (P 4) (N 3) ~ P 1)
_test_Rem_RoundUp_P4_N3 =  Dict
_test_Div_RoundUp_P4_N4 :: Dict (K.Div 'K.RoundUp (P 4) (N 4) ~ N 1)
_test_Div_RoundUp_P4_N4 =  Dict
_test_Rem_RoundUp_P4_N4 :: Dict (K.Rem 'K.RoundUp (P 4) (N 4) ~ Z)
_test_Rem_RoundUp_P4_N4 =  Dict
_test_Div_RoundUp_P4_P1 :: Dict (K.Div 'K.RoundUp (P 4) (P 1) ~ P 4)
_test_Div_RoundUp_P4_P1 =  Dict
_test_Rem_RoundUp_P4_P1 :: Dict (K.Rem 'K.RoundUp (P 4) (P 1) ~ Z)
_test_Rem_RoundUp_P4_P1 =  Dict
_test_Div_RoundUp_P4_P2 :: Dict (K.Div 'K.RoundUp (P 4) (P 2) ~ P 2)
_test_Div_RoundUp_P4_P2 =  Dict
_test_Rem_RoundUp_P4_P2 :: Dict (K.Rem 'K.RoundUp (P 4) (P 2) ~ Z)
_test_Rem_RoundUp_P4_P2 =  Dict
_test_Div_RoundUp_P4_P3 :: Dict (K.Div 'K.RoundUp (P 4) (P 3) ~ P 2)
_test_Div_RoundUp_P4_P3 =  Dict
_test_Rem_RoundUp_P4_P3 :: Dict (K.Rem 'K.RoundUp (P 4) (P 3) ~ N 2)
_test_Rem_RoundUp_P4_P3 =  Dict
_test_Div_RoundUp_P4_P4 :: Dict (K.Div 'K.RoundUp (P 4) (P 4) ~ P 1)
_test_Div_RoundUp_P4_P4 =  Dict
_test_Rem_RoundUp_P4_P4 :: Dict (K.Rem 'K.RoundUp (P 4) (P 4) ~ Z)
_test_Rem_RoundUp_P4_P4 =  Dict
_test_Div_RoundZero_N1_N1 :: Dict (K.Div 'K.RoundZero (N 1) (N 1) ~ P 1)
_test_Div_RoundZero_N1_N1 =  Dict
_test_Rem_RoundZero_N1_N1 :: Dict (K.Rem 'K.RoundZero (N 1) (N 1) ~ Z)
_test_Rem_RoundZero_N1_N1 =  Dict
_test_Div_RoundZero_N1_N2 :: Dict (K.Div 'K.RoundZero (N 1) (N 2) ~ Z)
_test_Div_RoundZero_N1_N2 =  Dict
_test_Rem_RoundZero_N1_N2 :: Dict (K.Rem 'K.RoundZero (N 1) (N 2) ~ N 1)
_test_Rem_RoundZero_N1_N2 =  Dict
_test_Div_RoundZero_N1_N3 :: Dict (K.Div 'K.RoundZero (N 1) (N 3) ~ Z)
_test_Div_RoundZero_N1_N3 =  Dict
_test_Rem_RoundZero_N1_N3 :: Dict (K.Rem 'K.RoundZero (N 1) (N 3) ~ N 1)
_test_Rem_RoundZero_N1_N3 =  Dict
_test_Div_RoundZero_N1_N4 :: Dict (K.Div 'K.RoundZero (N 1) (N 4) ~ Z)
_test_Div_RoundZero_N1_N4 =  Dict
_test_Rem_RoundZero_N1_N4 :: Dict (K.Rem 'K.RoundZero (N 1) (N 4) ~ N 1)
_test_Rem_RoundZero_N1_N4 =  Dict
_test_Div_RoundZero_N1_P1 :: Dict (K.Div 'K.RoundZero (N 1) (P 1) ~ N 1)
_test_Div_RoundZero_N1_P1 =  Dict
_test_Rem_RoundZero_N1_P1 :: Dict (K.Rem 'K.RoundZero (N 1) (P 1) ~ Z)
_test_Rem_RoundZero_N1_P1 =  Dict
_test_Div_RoundZero_N1_P2 :: Dict (K.Div 'K.RoundZero (N 1) (P 2) ~ Z)
_test_Div_RoundZero_N1_P2 =  Dict
_test_Rem_RoundZero_N1_P2 :: Dict (K.Rem 'K.RoundZero (N 1) (P 2) ~ N 1)
_test_Rem_RoundZero_N1_P2 =  Dict
_test_Div_RoundZero_N1_P3 :: Dict (K.Div 'K.RoundZero (N 1) (P 3) ~ Z)
_test_Div_RoundZero_N1_P3 =  Dict
_test_Rem_RoundZero_N1_P3 :: Dict (K.Rem 'K.RoundZero (N 1) (P 3) ~ N 1)
_test_Rem_RoundZero_N1_P3 =  Dict
_test_Div_RoundZero_N1_P4 :: Dict (K.Div 'K.RoundZero (N 1) (P 4) ~ Z)
_test_Div_RoundZero_N1_P4 =  Dict
_test_Rem_RoundZero_N1_P4 :: Dict (K.Rem 'K.RoundZero (N 1) (P 4) ~ N 1)
_test_Rem_RoundZero_N1_P4 =  Dict
_test_Div_RoundZero_N2_N1 :: Dict (K.Div 'K.RoundZero (N 2) (N 1) ~ P 2)
_test_Div_RoundZero_N2_N1 =  Dict
_test_Rem_RoundZero_N2_N1 :: Dict (K.Rem 'K.RoundZero (N 2) (N 1) ~ Z)
_test_Rem_RoundZero_N2_N1 =  Dict
_test_Div_RoundZero_N2_N2 :: Dict (K.Div 'K.RoundZero (N 2) (N 2) ~ P 1)
_test_Div_RoundZero_N2_N2 =  Dict
_test_Rem_RoundZero_N2_N2 :: Dict (K.Rem 'K.RoundZero (N 2) (N 2) ~ Z)
_test_Rem_RoundZero_N2_N2 =  Dict
_test_Div_RoundZero_N2_N3 :: Dict (K.Div 'K.RoundZero (N 2) (N 3) ~ Z)
_test_Div_RoundZero_N2_N3 =  Dict
_test_Rem_RoundZero_N2_N3 :: Dict (K.Rem 'K.RoundZero (N 2) (N 3) ~ N 2)
_test_Rem_RoundZero_N2_N3 =  Dict
_test_Div_RoundZero_N2_N4 :: Dict (K.Div 'K.RoundZero (N 2) (N 4) ~ Z)
_test_Div_RoundZero_N2_N4 =  Dict
_test_Rem_RoundZero_N2_N4 :: Dict (K.Rem 'K.RoundZero (N 2) (N 4) ~ N 2)
_test_Rem_RoundZero_N2_N4 =  Dict
_test_Div_RoundZero_N2_P1 :: Dict (K.Div 'K.RoundZero (N 2) (P 1) ~ N 2)
_test_Div_RoundZero_N2_P1 =  Dict
_test_Rem_RoundZero_N2_P1 :: Dict (K.Rem 'K.RoundZero (N 2) (P 1) ~ Z)
_test_Rem_RoundZero_N2_P1 =  Dict
_test_Div_RoundZero_N2_P2 :: Dict (K.Div 'K.RoundZero (N 2) (P 2) ~ N 1)
_test_Div_RoundZero_N2_P2 =  Dict
_test_Rem_RoundZero_N2_P2 :: Dict (K.Rem 'K.RoundZero (N 2) (P 2) ~ Z)
_test_Rem_RoundZero_N2_P2 =  Dict
_test_Div_RoundZero_N2_P3 :: Dict (K.Div 'K.RoundZero (N 2) (P 3) ~ Z)
_test_Div_RoundZero_N2_P3 =  Dict
_test_Rem_RoundZero_N2_P3 :: Dict (K.Rem 'K.RoundZero (N 2) (P 3) ~ N 2)
_test_Rem_RoundZero_N2_P3 =  Dict
_test_Div_RoundZero_N2_P4 :: Dict (K.Div 'K.RoundZero (N 2) (P 4) ~ Z)
_test_Div_RoundZero_N2_P4 =  Dict
_test_Rem_RoundZero_N2_P4 :: Dict (K.Rem 'K.RoundZero (N 2) (P 4) ~ N 2)
_test_Rem_RoundZero_N2_P4 =  Dict
_test_Div_RoundZero_N3_N1 :: Dict (K.Div 'K.RoundZero (N 3) (N 1) ~ P 3)
_test_Div_RoundZero_N3_N1 =  Dict
_test_Rem_RoundZero_N3_N1 :: Dict (K.Rem 'K.RoundZero (N 3) (N 1) ~ Z)
_test_Rem_RoundZero_N3_N1 =  Dict
_test_Div_RoundZero_N3_N2 :: Dict (K.Div 'K.RoundZero (N 3) (N 2) ~ P 1)
_test_Div_RoundZero_N3_N2 =  Dict
_test_Rem_RoundZero_N3_N2 :: Dict (K.Rem 'K.RoundZero (N 3) (N 2) ~ N 1)
_test_Rem_RoundZero_N3_N2 =  Dict
_test_Div_RoundZero_N3_N3 :: Dict (K.Div 'K.RoundZero (N 3) (N 3) ~ P 1)
_test_Div_RoundZero_N3_N3 =  Dict
_test_Rem_RoundZero_N3_N3 :: Dict (K.Rem 'K.RoundZero (N 3) (N 3) ~ Z)
_test_Rem_RoundZero_N3_N3 =  Dict
_test_Div_RoundZero_N3_N4 :: Dict (K.Div 'K.RoundZero (N 3) (N 4) ~ Z)
_test_Div_RoundZero_N3_N4 =  Dict
_test_Rem_RoundZero_N3_N4 :: Dict (K.Rem 'K.RoundZero (N 3) (N 4) ~ N 3)
_test_Rem_RoundZero_N3_N4 =  Dict
_test_Div_RoundZero_N3_P1 :: Dict (K.Div 'K.RoundZero (N 3) (P 1) ~ N 3)
_test_Div_RoundZero_N3_P1 =  Dict
_test_Rem_RoundZero_N3_P1 :: Dict (K.Rem 'K.RoundZero (N 3) (P 1) ~ Z)
_test_Rem_RoundZero_N3_P1 =  Dict
_test_Div_RoundZero_N3_P2 :: Dict (K.Div 'K.RoundZero (N 3) (P 2) ~ N 1)
_test_Div_RoundZero_N3_P2 =  Dict
_test_Rem_RoundZero_N3_P2 :: Dict (K.Rem 'K.RoundZero (N 3) (P 2) ~ N 1)
_test_Rem_RoundZero_N3_P2 =  Dict
_test_Div_RoundZero_N3_P3 :: Dict (K.Div 'K.RoundZero (N 3) (P 3) ~ N 1)
_test_Div_RoundZero_N3_P3 =  Dict
_test_Rem_RoundZero_N3_P3 :: Dict (K.Rem 'K.RoundZero (N 3) (P 3) ~ Z)
_test_Rem_RoundZero_N3_P3 =  Dict
_test_Div_RoundZero_N3_P4 :: Dict (K.Div 'K.RoundZero (N 3) (P 4) ~ Z)
_test_Div_RoundZero_N3_P4 =  Dict
_test_Rem_RoundZero_N3_P4 :: Dict (K.Rem 'K.RoundZero (N 3) (P 4) ~ N 3)
_test_Rem_RoundZero_N3_P4 =  Dict
_test_Div_RoundZero_N4_N1 :: Dict (K.Div 'K.RoundZero (N 4) (N 1) ~ P 4)
_test_Div_RoundZero_N4_N1 =  Dict
_test_Rem_RoundZero_N4_N1 :: Dict (K.Rem 'K.RoundZero (N 4) (N 1) ~ Z)
_test_Rem_RoundZero_N4_N1 =  Dict
_test_Div_RoundZero_N4_N2 :: Dict (K.Div 'K.RoundZero (N 4) (N 2) ~ P 2)
_test_Div_RoundZero_N4_N2 =  Dict
_test_Rem_RoundZero_N4_N2 :: Dict (K.Rem 'K.RoundZero (N 4) (N 2) ~ Z)
_test_Rem_RoundZero_N4_N2 =  Dict
_test_Div_RoundZero_N4_N3 :: Dict (K.Div 'K.RoundZero (N 4) (N 3) ~ P 1)
_test_Div_RoundZero_N4_N3 =  Dict
_test_Rem_RoundZero_N4_N3 :: Dict (K.Rem 'K.RoundZero (N 4) (N 3) ~ N 1)
_test_Rem_RoundZero_N4_N3 =  Dict
_test_Div_RoundZero_N4_N4 :: Dict (K.Div 'K.RoundZero (N 4) (N 4) ~ P 1)
_test_Div_RoundZero_N4_N4 =  Dict
_test_Rem_RoundZero_N4_N4 :: Dict (K.Rem 'K.RoundZero (N 4) (N 4) ~ Z)
_test_Rem_RoundZero_N4_N4 =  Dict
_test_Div_RoundZero_N4_P1 :: Dict (K.Div 'K.RoundZero (N 4) (P 1) ~ N 4)
_test_Div_RoundZero_N4_P1 =  Dict
_test_Rem_RoundZero_N4_P1 :: Dict (K.Rem 'K.RoundZero (N 4) (P 1) ~ Z)
_test_Rem_RoundZero_N4_P1 =  Dict
_test_Div_RoundZero_N4_P2 :: Dict (K.Div 'K.RoundZero (N 4) (P 2) ~ N 2)
_test_Div_RoundZero_N4_P2 =  Dict
_test_Rem_RoundZero_N4_P2 :: Dict (K.Rem 'K.RoundZero (N 4) (P 2) ~ Z)
_test_Rem_RoundZero_N4_P2 =  Dict
_test_Div_RoundZero_N4_P3 :: Dict (K.Div 'K.RoundZero (N 4) (P 3) ~ N 1)
_test_Div_RoundZero_N4_P3 =  Dict
_test_Rem_RoundZero_N4_P3 :: Dict (K.Rem 'K.RoundZero (N 4) (P 3) ~ N 1)
_test_Rem_RoundZero_N4_P3 =  Dict
_test_Div_RoundZero_N4_P4 :: Dict (K.Div 'K.RoundZero (N 4) (P 4) ~ N 1)
_test_Div_RoundZero_N4_P4 =  Dict
_test_Rem_RoundZero_N4_P4 :: Dict (K.Rem 'K.RoundZero (N 4) (P 4) ~ Z)
_test_Rem_RoundZero_N4_P4 =  Dict
_test_Div_RoundZero_Z_N1 :: Dict (K.Div 'K.RoundZero Z (N 1) ~ Z)
_test_Div_RoundZero_Z_N1 =  Dict
_test_Div_RoundZero_Z_N2 :: Dict (K.Div 'K.RoundZero Z (N 2) ~ Z)
_test_Div_RoundZero_Z_N2 =  Dict
_test_Div_RoundZero_Z_N3 :: Dict (K.Div 'K.RoundZero Z (N 3) ~ Z)
_test_Div_RoundZero_Z_N3 =  Dict
_test_Div_RoundZero_Z_N4 :: Dict (K.Div 'K.RoundZero Z (N 4) ~ Z)
_test_Div_RoundZero_Z_N4 =  Dict
_test_Div_RoundZero_Z_P1 :: Dict (K.Div 'K.RoundZero Z (P 1) ~ Z)
_test_Div_RoundZero_Z_P1 =  Dict
_test_Div_RoundZero_Z_P2 :: Dict (K.Div 'K.RoundZero Z (P 2) ~ Z)
_test_Div_RoundZero_Z_P2 =  Dict
_test_Div_RoundZero_Z_P3 :: Dict (K.Div 'K.RoundZero Z (P 3) ~ Z)
_test_Div_RoundZero_Z_P3 =  Dict
_test_Div_RoundZero_Z_P4 :: Dict (K.Div 'K.RoundZero Z (P 4) ~ Z)
_test_Div_RoundZero_Z_P4 =  Dict
_test_Div_RoundZero_P1_N1 :: Dict (K.Div 'K.RoundZero (P 1) (N 1) ~ N 1)
_test_Div_RoundZero_P1_N1 =  Dict
_test_Rem_RoundZero_P1_N1 :: Dict (K.Rem 'K.RoundZero (P 1) (N 1) ~ Z)
_test_Rem_RoundZero_P1_N1 =  Dict
_test_Div_RoundZero_P1_N2 :: Dict (K.Div 'K.RoundZero (P 1) (N 2) ~ Z)
_test_Div_RoundZero_P1_N2 =  Dict
_test_Rem_RoundZero_P1_N2 :: Dict (K.Rem 'K.RoundZero (P 1) (N 2) ~ P 1)
_test_Rem_RoundZero_P1_N2 =  Dict
_test_Div_RoundZero_P1_N3 :: Dict (K.Div 'K.RoundZero (P 1) (N 3) ~ Z)
_test_Div_RoundZero_P1_N3 =  Dict
_test_Rem_RoundZero_P1_N3 :: Dict (K.Rem 'K.RoundZero (P 1) (N 3) ~ P 1)
_test_Rem_RoundZero_P1_N3 =  Dict
_test_Div_RoundZero_P1_N4 :: Dict (K.Div 'K.RoundZero (P 1) (N 4) ~ Z)
_test_Div_RoundZero_P1_N4 =  Dict
_test_Rem_RoundZero_P1_N4 :: Dict (K.Rem 'K.RoundZero (P 1) (N 4) ~ P 1)
_test_Rem_RoundZero_P1_N4 =  Dict
_test_Div_RoundZero_P1_P1 :: Dict (K.Div 'K.RoundZero (P 1) (P 1) ~ P 1)
_test_Div_RoundZero_P1_P1 =  Dict
_test_Rem_RoundZero_P1_P1 :: Dict (K.Rem 'K.RoundZero (P 1) (P 1) ~ Z)
_test_Rem_RoundZero_P1_P1 =  Dict
_test_Div_RoundZero_P1_P2 :: Dict (K.Div 'K.RoundZero (P 1) (P 2) ~ Z)
_test_Div_RoundZero_P1_P2 =  Dict
_test_Rem_RoundZero_P1_P2 :: Dict (K.Rem 'K.RoundZero (P 1) (P 2) ~ P 1)
_test_Rem_RoundZero_P1_P2 =  Dict
_test_Div_RoundZero_P1_P3 :: Dict (K.Div 'K.RoundZero (P 1) (P 3) ~ Z)
_test_Div_RoundZero_P1_P3 =  Dict
_test_Rem_RoundZero_P1_P3 :: Dict (K.Rem 'K.RoundZero (P 1) (P 3) ~ P 1)
_test_Rem_RoundZero_P1_P3 =  Dict
_test_Div_RoundZero_P1_P4 :: Dict (K.Div 'K.RoundZero (P 1) (P 4) ~ Z)
_test_Div_RoundZero_P1_P4 =  Dict
_test_Rem_RoundZero_P1_P4 :: Dict (K.Rem 'K.RoundZero (P 1) (P 4) ~ P 1)
_test_Rem_RoundZero_P1_P4 =  Dict
_test_Div_RoundZero_P2_N1 :: Dict (K.Div 'K.RoundZero (P 2) (N 1) ~ N 2)
_test_Div_RoundZero_P2_N1 =  Dict
_test_Rem_RoundZero_P2_N1 :: Dict (K.Rem 'K.RoundZero (P 2) (N 1) ~ Z)
_test_Rem_RoundZero_P2_N1 =  Dict
_test_Div_RoundZero_P2_N2 :: Dict (K.Div 'K.RoundZero (P 2) (N 2) ~ N 1)
_test_Div_RoundZero_P2_N2 =  Dict
_test_Rem_RoundZero_P2_N2 :: Dict (K.Rem 'K.RoundZero (P 2) (N 2) ~ Z)
_test_Rem_RoundZero_P2_N2 =  Dict
_test_Div_RoundZero_P2_N3 :: Dict (K.Div 'K.RoundZero (P 2) (N 3) ~ Z)
_test_Div_RoundZero_P2_N3 =  Dict
_test_Rem_RoundZero_P2_N3 :: Dict (K.Rem 'K.RoundZero (P 2) (N 3) ~ P 2)
_test_Rem_RoundZero_P2_N3 =  Dict
_test_Div_RoundZero_P2_N4 :: Dict (K.Div 'K.RoundZero (P 2) (N 4) ~ Z)
_test_Div_RoundZero_P2_N4 =  Dict
_test_Rem_RoundZero_P2_N4 :: Dict (K.Rem 'K.RoundZero (P 2) (N 4) ~ P 2)
_test_Rem_RoundZero_P2_N4 =  Dict
_test_Div_RoundZero_P2_P1 :: Dict (K.Div 'K.RoundZero (P 2) (P 1) ~ P 2)
_test_Div_RoundZero_P2_P1 =  Dict
_test_Rem_RoundZero_P2_P1 :: Dict (K.Rem 'K.RoundZero (P 2) (P 1) ~ Z)
_test_Rem_RoundZero_P2_P1 =  Dict
_test_Div_RoundZero_P2_P2 :: Dict (K.Div 'K.RoundZero (P 2) (P 2) ~ P 1)
_test_Div_RoundZero_P2_P2 =  Dict
_test_Rem_RoundZero_P2_P2 :: Dict (K.Rem 'K.RoundZero (P 2) (P 2) ~ Z)
_test_Rem_RoundZero_P2_P2 =  Dict
_test_Div_RoundZero_P2_P3 :: Dict (K.Div 'K.RoundZero (P 2) (P 3) ~ Z)
_test_Div_RoundZero_P2_P3 =  Dict
_test_Rem_RoundZero_P2_P3 :: Dict (K.Rem 'K.RoundZero (P 2) (P 3) ~ P 2)
_test_Rem_RoundZero_P2_P3 =  Dict
_test_Div_RoundZero_P2_P4 :: Dict (K.Div 'K.RoundZero (P 2) (P 4) ~ Z)
_test_Div_RoundZero_P2_P4 =  Dict
_test_Rem_RoundZero_P2_P4 :: Dict (K.Rem 'K.RoundZero (P 2) (P 4) ~ P 2)
_test_Rem_RoundZero_P2_P4 =  Dict
_test_Div_RoundZero_P3_N1 :: Dict (K.Div 'K.RoundZero (P 3) (N 1) ~ N 3)
_test_Div_RoundZero_P3_N1 =  Dict
_test_Rem_RoundZero_P3_N1 :: Dict (K.Rem 'K.RoundZero (P 3) (N 1) ~ Z)
_test_Rem_RoundZero_P3_N1 =  Dict
_test_Div_RoundZero_P3_N2 :: Dict (K.Div 'K.RoundZero (P 3) (N 2) ~ N 1)
_test_Div_RoundZero_P3_N2 =  Dict
_test_Rem_RoundZero_P3_N2 :: Dict (K.Rem 'K.RoundZero (P 3) (N 2) ~ P 1)
_test_Rem_RoundZero_P3_N2 =  Dict
_test_Div_RoundZero_P3_N3 :: Dict (K.Div 'K.RoundZero (P 3) (N 3) ~ N 1)
_test_Div_RoundZero_P3_N3 =  Dict
_test_Rem_RoundZero_P3_N3 :: Dict (K.Rem 'K.RoundZero (P 3) (N 3) ~ Z)
_test_Rem_RoundZero_P3_N3 =  Dict
_test_Div_RoundZero_P3_N4 :: Dict (K.Div 'K.RoundZero (P 3) (N 4) ~ Z)
_test_Div_RoundZero_P3_N4 =  Dict
_test_Rem_RoundZero_P3_N4 :: Dict (K.Rem 'K.RoundZero (P 3) (N 4) ~ P 3)
_test_Rem_RoundZero_P3_N4 =  Dict
_test_Div_RoundZero_P3_P1 :: Dict (K.Div 'K.RoundZero (P 3) (P 1) ~ P 3)
_test_Div_RoundZero_P3_P1 =  Dict
_test_Rem_RoundZero_P3_P1 :: Dict (K.Rem 'K.RoundZero (P 3) (P 1) ~ Z)
_test_Rem_RoundZero_P3_P1 =  Dict
_test_Div_RoundZero_P3_P2 :: Dict (K.Div 'K.RoundZero (P 3) (P 2) ~ P 1)
_test_Div_RoundZero_P3_P2 =  Dict
_test_Rem_RoundZero_P3_P2 :: Dict (K.Rem 'K.RoundZero (P 3) (P 2) ~ P 1)
_test_Rem_RoundZero_P3_P2 =  Dict
_test_Div_RoundZero_P3_P3 :: Dict (K.Div 'K.RoundZero (P 3) (P 3) ~ P 1)
_test_Div_RoundZero_P3_P3 =  Dict
_test_Rem_RoundZero_P3_P3 :: Dict (K.Rem 'K.RoundZero (P 3) (P 3) ~ Z)
_test_Rem_RoundZero_P3_P3 =  Dict
_test_Div_RoundZero_P3_P4 :: Dict (K.Div 'K.RoundZero (P 3) (P 4) ~ Z)
_test_Div_RoundZero_P3_P4 =  Dict
_test_Rem_RoundZero_P3_P4 :: Dict (K.Rem 'K.RoundZero (P 3) (P 4) ~ P 3)
_test_Rem_RoundZero_P3_P4 =  Dict
_test_Div_RoundZero_P4_N1 :: Dict (K.Div 'K.RoundZero (P 4) (N 1) ~ N 4)
_test_Div_RoundZero_P4_N1 =  Dict
_test_Rem_RoundZero_P4_N1 :: Dict (K.Rem 'K.RoundZero (P 4) (N 1) ~ Z)
_test_Rem_RoundZero_P4_N1 =  Dict
_test_Div_RoundZero_P4_N2 :: Dict (K.Div 'K.RoundZero (P 4) (N 2) ~ N 2)
_test_Div_RoundZero_P4_N2 =  Dict
_test_Rem_RoundZero_P4_N2 :: Dict (K.Rem 'K.RoundZero (P 4) (N 2) ~ Z)
_test_Rem_RoundZero_P4_N2 =  Dict
_test_Div_RoundZero_P4_N3 :: Dict (K.Div 'K.RoundZero (P 4) (N 3) ~ N 1)
_test_Div_RoundZero_P4_N3 =  Dict
_test_Rem_RoundZero_P4_N3 :: Dict (K.Rem 'K.RoundZero (P 4) (N 3) ~ P 1)
_test_Rem_RoundZero_P4_N3 =  Dict
_test_Div_RoundZero_P4_N4 :: Dict (K.Div 'K.RoundZero (P 4) (N 4) ~ N 1)
_test_Div_RoundZero_P4_N4 =  Dict
_test_Rem_RoundZero_P4_N4 :: Dict (K.Rem 'K.RoundZero (P 4) (N 4) ~ Z)
_test_Rem_RoundZero_P4_N4 =  Dict
_test_Div_RoundZero_P4_P1 :: Dict (K.Div 'K.RoundZero (P 4) (P 1) ~ P 4)
_test_Div_RoundZero_P4_P1 =  Dict
_test_Rem_RoundZero_P4_P1 :: Dict (K.Rem 'K.RoundZero (P 4) (P 1) ~ Z)
_test_Rem_RoundZero_P4_P1 =  Dict
_test_Div_RoundZero_P4_P2 :: Dict (K.Div 'K.RoundZero (P 4) (P 2) ~ P 2)
_test_Div_RoundZero_P4_P2 =  Dict
_test_Rem_RoundZero_P4_P2 :: Dict (K.Rem 'K.RoundZero (P 4) (P 2) ~ Z)
_test_Rem_RoundZero_P4_P2 =  Dict
_test_Div_RoundZero_P4_P3 :: Dict (K.Div 'K.RoundZero (P 4) (P 3) ~ P 1)
_test_Div_RoundZero_P4_P3 =  Dict
_test_Rem_RoundZero_P4_P3 :: Dict (K.Rem 'K.RoundZero (P 4) (P 3) ~ P 1)
_test_Rem_RoundZero_P4_P3 =  Dict
_test_Div_RoundZero_P4_P4 :: Dict (K.Div 'K.RoundZero (P 4) (P 4) ~ P 1)
_test_Div_RoundZero_P4_P4 =  Dict
_test_Rem_RoundZero_P4_P4 :: Dict (K.Rem 'K.RoundZero (P 4) (P 4) ~ Z)
_test_Rem_RoundZero_P4_P4 =  Dict

_test_ShowsPrec_AppPrec_Z :: Dict (P.ShowsPrec AppPrec Z "y" ~ "0y")
_test_ShowsPrec_AppPrec_Z =  Dict
_test_ShowsPrec_AppPrec_P1 :: Dict (P.ShowsPrec AppPrec (P 1) "y" ~ "1y")
_test_ShowsPrec_AppPrec_P1 =  Dict
_test_ShowsPrec_AppPrec_N1 :: Dict (P.ShowsPrec AppPrec (N 1) "y" ~ "-1y")
_test_ShowsPrec_AppPrec_N1 =  Dict

_test_ShowsPrec_AppPrec1_Z :: Dict (P.ShowsPrec AppPrec1 Z "y" ~ "0y")
_test_ShowsPrec_AppPrec1_Z =  Dict
_test_ShowsPrec_AppPrec1_P1 :: Dict (P.ShowsPrec AppPrec1 (P 1) "y" ~ "1y")
_test_ShowsPrec_AppPrec1_P1 =  Dict
_test_ShowsPrec_AppPrec1_N1 :: Dict (P.ShowsPrec AppPrec1 (N 1) "y" ~ "-1y")
_test_ShowsPrec_AppPrec1_N1 =  Dict

_test_ShowsPrecLit_AppPrec_Z :: Dict (K.ShowsPrecLit AppPrec Z "y" ~ "Zy")
_test_ShowsPrecLit_AppPrec_Z =  Dict
_test_ShowsPrecLit_AppPrec_P1 :: Dict (K.ShowsPrecLit AppPrec (P 1) "y" ~ "P 1y")
_test_ShowsPrecLit_AppPrec_P1 =  Dict
_test_ShowsPrecLit_AppPrec_N1 :: Dict (K.ShowsPrecLit AppPrec (N 1) "y" ~ "N 1y")
_test_ShowsPrecLit_AppPrec_N1 =  Dict

_test_ShowsPrecLit_AppPrec1_Z :: Dict (K.ShowsPrecLit AppPrec1 Z "y" ~ "Zy")
_test_ShowsPrecLit_AppPrec1_Z =  Dict
_test_ShowsPrecLit_AppPrec1_P1 :: Dict (K.ShowsPrecLit AppPrec1 (P 1) "y" ~ "(P 1)y")
_test_ShowsPrecLit_AppPrec1_P1 =  Dict
_test_ShowsPrecLit_AppPrec1_N1 :: Dict (K.ShowsPrecLit AppPrec1 (N 1) "y" ~ "(N 1)y")
_test_ShowsPrecLit_AppPrec1_N1 =  Dict

