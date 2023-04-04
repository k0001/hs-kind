# Version 0.4

* COMPILER ASSISTED BREAKING CHANGE: `TestEquality` and `TestCoercion` consider
  `N 0` and `P 0` to be different.

* BREAKING CHANGE: Term-level `Integer`s and `SInteger`s are not automatically
  normalized anymore. This is so that `SDecide`, `TestEquality` and
  `TestCoercion` behave as expected. This is mostly an internal change,
  but it can be observed in the `Show` instance for `SInteger`, for example.

* Added role annotations to `SInteger`.

* Add dependency on `singletons` so that we can give a `Sing` and `SDecide`
  instances for type-level `Integer`s.

* Export `fromSInteger'`.


# Version 0.3

* COMPILER ASSISTED BREAKING CHANGE: Renamed `Mod` to `Rem`, `DivMod` to
  `DivRem`, `mod` to `rem`, `divMod` to `divRem`.


# Version 0.2

* COMPILER ASSISTED BREAKING CHANGE: Removed `Div`, `Mod`, `Quote`
  and `Rem` in favour of more polymorphic `Div`, `Mod`.

* COMPILER ASSISTED BREAKING CHANGE: Removed `integerVal'`. Nothing
  wrong with it, just redundant.

* Export `Sign`, `Abs`, `GCD`, `LCM`, `Odd`, `Even`, `toPrelude`,
  `fromPrelude`, `showsPrecTypeLit`, `div`, `mod`, `divMod`,
  `DivMod`.

* Add `Eq`, `Ord`, `Show`, `Read` instances for `Integer`.

* Minor cabal and documentation improvements.


# Version 0.1

* Initial version.
