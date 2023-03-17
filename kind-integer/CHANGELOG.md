# Version 0.1.1

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
