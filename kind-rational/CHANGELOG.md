# Version 0.5.1

* Add missing `COMPLETE` pragma.


# Version 0.5.0

* COMPILER ASSISTED BREAKING CHANGE: `KindRational.Rational` is now
  is now only ever used as a kind. So, all term-level functions in
  the `KindRational` consume and produce `Prelude.Rational`s. Term-level
  functions crash will `error` if they are supplied `Rational`s that
  are not `Reduced` as input.

* COMPILER ASSISTED BREAKING CHANGE: Removed `Eq`, `Ord`, `Show` and
  `Read` instances for `KindRational.Rational`.

* COMPILER ASSISTED BREAKING CHANGE: Removed `withTerminating` in favor of
  `termination`.

* COMPILER ASSISTED BREAKING CHANGE: `Rational`s that are not `Reduced`
  are not `KnownRational`s anymore.

* COMPILER ASSISTED BREAKING CHANGE: `KnownRational` is now a type-synonym
  that implies `Normalize r ~ r`, `KnownInteger (Num r)` and
  `KnownNat (Den r)` as well.

* Added `singletons-base` support for `Rational`, including `PNum`, `SNum`,
  `PEq`, `SEq`, `POrd`, `SOrd`, `PShow` and `SShow`. Most arithmetic
  functions are now exported through `PNum` and `SNum`, rather than standalone.

* Added `readPrecTypeLit`, `SRationalTerminates`, `SRationalTerminatesNot`,
  `normalize`, `rationalLit`, `NonTerminating`, `%`, `%%`, `ToRational`,
  `mkRational`, `sMkRational`, `sRecip'`.

* Added `ShowLit`, `ShowsLit`, `ShowsPrecLit` and its singletons and
  promoted versions.

* Added defunctionalization symbols.


# Version 0.4

* COMPILER ASSISTED BREAKING CHANGE: `rationalVal`, `someRationalVal`,
  `fromSRational`, `terminates`, `divRem`, `div` and `rem` now deal
  with `KindRational`'s `Rational`s, rather than `Prelude`'s `Rational`s.

* COMPILER ASSISTED BREAKING CHANGE: Removed `fromSRational'`.

* Added `SingI` and `SingKind` instances.


# Version 0.3

* COMPILER ASSISTED BREAKING CHANGE: `TestEquality` and `TestCoercion`
  don't `Normalize` inputs before making a decision anymore.

* BREAKING CHANGE: The `Rational` inside `SRational` is not automatically
  normalized anymore. This is so that `SDecide`, `TestEquality` and
  `TestCoercion` behave as expected, treating `1/2` differently than `2/4`,
  for example. This is mostly an internal change, but it can be observed in
  the `Show` instance for `SRational`, for example.

* Added role annotations to `SRational`.

* Add dependency on `singletons` so that we can give a `Sing` and `SDecide`
  instances for type-level `Rational`s.

* Export `fromSRational'`.

# Version 0.2

* COMPILER ASSISTED BREAKING CHANGE: Removed `Mod`, `DivMod`, `mod`, `divMod`.

* COMPILER ASSISTED BREAKING CHANGE: Renamed `Dif` to `Rem`, `DivDif` to
  `DivRem`, `mod` to `rem`, `divDif` to `divRem`.


# Version 0.1

* Initial version.
