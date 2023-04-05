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
