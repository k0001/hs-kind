{ mkDerivation, base, lib, kind-integer, singletons, singletons-base }:
mkDerivation {
  pname = "kind-rational";
  version = "0.5.0";
  src = lib.sources.cleanSource ./.;
  libraryHaskellDepends =
    [ base kind-integer singletons singletons-base ];
  testHaskellDepends =
    [ base kind-integer singletons singletons-base ];
  homepage = "https://github.com/k0001/hs-kind";
  description = "Type-level rationals. Like KnownNat, but for rationals.";
  license = lib.licenses.bsd3;
}
