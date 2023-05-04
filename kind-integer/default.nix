{ mkDerivation, base, lib, singletons, singletons-base }:
mkDerivation {
  pname = "kind-integer";
  version = "0.6.0";
  src = lib.sources.cleanSource ./.;
  libraryHaskellDepends = [ base singletons singletons-base ];
  testHaskellDepends = [ base singletons singletons-base ];
  homepage = "https://github.com/k0001/hs-kind";
  description = "Type-level integers. Like KnownNat, but for integers.";
  license = lib.licenses.bsd3;
}
