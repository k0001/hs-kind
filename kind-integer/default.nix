{ mkDerivation, base, lib, singletons }:
mkDerivation {
  pname = "kind-integer";
  version = "0.6";
  src = lib.sources.cleanSource ./.;
  libraryHaskellDepends = [ base singletons ];
  testHaskellDepends = [ base singletons ];
  homepage = "https://github.com/k0001/hs-kind";
  description = "Type-level integers. Like KnownNat, but for integers.";
  license = lib.licenses.bsd3;
}
