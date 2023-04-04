{ mkDerivation, base, lib, kind-integer, singletons }:
mkDerivation {
  pname = "kind-rational";
  version = "0.3";
  src = lib.sources.cleanSource ./.;
  libraryHaskellDepends = [ base kind-integer singletons ];
  homepage = "https://github.com/k0001/hs-kind";
  description = "Type-level integers. Like KnownNat, but for integers.";
  license = lib.licenses.bsd3;
}
