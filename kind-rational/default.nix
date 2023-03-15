{ mkDerivation, base, lib, kind-integer }:
mkDerivation {
  pname = "kind-rational";
  version = "0.1";
  src = lib.sources.cleanSource ./.;
  libraryHaskellDepends = [ base kind-integer ];
  homepage = "https://github.com/k0001/hs-kind";
  description = "Type-level integers. Like KnownNat, but for integers.";
  license = lib.licenses.bsd3;
}