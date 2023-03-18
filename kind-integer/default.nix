{ mkDerivation, base, lib }:
mkDerivation {
  pname = "kind-integer";
  version = "0.3";
  src = lib.sources.cleanSource ./.;
  libraryHaskellDepends = [ base ];
  homepage = "https://github.com/k0001/hs-kind";
  description = "Type-level integers. Like KnownNat, but for integers.";
  license = lib.licenses.bsd3;
}
