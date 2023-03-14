{
  description = "Haskell 'kind-integer' library";

  outputs = { self, nixpkgs }:
    let
      inherit (nixpkgs) lib;
      hs_kind-integer = import ./kind-integer;
      hspkgsOverrides = pself: psuper: hself: hsuper: {
        kind-integer = hsuper.callPackage hs_kind-integer { };
      };
      pkgsOverlay = pself: psuper: {
        haskell = psuper.haskell // {
          packageOverrides = hspkgsOverrides pself psuper;
        };
      };
      pkgsFor = system:
        import nixpkgs {
          inherit system;
          overlays = [ pkgsOverlay ];
        };

    in {
      inherit hs_kind-integer hspkgsOverrides pkgsOverlay;
      packages = lib.genAttrs [ "x86_64-linux" "i686-linux" "aarch64-linux" ]
        (system:
          let pkgs = pkgsFor system;
          in {
            default = pkgs.releaseTools.aggregate {
              name = "every output from this flake";
              constituents = let
                p = self.packages.${system};
                s = self.devShells.${system};
              in [
                # p.hs_kind-integer__ghcDefault
                p.hs_kind-integer__ghc943

                # p.hs_kind-integer__ghcDefault.doc
                p.hs_kind-integer__ghc943.doc

                # s.hs_kind-integer__ghcDefault
                s.hs_kind-integer__ghc943
              ];
            };
            # hs_kind-integer__ghcDefault = pkgs.haskellPackages.kind-integer;
            hs_kind-integer__ghc943 = pkgs.haskell.packages.ghc943.kind-integer;
          });
      devShells = lib.genAttrs [ "x86_64-linux" "i686-linux" "aarch64-linux" ]
        (system:
          let
            pkgs = pkgsFor system;
            mkShellFor = hpkgs:
              hpkgs.shellFor {
                packages = p: [ p.kind-integer ];
                withHoogle = true;
                nativeBuildInputs = [ pkgs.cabal-install pkgs.cabal2nix ];
              };
          in {
            default = self.devShells.${system}.hs_kind-integer__ghc943;
            # hs_kind-integer__ghcDefault = mkShellFor pkgs.haskellPackages;
            hs_kind-integer__ghc943 = mkShellFor pkgs.haskell.packages.ghc943;
          });
    };

}
