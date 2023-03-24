{
  description = "Haskell 'kind-integer' library";

  outputs = { self, nixpkgs }:
    let
      inherit (nixpkgs) lib;
      overlay = pself: psuper: {
        haskell = psuper.haskell // {
          packageOverrides = hself: hsuper: {
            kind-integer = hsuper.callPackage ./kind-integer { };
            kind-rational = hsuper.callPackage ./kind-rational { };
          };
        };
      };
      pkgsFor = system:
        import nixpkgs {
          inherit system;
          overlays = [ overlay ];
        };

    in {
      overlays.default = final: prev: overlay final prev;
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
                # p.hs_kind-rational__ghcDefault
                p.hs_kind-rational__ghc943

                # p.hs_kind-integer__ghcDefault.doc
                p.hs_kind-integer__ghc943.doc
                # p.hs_kind-rational__ghcDefault.doc
                p.hs_kind-rational__ghc943.doc

                # s.ghcDefault
                s.ghc943
              ];
            };
            # hs_kind-integer__ghcDefault = pkgs.haskellPackages.kind-integer;
            hs_kind-integer__ghc943 = pkgs.haskell.packages.ghc943.kind-integer;
            # hs_kind-rational__ghcDefault = pkgs.haskellPackages.kind-rational;
            hs_kind-rational__ghc943 =
              pkgs.haskell.packages.ghc943.kind-rational;
          });
      devShells = lib.genAttrs [ "x86_64-linux" "i686-linux" "aarch64-linux" ]
        (system:
          let
            pkgs = pkgsFor system;
            mkShellFor = hpkgs:
              hpkgs.shellFor {
                packages = p: [ p.kind-integer p.kind-rational ];
                withHoogle = true;
                nativeBuildInputs = [ pkgs.cabal-install pkgs.cabal2nix ];
              };
          in {
            default = self.devShells.${system}.ghc943;
            # ghcDefault = mkShellFor pkgs.haskellPackages;
            ghc943 = mkShellFor pkgs.haskell.packages.ghc943;
          });
    };
}
