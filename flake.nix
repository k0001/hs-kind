{
  description = "Haskell 'kind-integer' and 'kind-rational' libraries";

  inputs = {
    flakety.url = "github:k0001/flakety";
    nixpkgs.follows = "flakety/nixpkgs";
    flake-parts.follows = "flakety/flake-parts";
  };

  outputs = inputs@{ ... }:
    inputs.flake-parts.lib.mkFlake { inherit inputs; } {
      flake.overlays.default = inputs.nixpkgs.lib.composeManyExtensions [
        inputs.flakety.overlays.default
        (final: prev: {
          haskell = prev.haskell // {
            packageOverrides = prev.lib.composeExtensions
              (prev.haskell.packageOverrides or (_: _: { })) (hself: hsuper: {
                kind-integer = hself.callPackage ./kind-integer { };
                kind-rational = hself.callPackage ./kind-rational { };
              });
          };
        })
      ];
      systems = [ "x86_64-linux" ];
      perSystem = { config, system, pkgs, ... }: {
        _module.args.pkgs = import inputs.nixpkgs {
          inherit system;
          overlays = [ inputs.self.overlays.default ];
        };
        packages = {
          default = pkgs.releaseTools.aggregate {
            name = "every output from this flake";
            constituents = [
              config.devShells.ghc96
              config.packages.kind-integer__ghc96
              config.packages.kind-integer__ghc96.doc
              config.packages.kind-integer__ghc96__sdist
              config.packages.kind-integer__ghc96__sdistDoc
              config.packages.kind-rational__ghc96
              config.packages.kind-rational__ghc96.doc
              config.packages.kind-rational__ghc96__sdist
              config.packages.kind-rational__ghc96__sdistDoc

              config.devShells.ghc98
              config.packages.kind-integer__ghc98
              config.packages.kind-integer__ghc98.doc
              config.packages.kind-integer__ghc98__sdist
              config.packages.kind-integer__ghc98__sdistDoc
              config.packages.kind-rational__ghc98
              config.packages.kind-rational__ghc98.doc
              config.packages.kind-rational__ghc98__sdist
              config.packages.kind-rational__ghc98__sdistDoc
            ];
          };

          kind-integer__ghc96 = pkgs.haskell.packages.ghc96.kind-integer;
          kind-integer__ghc96__sdist =
            pkgs.haskell.packages.ghc96.cabalSdist { src = ./kind-integer; };
          kind-integer__ghc96__sdistDoc = pkgs.haskell.lib.documentationTarball
            config.packages.kind-integer__ghc96;
          kind-rational__ghc96 = pkgs.haskell.packages.ghc96.kind-rational;
          kind-rational__ghc96__sdist =
            pkgs.haskell.packages.ghc96.cabalSdist { src = ./kind-rational; };
          kind-rational__ghc96__sdistDoc = pkgs.haskell.lib.documentationTarball
            config.packages.kind-rational__ghc96;

          kind-integer__ghc98 = pkgs.haskell.packages.ghc98.kind-integer;
          kind-integer__ghc98__sdist =
            pkgs.haskell.packages.ghc98.cabalSdist { src = ./kind-integer; };
          kind-integer__ghc98__sdistDoc = pkgs.haskell.lib.documentationTarball
            config.packages.kind-integer__ghc98;
          kind-rational__ghc98 = pkgs.haskell.packages.ghc98.kind-rational;
          kind-rational__ghc98__sdist =
            pkgs.haskell.packages.ghc98.cabalSdist { src = ./kind-rational; };
          kind-rational__ghc98__sdistDoc = pkgs.haskell.lib.documentationTarball
            config.packages.kind-rational__ghc98;
        };
        devShells = let
          mkShellFor = ghc:
            ghc.shellFor {
              packages = p: [ p.kind-integer p.kind-rational ];
              withHoogle = true;
              nativeBuildInputs =
                [ pkgs.cabal-install pkgs.cabal2nix pkgs.ghcid ];
            };
        in {
          default = config.devShells.ghc98;
          ghc96 = mkShellFor pkgs.haskell.packages.ghc96;
          ghc98 = mkShellFor pkgs.haskell.packages.ghc98;
        };
      };
    };
}
