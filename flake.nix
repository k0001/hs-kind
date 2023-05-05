{
  description = "Haskell 'kind-integer' and 'kind-rational' libraries";

  inputs = {
    nixpkgs.url =
      "github:NixOS/nixpkgs/43297919d746de7b71fc83dba95272b2991ba20f";
    flake-parts.url = "github:hercules-ci/flake-parts";
    singletons_3_2.url = "github:goldfirere/singletons/singletons-th-base-3.2";
    singletons_3_2.flake = false;
  };

  outputs = inputs@{ ... }:
    inputs.flake-parts.lib.mkFlake { inherit inputs; } {
      flake.overlays.default = final: prev: {
        haskell = prev.haskell // {
          packageOverrides = prev.lib.composeExtensions
            (prev.haskell.packageOverrides or (_: _: { })) (hself: hsuper:
              {
                kind-integer = hself.callPackage ./kind-integer { };
                kind-rational = hself.callPackage ./kind-rational { };
                chell = prev.haskell.lib.doJailbreak hsuper.chell;
                singletons = hself.callHackage "singletons" "3.0.2" { };
              } // prev.lib.optionalAttrs
              (prev.lib.versionAtLeast hsuper.ghc.version "9.4") {
                singletons-base =
                  hself.callHackage "singletons-base" "3.1.1" { };
                singletons-th = hself.callHackage "singletons-th" "3.1.1" { };
                th-desugar = hself.callHackage "th-desugar" "1.14" { };
                th-abstraction =
                  hself.callHackage "th-abstraction" "0.4.5.0" { };
              } // prev.lib.optionalAttrs
              (prev.lib.versionAtLeast hsuper.ghc.version "9.6") {
                singletons-base = hself.callCabal2nix "singletons-base"
                  "${inputs.singletons_3_2}/singletons-base" { };
                singletons-th = hself.callCabal2nix "singletons-th"
                  "${inputs.singletons_3_2}/singletons-th" { };
                th-abstraction =
                  hself.callHackage "th-abstraction" "0.5.0.0" { };
                th-desugar = hself.callHackage "th-desugar" "1.15" { };
              });
        };
      };
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
              config.packages.kind-integer__ghc94
              config.packages.kind-rational__ghc94
              config.packages.kind-integer__ghc94.doc
              config.packages.kind-rational__ghc94.doc
              config.packages.kind-integer__ghc94__sdist
              config.packages.kind-rational__ghc94__sdist
              config.packages.kind-integer__ghc94__sdistDoc
              config.packages.kind-rational__ghc94__sdistDoc
              config.devShells.ghc94

              config.packages.kind-integer__ghc96
              config.packages.kind-rational__ghc96
              config.packages.kind-integer__ghc96.doc
              config.packages.kind-rational__ghc96.doc
              config.packages.kind-integer__ghc96__sdist
              config.packages.kind-rational__ghc96__sdist
              config.packages.kind-integer__ghc96__sdistDoc
              config.packages.kind-rational__ghc96__sdistDoc
              config.devShells.ghc96
            ];
          };

          kind-integer__ghc94 = pkgs.haskell.packages.ghc94.kind-integer;
          kind-rational__ghc94 = pkgs.haskell.packages.ghc94.kind-rational;
          kind-integer__ghc94__sdist =
            pkgs.haskell.packages.ghc94.cabalSdist { src = ./kind-integer; };
          kind-rational__ghc94__sdist =
            pkgs.haskell.packages.ghc94.cabalSdist { src = ./kind-rational; };
          kind-integer__ghc94__sdistDoc = pkgs.haskell.lib.documentationTarball
            config.packages.kind-integer__ghc94;
          kind-rational__ghc94__sdistDoc = pkgs.haskell.lib.documentationTarball
            config.packages.kind-rational__ghc94;

          kind-integer__ghc96 = pkgs.haskell.packages.ghc96.kind-integer;
          kind-rational__ghc96 = pkgs.haskell.packages.ghc96.kind-rational;
          kind-integer__ghc96__sdist =
            pkgs.haskell.packages.ghc96.cabalSdist { src = ./kind-integer; };
          kind-rational__ghc96__sdist =
            pkgs.haskell.packages.ghc96.cabalSdist { src = ./kind-rational; };
          kind-integer__ghc96__sdistDoc = pkgs.haskell.lib.documentationTarball
            config.packages.kind-integer__ghc96;
          kind-rational__ghc96__sdistDoc = pkgs.haskell.lib.documentationTarball
            config.packages.kind-rational__ghc96;
        };
        devShells = let
          mkShellFor = ghc:
            ghc.shellFor {
              packages = p: [ p.kind-integer p.kind-rational ];
              withHoogle = false;
              nativeBuildInputs =
                [ pkgs.cabal-install pkgs.cabal2nix pkgs.ghcid ];
            };
        in {
          default = config.devShells.ghc94;
          ghc94 = mkShellFor pkgs.haskell.packages.ghc94;
          ghc96 = mkShellFor pkgs.haskell.packages.ghc96;
        };
      };
    };
}
