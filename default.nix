{ nixpkgs ? import <nixpkgs> {}, compiler ? "ghc864" }:
let
  inherit (nixpkgs) pkgs;
  haskellPackages = if compiler == "default"
                       then pkgs.haskellPackages
                       else pkgs.haskell.packages.${compiler};
  jenkinsPlugins2nix = import ./jenkinsPlugins2nix.nix;
  hnix = import (nixpkgs.fetchFromGitHub {
            owner = "haskell-nix";
            repo = "hnix";
            rev = "0.6.0";
            sha256 = "1gms2pb4x95c1ibr2gbsbfbba4sm5qcz8nwmwksiwpx1qcmmi6j4";
            }) { inherit (nixpkgs) pkgs; compiler = compiler; };
in
haskellPackages.callPackage jenkinsPlugins2nix { hnix = pkgs.haskell.lib.dontCheck hnix; }
