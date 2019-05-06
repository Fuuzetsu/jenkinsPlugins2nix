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
            rev = "0.6.1";
            sha256 = "1v1g63icw66290lsr8g51gb13dgm3xbgqdq71q2g0v0bfc2nfd1c";
            }) { inherit (nixpkgs) pkgs; compiler = compiler; };
in
haskellPackages.callPackage jenkinsPlugins2nix { hnix = pkgs.haskell.lib.dontCheck hnix; }
