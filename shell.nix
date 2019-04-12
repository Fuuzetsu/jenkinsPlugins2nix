{ nixpkgs ? import <nixpkgs> {}, compiler ? "ghc864" }:
let
  drv = import ./default.nix { inherit nixpkgs compiler; };
in
  if nixpkgs.pkgs.lib.inNixShell then drv.env else drv
