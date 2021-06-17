# This is just a compat shim for nix flakes: to add things to the expression,
# look in `flake.nix` instead.
let
  lock = builtins.fromJSON (builtins.readFile ./flake.lock);
  flake-compat = import (fetchTarball {
    url =
      "https://github.com/edolstra/flake-compat/archive/${lock.nodes.flake-compat.locked.rev}.tar.gz";
    sha256 = lock.nodes.flake-compat.locked.narHash;
  }) { src = ./.; };
  pkgs = import (fetchTarball {
    url =
      "https://github.com/NixOS/nixpkgs/archive/${lock.nodes.nixpkgs.locked.rev}.tar.gz";
    sha256 = lock.nodes.nixpkgs.locked.narHash;
  }) { };
  legacyPackages = flake-compat.defaultNix.legacyPackages.${pkgs.stdenv.system};
  flakePackages = flake-compat.defaultNix.packages.${pkgs.stdenv.system};
  # Expose full package set for commands like `nix run`, giving priority to
  # anything defined in the flake package set.
in legacyPackages // flakePackages
