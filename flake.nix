{
  description = "jenkinsPlugins2nix";
  inputs = {
    flake-utils.url = "github:numtide/flake-utils";
    nixpkgs.url = "github:NixOS/nixpkgs/22.11";
    flake-compat = {
      url = "github:edolstra/flake-compat";
      flake = false;
    };
  };

  outputs = { self, flake-compat, flake-utils, nixpkgs }: flake-utils.lib.eachDefaultSystem (system:
    let pkgs = import nixpkgs {
      inherit system;
    };
    in
    rec {
      defaultPackage = packages.jenkinsPlugins2nix;
      legacyPackages.jenkinsPlugins2nix = packages.jenkinsPlugins2nix;
      packages = flake-utils.lib.flattenTree {
        jenkinsPlugins2nix = pkgs.haskellPackages.callPackage ./jenkinsPlugins2nix.nix { };
      };
      apps.jenkinsPlugins2nix = flake-utils.lib.mkApp { drv = packages.jenkinsPlugins2nix; };
      defaultApp = apps.jenkinsPlugins2nix;
      devShell = defaultPackage.env.overrideAttrs (attrs: {
        # Add useful tools to the development shell.
        buildInputs = attrs.buildInputs or [ ] ++ [ pkgs.cabal-install pkgs.stack pkgs.cabal2nix ];
      });
    }
  );

  # packages.x86_64-linux.hello = nixpkgs.legacyPackages.x86_64-linux.hello;

  # defaultPackage.x86_64-linux = self.packages.x86_64-linux.hello;



}
