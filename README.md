# jenkinsPlugins2nix

```
jenkinsPlugins2nix: PLUGIN_NAME{:PLUGIN_VERSION} [PLUGIN_NAME{:PLUGIN_VERSION} ...]
```

Along with recent nixpkgs, you can then do the following.

```
jenkinsPlugins2nix github-api > plugins.nix

```

and in your `configuration.nix`:

```
services.jenkins.plugins = import plugins.nix { inherit (pkgs) fetchurl stdenv; };
```
