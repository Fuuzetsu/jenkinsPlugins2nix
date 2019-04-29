# jenkinsPlugins2nix

```
Usage: jenkinsPlugins2nix [-r|--dependency-resolution [as-given|latest]]
                          (-p|--plugin PLUGIN_NAME{:PLUGIN_VERSION})
  Generate nix expressions for requested Jenkins plugins.

Available options:
  -r,--dependency-resolution [as-given|latest]
                           Dependency resolution (default: latest)
  -p,--plugin PLUGIN_NAME{:PLUGIN_VERSION}
                           Plugins we should generate nix for. Latest version is
                           used if not specified.
  -h,--help                Show this help text

```

Along with recent nixpkgs, you can then do the following.

```
jenkinsPlugins2nix -p github-api > plugins.nix

```

and in your `configuration.nix`:

```
services.jenkins.plugins = import plugins.nix { inherit (pkgs) fetchurl stdenv; };
```

## Version specification

Care is taken to preserve versions of plugins explicitly specified by
the user, even with the `as-given` resolution strategy. For example,
if plugin `A` has a dependency `B:0.2` in its manifest file and we
specify:

```
jenkinsPlugins2nix -r latest -p A:0.7 -p B:0.1
```

We will end up with `A:0.7` and `B:0.1`. This also applies when no
explicit version is provided which is equivalent to asking for the
latest one.

In case we only ask for `A`, the version of `B` will depend on
`--resolution-strategy`.
