# jenkinsPlugins2nix

```
Usage: jenkinsPlugins2nix [-r|--dependency-resolution [as-given|latest|jenkins[:VERSION]]]
                          [--no-deps]
                          [--skip-optional]
                          (-p|--plugin PLUGIN_NAME{:PLUGIN_VERSION})
  Generate nix expressions for requested Jenkins plugins.

Available options:
  -r,--dependency-resolution [as-given|latest|jenkins[:VERSION]]
                           Dependency resolution (default: latest). Use
                           `jenkins:<VERSION>` to resolve against a specific
                           Jenkins version (for example: `-r jenkins:2.401`).
  -p,--plugin PLUGIN_NAME{:PLUGIN_VERSION}
                            Plugins we should generate nix for. Latest version is
                            used if not specified.
  --no-deps                Do not download or include transitive dependencies;
                           only generate nix for explicitly requested --plugin
                           entries.
  --skip-optional          Skip optional dependencies when downloading plugins
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

### Jenkins-specific strategy

You can select a specific Jenkins version for dependency resolution with
`-r jenkins:<VERSION>`. When this strategy is used, dependencies without an
explicit version will be resolved with regard to the supplied Jenkins
version (for example `-r jenkins:2.401`).

### Optional dependencies

By default the tool downloads all dependencies (including optional ones) to
avoid backward-compatibility problems. Use `--skip-optional` to exclude
optional dependencies from downloads when you prefer a minimal set of
dependencies.

### No dependency downloads

If you already have a fully resolved/pinned plugins list (for example produced by
`jenkins-plugin-cli --list`) and only want the nix conversion, use `--no-deps`.
This prevents `jenkinsPlugins2nix` from downloading any transitive dependencies
and will only produce output for the plugins explicitly provided via `--plugin`.
