{
  description = "A Haskell re-implementation of the Nix expression language";

  inputs = {
    nixpkgs.follows = "haskellNix/nixpkgs-unstable";
    nix.url = "github:NixOS/nix";
    haskellNix.url = "github:input-output-hk/haskell.nix/pull/2467/merge";
    flake-utils.url = "github:numtide/flake-utils";
  };

  # Binary Cache for haskell.nix
  # nixConfig = {
  #   extra-trusted-public-keys = [ "hydra.iohk.io:f/Ea+s+dFdN+3Y/G+FDgSq+a5NEWhJGzdjvKNGv0/EQ=" ];
  #   extra-substituters = [ "https://cache.iog.io" ];
  # };

  outputs = { self, nixpkgs, nix, flake-utils, haskellNix }:
    flake-utils.lib.eachDefaultSystem (system:
      let
        pkgs = import nixpkgs {
          inherit system overlays;
          inherit (haskellNix) config;
        };
        flake = pkgs.hnix.flake {
        };
        overlays = [ haskellNix.overlay
          (final: prev: {
            # Source for hnix-store-json that includes upstream test data
            hnix-store-json-src = final.lib.fileset.toSource {
              root = ./hnix-store;
              fileset = final.lib.fileset.unions [
                ./hnix-store/hnix-store-json
                (final.lib.fileset.fileFilter (file: file.hasExt "json") ./hnix-store/upstream-nix/src/libstore-tests/data/build-result)
                (final.lib.fileset.fileFilter (file: file.hasExt "json") ./hnix-store/upstream-nix/src/libstore-tests/data/content-address)
                (final.lib.fileset.fileFilter (file: file.hasExt "json") ./hnix-store/upstream-nix/src/libstore-tests/data/derived-path)
                (final.lib.fileset.fileFilter (file: file.hasExt "json") ./hnix-store/upstream-nix/src/libstore-tests/data/outputs-spec)
                (final.lib.fileset.fileFilter (file: file.hasExt "json") ./hnix-store/upstream-nix/src/libstore-tests/data/realisation)
                (final.lib.fileset.fileFilter (file: file.hasExt "json") ./hnix-store/upstream-nix/src/libstore-tests/data/store-path)
                (final.lib.fileset.fileFilter (file: file.hasExt "json") ./hnix-store/upstream-nix/src/libutil-tests/data/hash)
              ];
            };
            hnix =
              final.haskell-nix.project' {
                src = ./.;
                supportHpack = true;
                compiler-nix-name = "ghc912";
                shell = {
                  tools = {
                    cabal = {};
                    # hlint = {};
                    # haskell-language-server = {};
                  };
                  buildInputs = with pkgs; [
                    pkg-config
                  ];
                  withHoogle = false;
                  # Point to source data directory for development
                  shellHook = let
                    # Get the Nix-built hnix packages for the GHC API.
                    # These are used by Nix.Compile.Driver.initSession to find runtime types.
                    typesPkg = final.hnix.hsPkgs.hnix.components.sublibs.hnix-types;
                    runtimePkg = final.hnix.hsPkgs.hnix.components.sublibs.hnix-compile-runtime;
                  in ''
                    export NIX_DATA_DIR="$PWD/data"
                    # NIX_GHC_LIBDIR: Override ghc-paths library directory at runtime.
                    # The ghc-paths package is compiled with a hard-coded path, but at runtime
                    # we need the GHC from the shell environment (ghc-shell-for-packages-...-env)
                    # which includes all project dependencies in its global package DB.
                    # We use $(ghc --print-libdir) to get the correct path dynamically.
                    export NIX_GHC_LIBDIR="$(ghc --print-libdir)"
                    # HNIX_PACKAGE_DBS: Only used when no .ghc.environment.* file exists.
                    #
                    # With 'cabal build --write-ghc-environment-files=always', the environment
                    # file contains Cabal-built packages, and HNIX_PACKAGE_DBS is ignored to
                    # avoid duplicate module errors (FoundMultiple).
                    #
                    # This variable is for pure Nix builds where no environment file exists.
                    export HNIX_PACKAGE_DBS="${typesPkg}/package.conf.d:${runtimePkg}/package.conf.d"
                  '';
                };
                modules = [{
                  contentAddressed = true;
                  ghcOptions = [ "-fobject-determinism" ];
                  # enableLibraryProfiling = true;
                  # profilingDetail = "none";
                  # Allow using the ghc package (GHC API)
                  reinstallableLibGhc = true;
                  # Override hnix-store-json source to include upstream test data
                  packages.hnix-store-json.src = final.lib.mkForce (final.runCommand "hnix-store-json-src" {} ''
                    cp -r ${final.hnix-store-json-src}/hnix-store-json $out
                    chmod -R +w $out
                    # Create upstream-libstore-data with actual files instead of symlink
                    rm -f $out/upstream-libstore-data
                    mkdir -p $out/upstream-libstore-data
                    cp -r ${final.hnix-store-json-src}/upstream-nix/src/libstore-tests/data/* $out/upstream-libstore-data/
                    # Create upstream-libutil-data with actual files instead of symlink
                    rm -f $out/upstream-libutil-data
                    mkdir -p $out/upstream-libutil-data
                    cp -r ${final.hnix-store-json-src}/upstream-nix/src/libutil-tests/data/* $out/upstream-libutil-data/
                  '');
                }];
              };
          })
        ];
        # Extract sublibraries for use with GHC API
        hnixTypes = pkgs.hnix.hsPkgs.hnix.components.sublibs.hnix-types;
        hnixCompileRuntime = pkgs.hnix.hsPkgs.hnix.components.sublibs.hnix-compile-runtime;
      in flake // {
        legacyPackages = pkgs;
        packages.default = flake.packages."hnix:exe:hnix";
        packages.hnix-types = hnixTypes;
        packages.hnix-compile-runtime = hnixCompileRuntime;
      });
}

