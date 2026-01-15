{
  description = "A Haskell re-implementation of the Nix expression language";

  inputs = {
    nixpkgs.follows = "haskellNix/nixpkgs-unstable";
    nix.url = "github:NixOS/nix";
    haskellNix.url = "github:input-output-hk/haskell.nix/pull/2434/merge";
    flake-utils.url = "github:numtide/flake-utils";
  };

  # Binary Cache for haskell.nix
  nixConfig = {
    extra-trusted-public-keys = [ "hydra.iohk.io:f/Ea+s+dFdN+3Y/G+FDgSq+a5NEWhJGzdjvKNGv0/EQ=" ];
    extra-substituters = [ "https://cache.iog.io" ];
  };

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
                compiler-nix-name = "ghc9141";
                shell = {
                  tools = {
                    cabal.cabalProjectLocal = ''
                      -- Some relaxed bounds are needed for base.
                      allow-newer:
                        semaphore-compat:base,
                        HTTP:base,

                      -- GHC 9.14's bundled exceptions-0.10.11 has broken template-haskell-inplace refs.
                      -- Force building from Hackage source instead.
                      source-repository-package
                        type: git
                        location: https://github.com/ekmett/exceptions.git
                        tag: v0.10.11
                        --sha256: sha256-kabkX239AgEdzcDfQMJQF7gOgZSrv3AqqCMOf0o0thQ=
                    '';
                    # hlint = {};
                    # haskell-language-server = {};
                  };
                  buildInputs = with pkgs; [
                    pkg-config
                  ];
                  withHoogle = false;
                  # Point to source data directory for development
                  shellHook = ''
                    export NIX_DATA_DIR="$PWD/data"
                  '';
                };
                modules = [{
                  contentAddressed = true;
                  ghcOptions = [ "-fobject-determinism" ];
                  # enableLibraryProfiling = true;
                  # profilingDetail = "none";
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
      in flake // {
        legacyPackages = pkgs;
        packages.default = flake.packages."hnix:exe:hnix";
      });
}

