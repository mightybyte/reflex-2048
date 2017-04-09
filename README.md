To build this project:

    deps/reflex-platform/work-on ./packages.nix ./.

When that is done running, it puts you in a nix-shell with all the
dependencies in place.  Then you can build with:

    cabal configure --ghcjs
    cabal build
