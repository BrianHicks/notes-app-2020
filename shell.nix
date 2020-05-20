{ ... }:
let
  sources = import ./nix/sources.nix;
  nixpkgs = import sources.nixpkgs { };
  niv = import sources.niv { };

  # additional header files on macOS
  darwinDeps = [ nixpkgs.darwin.apple_sdk.frameworks.CoreServices ];
in with nixpkgs;
stdenv.mkDerivation {
  name = "notes";
  buildInputs = [
    niv.niv
    git

    # elm
    elmPackages.elm
    elmPackages.elm-format
    elmPackages.elm-test
    # elmPackages.elm-json
    # elmPackages.elm-review

    # js
    nodePackages.prettier

    # building
    ninja
    nodePackages.uglify-js

    # development
    devd
    modd
  ] ++ lib.optionals stdenv.isDarwin darwinDeps;
}
