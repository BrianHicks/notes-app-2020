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

    # building
    nodejs-12_x
    nodePackages.npm
  ] ++ lib.optionals stdenv.isDarwin darwinDeps;
}
