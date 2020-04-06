{ ... }:
let
  sources = import ./nix/sources.nix;
  nixpkgs = import sources.nixpkgs { };
  niv = import sources.niv { };
in with nixpkgs;
stdenv.mkDerivation {
  name = "notes";
  buildInputs = [
    niv.niv
    git

    # building
    nodejs-12_x
    nodePackages.npm
  ];
}
