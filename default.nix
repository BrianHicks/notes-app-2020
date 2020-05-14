{ ... }:
let
  sources = import ./nix/sources.nix;
  nixpkgs = import sources.nixpkgs { };
  gitignore = import sources.gitignore { };
in with nixpkgs;
stdenv.mkDerivation {
  name = "notes";
  src = gitignore.gitignoreSource ./.;

  buildInputs = [ elmPackages.elm ninja nodePackages.uglify-js ];
  buildPhase = ''
    ./script/ninja.sh > build.ninja
    env ELM_HOME=.elm ELM_FLAGS= ninja
  '';

  installPhase = "cp -r dist/ $out/";
}
