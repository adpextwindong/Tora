let
  nixpkgsPin = {
    url = https://github.com/NixOS/nixpkgs/archive/8c03897e2622f4bc8060370736df431d5942522c.tar.gz;
  };
  pkgs = import (builtins.fetchTarball nixpkgsPin) {};
in

pkgs.stdenv.mkDerivation rec {
  name = "Tora-Compiler";
  src = ./.;
  buildInputs = with pkgs; with haskellPackages; [
    haskell.compiler.ghc8107
    cabal-install
    hlint
    alex
    happy
  ];
}
