let
  pkgs = import <nixpkgs> {};
in
  pkgs.mkShell {
    packages = with pkgs; [
      ghc
      ormolu
      gcc
      gnumake
    ];
  }
