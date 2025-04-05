{
  description = "GUI designed to create extremely customized cellular automata based on LENIA.";

  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs";
    flake-utils = {
      url = "github:numtide/flake-utils";
    };
    fenix = {
      url = "github:nix-community/fenix";
      inputs.nixpkgs.follows = "nixpkgs";
    };
    naersk = {
      url = "github:nmattia/naersk";
      inputs.nixpkgs.follows = "nixpkgs";
    };
  };

  outputs = { self, nixpkgs, fenix, naersk, flake-utils }:
    flake-utils.lib.eachDefaultSystem (system:
      let
        pkgs = import nixpkgs { inherit system; };

        toolchain = with fenix.packages.${system}; combine [
          complete.toolchain
        ];

        naersk-lib = naersk.lib.${system}.override {
          inherit (toolchain);
          rustc = toolchain;
          cargo = toolchain;
        };

        risk = naersk-lib.buildPackage {
          name = "lenia";
          src = ./.;
        };
      in {
        packages.risk = risk;
        defaultPackage = self.packages.${system}.risk;

        devShell = pkgs.mkShell rec {
          packages = with pkgs; [
            libxkbcommon
            libGL
            xorg.libXcursor
            xorg.libXrandr
            xorg.libXi
            xorg.libX11
            alsa-lib
          ] ++ [toolchain];
          LD_LIBRARY_PATH = pkgs.lib.makeLibraryPath packages;
          RUST_BACKTRACE = 0;
        };
      });
}
