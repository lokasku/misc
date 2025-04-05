{
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

        lisek = naersk-lib.buildPackage {
          name = "lisek";
          src = ./.;
        };
      in {
        packages.lisek = lisek;
        defaultPackage = self.packages.${system}.lisek;

        devShell = pkgs.mkShell rec {
          packages = with pkgs; [ toolchain ];
          RUST_BACKTRACE = 0;
          RUSTFLAGS = "-Zmacro-backtrace";
        };
      });
}
