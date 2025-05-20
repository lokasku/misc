{
  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs";
    flake-utils.url = "github:numtide/flake-utils";
  };

  outputs = { self, nixpkgs, flake-utils }:
    flake-utils.lib.eachDefaultSystem (system:
      let
        pkgs = nixpkgs.legacyPackages.${system};

        haskellPackages = pkgs.haskell.packages.ghc924;

        jailbreakUnbreak = pkg:
          pkgs.haskell.lib.doJailbreak (pkg.overrideAttrs (_: { meta = { }; }));

        packageName = "website";
      in {
        packages.${packageName} = haskellPackages.callCabal2nix packageName self {};

        defaultPackage = self.packages.${system}.${packageName};

        devShell = pkgs.mkShell {
          buildInputs = with haskellPackages; [
            cabal-install
            cabal-plan
            ghcid
            haskell-language-server
          ];
          packages = with pkgs; [
            zlib
            pkg-config
          ];
          inputsFrom = builtins.attrValues self.packages.${system};
        };
      });
}
