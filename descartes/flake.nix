{
  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-unstable";
    ocaml-overlay = {
      url = "github:nix-ocaml/nix-overlays";
      inputs.nixpkgs.follows = "nixpkgs";
    };
  };

  outputs = { nixpkgs, ocaml-overlay, ... }:
    let
      system = "x86_64-linux";
      pkgs = import nixpkgs {
        inherit system;
        overlays = [
          ocaml-overlay.overlays.default
        ];
      };
    in
    {
      devShell.${system} = pkgs.mkShell {
        buildInputs = with pkgs; [
          opam
          ocaml
          dune_3
        ] ++ (with pkgs.ocamlPackages; [
          ocaml-lsp
          ocamlformat_0_26_2
          utop
          ppx_deriving
        ]);
      };
    };
}
