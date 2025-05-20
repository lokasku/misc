{
  inputs.nixpkgs.url = "github:nixOS/nixpkgs";

  inputs.ocaml-overlay.url = "github:nix-ocaml/nix-overlays";
  inputs.ocaml-overlay.inputs.nixpkgs.follows = "nixpkgs";


  outputs = { self, nixpkgs, ocaml-overlay }:
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
          nodejs_21
        
          ocamlPackages.findlib
          ocamlPackages.ocaml-lsp
          ocamlPackages.dream
          ocamlPackages.dream-html
          ocamlPackages.ocamlformat_0_26_0

          nodePackages_latest.tailwindcss
        ];
      };
    };
}
