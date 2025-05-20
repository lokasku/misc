# SPDX-FileCopyrightText: 2025 Aljebriq <143266740+aljebriq@users.noreply.github.com>
# SPDX-FileCopyrightText: 2025 Łukasz Bartkiewicz <lukasku@proton.me>
#
# SPDX-License-Identifier: GPL-3.0-only

{
  description = "The Lys purely functional programming language";

  inputs = {
    nixpkgs.url = "github:nix-ocaml/nix-overlays";
  };

  outputs =
    {
      self,
      nixpkgs,
      ...
    }:
    let
      systems = [
        "x86_64-linux"
        "aarch64-linux"
        "x86_64-darwin"
        "aarch64-darwin"
      ];
      forAllSystems = f: nixpkgs.lib.genAttrs systems (system: f nixpkgs.legacyPackages.${system});
    in
    rec {
      packages = forAllSystems (pkgs: rec {
        default = lys;
        lys = pkgs.ocamlPackages.buildDunePackage rec {
          pname = "lys";
          version = "0.0.0";
          src = self;
          strictDeps = false;
          stdenv = pkgs.fastStdenv;
          buildInputs = with pkgs.ocamlPackages; [
            sedlex
            menhir
            menhirLib
            ppx_deriving
            ppx_inline_test
            uuseg
            yojson
          ];
          buildPhase = ''
            runHook preBuild
            dune build --profile release -p ${pname} ''${enableParallelBuilding:+-j $NIX_BUILD_CORES}
            runHook postBuild
          '';
          checkPhase = ''
            runHook preCheck
            dune runtest --profile release -p ${pname} ''${enableParallelBuilding:+-j $NIX_BUILD_CORES}
            runHook postCheck
          '';
          installPhase = ''
            runHook preInstall
            dune install --profile release --prefix $out --libdir $OCAMLFIND_DESTDIR ${pname} --docdir $out/share/doc --mandir $out/share/man
            runHook postInstall
          '';
          meta = {
            description = "The Lys purely functional programming language";
            homepage = "https://github.com/lyslang/lys";
            license = with pkgs.lib.licenses; [
              gpl3Only
              cc-by-sa-40
              cc0
            ];
            maintainers = [
              {
                name = "Aljebriq";
                email = "143266740+aljebriq@users.noreply.github.com";
                github = "aljebriq";
                githubId = 143266740;
                keys = [
                  {
                    fingerprint = "D6A9 9003 3864 3E39 32FE  2720 2DA5 4C53 EEAB 3434";
                  }
                ];
              }
              {
                name = "Łukasz Bartkiewicz";
                email = "lukasku@proton.me";
                github = "lokasku";
                githubId = 105018247;
              }
            ];
            inherit (pkgs.ocaml.meta) platforms;
          };
        };
      });

      devShells = forAllSystems (pkgs: {
        default = pkgs.mkShell.override { stdenv = pkgs.fastStdenv; } {
          inputsFrom = pkgs.lib.attrValues packages.${pkgs.system};
          packages =
            with pkgs;
            [
              reuse
              just
              ocamlformat_0_27_0
            ]
            ++ lib.optionals stdenv.isDarwin (
              [
                llvm
              ]
              ++ (with darwin.apple_sdk.frameworks; [
                CoreServices
                Foundation
                Security
                SystemConfiguration
              ])
              ++ (with darwin; [
                libiconv
                Libsystem
                cctools
              ])
            )
            ++ (with ocamlPackages; [
              ocaml-lsp
              utop
            ]);

          shellHook = pkgs.lib.optionalString pkgs.stdenv.isDarwin (
            with pkgs.darwin;
            ''
              export LIBRARY_PATH=${Libsystem}/lib:$LIBRARY_PATH
              export CPATH=${Libsystem}/include:$CPATH
              export SDKROOT=$(xcrun --show-sdk-path)
              export MACOSX_DEPLOYMENT_TARGET=11.3
            ''
          );
        };
      });

      formatter = forAllSystems (pkgs: pkgs.nixfmt-rfc-style);
    };
}
