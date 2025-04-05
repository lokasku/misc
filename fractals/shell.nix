{ pkgs ? import <nixpkgs> {} }:

pkgs.mkShell {
  buildInputs = with pkgs; [
    python3
    python3Packages.numpy
    python3Packages.matplotlib
    python3Packages.pip

    xorg.libX11
    xorg.libXrender
    xorg.libXext
  ];

  shellHook = ''
    if [ ! -d "venv" ]; then
      python -m venv venv
    fi
    source venv/bin/activate

    pip install numpy matplotlib
  '';

  LD_LIBRARY_PATH = "${pkgs.xorg.libX11}/lib:${pkgs.xorg.libXrender}/lib:${pkgs.xorg.libXext}/lib";
}
