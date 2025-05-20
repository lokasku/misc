open Dream_html
open HTML

let article : Article.article =
  {
    title = "Presentation";
    date = "April 21, 2024";
    edited = None;
    tags = [||];
    content =
      div []
        [
          p []
            [
              txt
                "This introductory article will cover my presentation, the way \
                 I do my computing, how this website is designed and finally \
                 how I intend to use it.";
            ];
          p []
            [
              txt
                "My name is Lokasku, I'm a 17-year-old French citizen with \
                 dual French and Polish nationality and I'm in 12th grade. My \
                 areas of interest are computing, mathematics and linguistics, \
                 but computing remains the predominant one. It turns out that \
                 I particularly like functional stuff, and this is \
                 characterized by my choice of languages and tools.";
            ];
          h2 [] [ txt "Computing" ];
          h3 [] [ txt "Hardware" ];
          ul []
            [
              li []
                [
                  txt
                    "I'm using a Rog Zephyrus 14\" laptop with a Ryzen 7 CPU \
                     (3.200 GHz), RTX 3050 Ti GPU, 512 GB SDD and 16 GB RAM.";
                ];
              li []
                [
                  txt "Sometimes I use the ";
                  a
                    [
                      href
                        "https://www.keychron.com/products/keychron-k2-wireless-mechanical-keyboard?variant=31063869685849";
                    ]
                    [ txt "Keychron K2 Wireless Mechanical Keyboard (v2)" ];
                  txt " with Gateron G Pro Blue switches and RGB backlight.";
                ];
            ];
          h3 [] [ txt "Software" ];
          h4 [] [ txt "Tools" ];
          ul []
            [
              li []
                [
                  a [ href "https://nixos.org" ] [ txt "NixOS" ];
                  txt
                    " as my main operating system (my configuration is \
                     available ";
                  a
                    [ href "https://github.com/Lokasku/nix-config" ]
                    [ txt "here" ];
                  txt ")";
                ];
              li []
                [
                  a [ href "https://github.com/lokasku/vide" ] [ txt "Vide" ];
                  txt " as my main IDE, and sometimes Emacs or VSCode.";
                ];
              li [] [ txt "Fish as my main shell." ];
              li [] [ txt "Alacritty as my terminal emulator." ];
              li []
                [
                  a [ href "https://hyprland.org/" ] [ txt "Hyprland" ];
                  txt " as my tilling window manager.";
                ];
            ];
          h4 [] [ txt "Languages" ];
          ul []
            [
              li [] [ txt "Python for educational or theoretical purposes." ];
              li []
                [
                  txt
                    "Rust and C for low-level or high-performance applications.";
                ];
              li []
                [
                  txt "OCaml and sometimes ";
                  a
                    [ href "https://www.scheme.org/" ]
                    [ txt "Scheme" ];
                  txt " (";
                  a [ href "https://racket-lang.org/" ] [ txt "Racket" ];
                  txt
                    " especially) for fun, and because in some applications they're more \
                     practical than other languages.";
                ];
            ];
          h2 [] [ txt "This website" ];
          h3 [] [ txt "Design" ];
          p []
            [
              txt "This website was written in OCaml with the ";
              a [ href "https://aantron.github.io/dream/" ] [ txt "Dream" ];
              txt
                " package because I found it less boring than doing it the \
                 conventional way, and because I find it fun. It's currently \
                 hosted on my raspberry PI 4 B with 64 GB memory and 16 GB \
                 RAM. It runs on ";
              a
                [
                  href
                    "https://hydra.nixos.org/job/nixos/trunk-combined/nixos.sd_image.aarch64-linux";
                ]
                [ txt "NixOS ARM" ];
              txt
                " (aarch64 in this case) in a Podman container configured with \
                 Nix. This server definitely has every conceivable quality.";
            ];
          h3 [] [ txt "Usage" ];
          p []
            [
              txt
                "I'll be posting articles on a variety of subjects of interest \
                 to me, including computer science, and sometimes to present \
                 and document my programming projects.";
            ];
          br [];
          p []
            [
              txt "This site is entirely free, you can find the source code ";
              a [ href "https://github.com/Lokasku/website" ] [ txt "here" ];
              txt ".";
            ];
        ];
  }
