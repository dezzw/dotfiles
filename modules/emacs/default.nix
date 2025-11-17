{ pkgs, lib, ... }:
let
  python = pkgs.python3.withPackages (
    p: with p; [
      epc
      orjson
      paramiko
      rapidfuzz
      setuptools
      sexpdata
      six
      watchdog
      packaging
    ]
  );
  pythonForLspBridge = pkgs.runCommand "python-for-lsp-bridge" { } ''
    mkdir -p $out/bin
    ln -s ${python}/bin/python $out/bin/python-for-lsp-bridge
  '';
in
{
  programs.emacs = {
    enable = true;
    package = pkgs.demacs;
  };

  home.packages =
    with pkgs;
    [

      emacs-lsp-booster

      global
      universal-ctags

      # Code Formating
      nixfmt-rfc-style

      # dirvish
      imagemagick
      ffmpegthumbnailer
      mediainfo

      (rustPlatform.buildRustPackage rec {
        pname = "devicon-lookup";
        version = "0.10.2";

        src = fetchFromGitHub {
          owner = "coreyja";
          repo = "devicon-lookup";
          rev = "v${version}";
          hash = "sha256-mDjRbBX3B1pfGX9SkrQLFXpgpq3Kay+crFXT1Bmfadk=";
        };

        cargoHash = "sha256-aewaNaeJLxRqm6p9K/GzHhJY3/b5z7N4Z8F7KjVxzcQ=";
      })

    (rustPlatform.buildRustPackage rec {
      pname = "emacs-lsp-proxy";
      version = "0.5.9";

      src = fetchFromGitHub {
        owner = "jadestrong";
        repo = "lsp-proxy";
        rev = "v${version}";
        hash = "sha256-dcS6XumwDNeF/+Js6w5JsdV/V/uG0f+w6C4oQmPnKGA=";
      };

      cargoHash = "sha256-yXj6MAaGBkVBD2F5kvWgwqnlCdL+8NDEtZB0K+SDzYU=";
    })

      # org-download
    ]
    ++ lib.optionals pkgs.stdenv.isDarwin [
      pngpaste
    ]
    ++ [

      # Spelling checking
      # enchant
      (aspellWithDicts (
        dicts: with dicts; [
          en
          en-computers
          en-science
        ]
      ))

      leetcode-cli

      emacs-all-the-icons-fonts

      gh

      pythonForLspBridge
    ];
}
