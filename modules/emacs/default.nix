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
    package = pkgs.demacs-master;
  };

  home.packages =
    with pkgs;
    [

      emacs-lsp-booster

      # global
      universal-ctags

      # Code Formating
      nixfmt

      # dirvish
      vips
      imagemagick
      ffmpegthumbnailer
      mediainfo

      # Custom Rust packages (from overlay)
      devicon-lookup
      # Note: emacs-lsp-proxy is now built as part of the Emacs configuration

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
