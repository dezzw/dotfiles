{ pkgs, ... }:
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
  # programs.emacs = {
  #   enable = true;
  #   package = pkgs.demacs;
  # };

  home.packages = with pkgs; [

    emacs-lsp-booster

    # Language Server
    ccls

    ruff-lsp
    basedpyright
    # pyright
    vscode-langservers-extracted
    typescript-language-server
    bash-language-server

    # clojure-lsp
    neil
    clj-kondo
    
    nixd

    texlab

    lua-language-server
    fennel-ls
    
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

      useFetchCargoVendor = true;
      cargoHash = "sha256-aewaNaeJLxRqm6p9K/GzHhJY3/b5z7N4Z8F7KjVxzcQ=";})

    # org-download
    pngpaste

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

    pythonForLspBridge
  ];

  home.sessionVariables = {
    PATH = "/Applications/Emacs.app/Contents/MacOS/bin:$PATH";
  };
}
