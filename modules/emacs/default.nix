{ pkgs, ... }:

{

  programs.emacs = {
    enable = true;
    package = pkgs.demacs;
  };
  
  home.packages = with pkgs; [

    emacs-lsp-booster 

    # Language Server
    ccls

    ruff-lsp
    nodePackages.pyright
    nodePackages.typescript-language-server
    nodePackages.vscode-langservers-extracted
    nodePackages.bash-language-server
    nodePackages.vscode-css-languageserver-bin
    nodePackages.vscode-html-languageserver-bin
    nodePackages.svelte-language-server

    nodePackages.eslint
    
    # clojure-lsp
    neil
    clj-kondo
    
    rnix-lsp
    nil

    texlab

    zls
    
    universal-ctags
    
    # Code Formating
    nixfmt

    # dirvish
    imagemagick
    ffmpegthumbnailer
    mediainfo

    # org-download
    pngpaste

    # Spelling checking
    # enchant

    emacs-all-the-icons-fonts
  ];
}
