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
    
    nil
    nixd

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
    (aspellWithDicts (dicts: with dicts; [en en-computers en-science]))

    emacs-all-the-icons-fonts
  ];
}
