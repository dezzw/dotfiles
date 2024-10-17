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
    basedpyright
    pyright
    vscode-langservers-extracted
    typescript-language-server
    bash-language-server
    
    # clojure-lsp
    neil
    clj-kondo
    
    nixd

    texlab
    
    universal-ctags
    
    # Code Formating
    nixfmt-rfc-style

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
