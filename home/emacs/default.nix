{ pkgs, ... }:

{
  programs.emacs = {
    enable = true;
    package = pkgs.emacs;
    extraPackages = epkgs: with epkgs;[
      vterm
      pdf-tools
    ];
  };


  home.packages = with pkgs; [
    # Language Server
    ccls

    nodePackages.pyright
    nodePackages.typescript-language-server
    nodePackages.vscode-langservers-extracted
    nodePackages.bash-language-server
    nodePackages.vscode-css-languageserver-bin
    nodePackages.vscode-html-languageserver-bin

    nodePackages.eslint
    nodePackages.prettier

    python39Packages.pylint
    
    rnix-lsp
    rPackages.languageserver

    universal-ctags
    
    # Code Formating
    nixfmt

    # dirvish
    imagemagick
    mediainfo

    # org-download
    pngpaste

    # Spelling checking
    enchant
  ];
}
