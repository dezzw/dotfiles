{ pkgs, ... }:

{

  programs.emacs = {
    enable = true;
    package = pkgs.demacs;
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

    python39Packages.pylint
    
    # rnix-lsp
    nil
    
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

    material-design-icons
    emacs-all-the-icons-fonts

    maple-mono-NF
  ];
}
