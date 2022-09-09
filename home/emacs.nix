{ pkgs, ... }:

{
  programs.emacs = {
    enable = true;
    # package = pkgs.emacsPgtkNativeComp; # emacs 29.0.5 native compiled
    # package = pkgs.emacs28NativeComp; # emacs 28.1 native compiled
    package = pkgs.emacs;
    extraPackages = epkgs: with epkgs;[
      vterm
      pdf-tools
      zmq
      exec-path-from-shell
      esup
    ];
  };

  # home.file.".emacs-profiles.el".text = ''
  #   (("default" . ((user-emacs-directory . "~/.dotfiles/Emacs/emacs-configs/demacs")))
  #    ("doom" . ((user-emacs-directory . "~/.dotfiles/Emacs/emacs-configs/doom-core")
  #     ;;(env . (("DOOMDIR" . "~/.dotfiles/Emacs/emacs-configs/doom")))))
  #   ))
  #    ("beta" . ((user-emacs-directory . "~/.dotfiles/Emacs/emacs-configs/beta_emacs")))
  #   )
  # '';

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
    python39Packages.pdf2image

    # For flyspell (spelling checking)
    hunspell
    hunspellDicts.en-us
    hunspellDicts.en-ca   
  ];
}
