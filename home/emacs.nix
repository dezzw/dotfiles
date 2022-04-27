{ pkgs, ... }:

{
  programs.emacs = {
    enable = true;
    package = pkgs.emacsPgtkGcc;
    extraPackages = epkgs: with epkgs;[
      vterm
      pdf-tools
    ];
  };

  home.file.".emacs-profiles.el".text = ''
    (("default" . ((user-emacs-directory . "~/.dotfiles/Emacs/emacs-configs/demacs")))
     ("doom" . ((user-emacs-directory . "~/.dotfiles/Emacs/emacs-configs/doom-core")
      ;;(env . (("DOOMDIR" . "~/.dotfiles/Emacs/emacs-configs/doom")))))
    ))
     ("beta" . ((user-emacs-directory . "~/.dotfiles/Emacs/emacs-configs/beta_emacs")))
    )
  '';

  home.packages = with pkgs; [
    # Language Server
    ccls

    nodePackages.pyright
    nodePackages.typescript-language-server
    nodePackages.bash-language-server
    nodePackages.vscode-css-languageserver-bin
    nodePackages.vscode-html-languageserver-bin


    python39Packages.pylint
    nodePackages.eslint

    rnix-lsp

    universal-ctags

    # For flyspell (spelling checking)
    aspell
    aspellDicts.en
    aspellDicts.en-computers
    aspellDicts.en-science
    
    # Code Formating
    nixpkgs-fmt

    # dirvish
    imagemagick
    mediainfo
    python39Packages.pdf2image
    
  ];
}
