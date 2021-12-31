{pkgs, ...}:

{
  programs.emacs.enable = true;

  programs.emacs.package =
    (
      pkgs.emacsWithPackagesFromUsePackage {
        alwaysEnsure = true;
        alwaysTangle = true;

        # Custom overlay derived from 'emacs' flake input
        package = pkgs.emacs;
        config = ../Emacs/demacs-darwin.org;

        extraEmacsPackages = epkgs: with epkgs;[

        ];
      }
    );

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

    nodePackages.typescript-language-server
    nodePackages.bash-language-server
    nodePackages.vscode-css-languageserver-bin
    nodePackages.vscode-html-languageserver-bin

    rnix-lsp
    
    universal-ctags

    ispell
  ];
}
