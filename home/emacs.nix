{ pkgs, ... }:

{
  programs.emacs = {
    enable = true;
    package = pkgs.emacs;
    extraPackages = epkgs: with epkgs;[
      setup
      use-package
      doom-themes
      doom-modeline
      hide-mode-line
      mini-modeline
      dashboard
      page-break-lines
      exec-path-from-shell
      auctex
      posframe
      gcmh
      super-save
      nyan-mode
      hl-todo
      highlight-numbers
      diff-hl
      beacon
      perspective
      ace-window
      dired-single
      all-the-icons-dired
      dired-hide-dotfiles
      diredfl
      dired-posframe
      which-key
      meow
      hungry-delete
      evil-nerd-commenter
      avy
      org
      org-superstar
      visual-fill-column
      valign
      org-appear
      org-roam
      org-super-agenda
      markdown-mode
      edit-indirect
      projectile
      company
      company-tabnine
      company-box
      company-quickhelp
      company-statistics
      vertico
      orderless
      corfu
      consult
      consult-dir
      marginalia
      embark
      embark-consult
      helpful
      smartparens
      rainbow-delimiters
      rainbow-mode
      highlight-indent-guides
      aggressive-indent
      format-all
      quickrun
      minimap
      treemacs
      undo-tree
      lsp-pyright
      typescript-mode
      js2-mode
      rjsx-mode
      coffee-mode
      sourcemap
      flymake-coffee
      web-mode
      skewer-mode
      emmet-mode
      scss-mode
      json-mode
      ccls
      modern-cpp-font-lock
      cmake-mode
      lsp-java
      cdlatex
      xenops
      yaml-mode
      ess
      slime
      nix-mode
      nixos-options
      nix-sandbox
      nix-update
      nixpkgs-fmt
      lsp-mode
      lsp-ui
      lsp-treemacs
      dap-mode
      flycheck
      yasnippet
      yasnippet-snippets
      direnv
      vterm
      multi-vterm
      eshell-prompt-extras
      eshell-info-banner
      eshell-up
      eshell-syntax-highlighting
      esh-autosuggest
      esh-buf-stack
      eshell-vterm
      shell-pop
      emamux
      magit
      magit-delta
      cliphist
      osx-trash
      password-store

      popper
      dirvish
      cape
      
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

    ispell

    nixpkgs-fmt

    # dirvish
    imagemagick
  ];
}
