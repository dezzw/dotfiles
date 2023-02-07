{ pkgs, ... }:
let
  emacs = (pkgs.emacsGit.override { nativeComp = true; withXwidgets = true; withGTK3 = true; withSQLite3 = true; withWebP = true; }).overrideAttrs (old: {
    # https://github.com/cmacrae/emacs/blob/03b4223e56e10a6d88faa151c5804d30b8680cca/flake.nix#L75
    buildInputs = old.buildInputs ++ [ pkgs.darwin.apple_sdk.frameworks.WebKit ];
    patches =
      (old.patches or [])
      ++ [
        ./patches/fix-window-role.patch
        ./patches/poll.patch
        ./patches/round-undecorated-frame.patch
        ./patches/system-appearance.patch
      ];
  });
in
{
  programs.emacs = {
    enable = true;
    package = emacs;
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

    # For flyspell (spelling checking)
    (aspellWithDicts (dicts: with dicts; [ en en-computers en-science ]))
  ];
}
