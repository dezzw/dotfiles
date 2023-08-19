{ pkgs, ... }:
let
  demacs = (pkgs.emacs-unstable.override { withNativeCompilation = true; withGTK3 = true; withSQLite3 = true; withWebP = true; }).overrideAttrs (old: {
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
    package = demacs;
    extraPackages = epkgs: with epkgs;[
      vterm
      pdf-tools
      jinx
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
  ];
}
