{ pkgs, ... }:

(
  pkgs.emacsWithPackagesFromUsePackage {
    alwaysEnsure = true;
    alwaysTangle = true;

    # Custom overlay derived from 'emacs' flake input
    package = pkgs.emacs;
    config = ../Emacs/demacs.org;

    extraEmacsPackages = epkgs: with epkgs;[
      use-package
      doom-themes
      doom-modeline
      dashboard
      page-break-lines
      exec-path-from-shell
      auctex
      posframe
    ];
  }
)
