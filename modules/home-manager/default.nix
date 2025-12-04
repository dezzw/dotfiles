# Main home-manager configuration
# Composes modules with platform-specific overrides

{
  inputs,
  config,
  pkgs,
  username,
  lib,
  homeDirectory ? if pkgs.stdenv.isDarwin then "/Users/${username}" else "/home/${username}",
  ...
}:

{
  programs.home-manager.enable = true;
  home.enableNixpkgsReleaseCheck = lib.mkDefault false;

  home = {
    inherit username homeDirectory;
    stateVersion = "25.11";
  };

  # Import modules
  imports = [
    ./packages.nix
    ../programs
    ../environment
    # Editor configurations (keep existing structure for now)
    ../emacs
    ../neovim
    ../zellij
    # Platform-specific dotfiles (moved to conditional below to avoid recursion)
  ];

  # Platform-specific dotfiles
  home.file = lib.optionalAttrs pkgs.stdenv.isDarwin {
    ".config/wezterm/".source = ./dotfiles/wezterm;
    ".config/wezterm/".recursive = true;
  };
}