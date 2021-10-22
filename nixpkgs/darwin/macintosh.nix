{ config, pkgs, ... }:

{
  nix.package = pkgs.nixFlakes;
  nix.extraOptions = ''
    experimental-features = nix-command flakes
  '';

  nix.binaryCaches = [
    "https://cachix.org/api/v1/cache/nix-community"
  ];

  # Used for backwards compatibility, please read the changelog before changing.
  # $ darwin-rebuild changelog
  system.stateVersion = 4;

  imports = [
    ./wm.nix
  ];

  nixpkgs.overlays = [
    (import ../overlays)
  ];

  nix.trustedUsers = [ "root" "dez" ];

  # List packages installed in system profile. To search by name, run:
  # $ nix-env -qaP | grep wget
  environment.systemPackages = with pkgs; [
    zsh
    vim
  ];

  environment.variables.EDITOR = "nvim";

  # Use a custom configuration.nix location.
  # $ darwin-rebuild switch -I darwin-config=$HOME/.config/nixpkgs/darwin/configuration.nix
  # environment.darwinConfig = "$HOME/.config/nixpkgs/darwin/configuration.nix";
  environment.darwinConfig = "$HOME/.config/nixpkgs/darwin/macintosh.nix";

  # Auto upgrade nix package and the daemon service.
  services.nix-daemon.enable = true;
  # nix.package = pkgs.nix;

  # Create /etc/bashrc that loads the nix-darwin environment.
  programs.zsh.enable = true;  # default shell on catalina
  # programs.fish.enable = true;

  fonts.enableFontDir = true;
  fonts.fonts = with pkgs; [ roboto roboto-mono ];

  homebrew.enable = true;
  homebrew.autoUpdate = true;
  homebrew.cleanup = "zap";
  homebrew.global.brewfile = true;
  homebrew.global.noLock = true;

  homebrew.taps = [
    "homebrew/core"
    "homebrew/cask"
    "homebrew/cask-fonts"
  ];

  homebrew.casks = [
    # fonts
    "font-fira-code"
    "font-hack-nerd-font"
    "font-jetbrains-mono"

    # Applications
    "alfred"
    "dash"
    "discord"
    "downie"
    "eclipse-java"
    "flycut"
    "hazeover"
    "gimp"
    "iina"
    "iterm2"
    "jetbrains-toolbox"
    "visual-studio-code"
    "zoom"
  ];
}
