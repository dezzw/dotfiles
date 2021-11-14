{ config, pkgs, ... }:

{
  nix.package = pkgs.nixFlakes;
  nix.extraOptions = ''
    experimental-features = nix-command flakes
    keep-derivations = true
    keep-outputs = true
  '';

  nix.binaryCaches = [
    "https://cachix.org/api/v1/cache/nix-community"

    "https://cachix.org/api/v1/cache/emacs"
  ];

  nix.binaryCachePublicKeys = [
    "emacs.cachix.org-1:b1SMJNLY/mZF6GxQE+eDBeps7WnkT0Po55TAyzwOxTY="
    "nix-community.cachix.org-1:mB9FSh9qf2dCimDSUo8Zy7bkq5CX+/rkCWyvRCYg3Fs="
  ];

  nix.trustedBinaryCaches = config.nix.binaryCaches;

  nixpkgs.overlays = [
    (import ../overlays)
  ];


  # Used for backwards compatibility, please read the changelog before changing.
  # $ darwin-rebuild changelog
  system.stateVersion = 4;

  nix.trustedUsers = [ "root" "dez" ];
  users.users.dez.home = "/Users/dez";

  # List packages installed in system profile. To search by name, run:
  # $ nix-env -qaP | grep wget
  environment.shells = [ pkgs.zsh ];
  environment.systemPackages = [ pkgs.zsh pkgs.gcc pkgs.git ];
  programs.bash.enable = true;

  # Create /etc/bashrc that loads the nix-darwin environment.
  programs.zsh = {
    enable = true;
    enableCompletion = false;
    interactiveShellInit = "autoload -U compinit && compinit";
  };
  # programs.fish.enable = true;

  environment.variables.EDITOR = "nvim";

  # Use a custom configuration.nix location.
  # $ darwin-rebuild switch -I darwin-config=$HOME/.config/nixpkgs/darwin/configuration.nix
  environment.darwinConfig = "$HOME/.dotfiles/system/macintosh.nix";

  # Auto upgrade nix package and the daemon service.
  services.nix-daemon.enable = true;
  # nix.package = pkgs.nix;


  fonts.enableFontDir = true;
  fonts.fonts = with pkgs; [ cantarell-fonts roboto roboto-mono mononoki emacs-all-the-icons-fonts ];

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
    "spotify"
    "google-chrome"
    "obs"
    "alacritty"
    "appcleaner"
  ];
}