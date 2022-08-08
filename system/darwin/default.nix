{ config, pkgs, ... }:

{
  imports = [
    ./homebrew.nix
    #./wm.nix
  ];
  
  nix.package = pkgs.nixFlakes;
  nix.extraOptions = ''
    experimental-features = nix-command flakes
    auto-optimise-store = true
  '';

  system.stateVersion = 4;
  nix.maxJobs = "auto";
  services.nix-daemon.enable = true;

  nix.trustedUsers = [ "root" "dez" ];

  nix.binaryCaches = [
    "https://cachix.org/api/v1/cache/nix-community"

    "https://cachix.org/api/v1/cache/emacs"

    "https://cachix.org/api/v1/cache/cmacrae"
  ];

  nix.binaryCachePublicKeys = [
    "emacs.cachix.org-1:b1SMJNLY/mZF6GxQE+eDBeps7WnkT0Po55TAyzwOxTY="
    "nix-community.cachix.org-1:mB9FSh9qf2dCimDSUo8Zy7bkq5CX+/rkCWyvRCYg3Fs="
  ];

  nix.trustedBinaryCaches = config.nix.binaryCaches;

  nixpkgs.config.allowUnfree = true;
  # nixpkgs.config.allowBroken = true;

  environment.shells = [ pkgs.zsh ];
  environment.systemPackages = with pkgs;[
    gcc
    git
    man-pages
    man-pages-posix
  ];
  programs.bash.enable = false;

  users.users.dez.shell = pkgs.zsh;
  users.users.dez.home = "/Users/dez";
  users.nix.configureBuildUsers = true;

  programs.zsh = {
    enable = true;
    enableCompletion = false;
    interactiveShellInit = "autoload -U compinit && compinit";
  };

  # system.defaults = {
  #   dock = {
  #     autohide = true;
  #     mru-spaces = false;
  #     minimize-to-application = true;
  #   };

  #   # screencapture.location = "/tmp";

  #   finder = {
  #     AppleShowAllExtensions = true;
  #     _FXShowPosixPathInTitle = true;
  #     FXEnableExtensionChangeWarning = false;
  #   };

  #   trackpad = {
  #     Clicking = true;
  #     TrackpadThreeFingerDrag = true;
  #   };
  # };

  # system.keyboard = {
  #   enableKeyMapping = true;
  #   remapCapsLockToControl = true;
  # };
  
  fonts = {
    fontDir.enable = true; 
    fonts = with pkgs; [
      # victor-mono
      # fira-code
      # jetbrains-mono
      nerdfonts
      emacs-all-the-icons-fonts
    ];
  };

  # Recreate /run/current-system symlink after boot
  services.activate-system.enable = true;
}
