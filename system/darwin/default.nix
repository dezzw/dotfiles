{ config, pkgs, ... }:

{
  imports = [
    ./homebrew.nix
  ];
  
  nix = {
    package = pkgs.nixVersions.stable;
    extraOptions = ''
      experimental-features = nix-command flakes
      auto-optimise-store = true
    '';

    configureBuildUsers = true;
    
    settings = {

      trusted-users = [ "root" "dez" ];

      max-jobs = "auto";
      
      substituters = [
        "https://cachix.org/api/v1/cache/nix-community"

        "https://cachix.org/api/v1/cache/emacs"

        "https://cachix.org/api/v1/cache/cmacrae"
      ];

      trusted-public-keys = [
        "emacs.cachix.org-1:b1SMJNLY/mZF6GxQE+eDBeps7WnkT0Po55TAyzwOxTY="
        "nix-community.cachix.org-1:mB9FSh9qf2dCimDSUo8Zy7bkq5CX+/rkCWyvRCYg3Fs="
      ];
    };
    
  };

  system.stateVersion = 4;
  services.nix-daemon.enable = true;

  nixpkgs.config.allowUnfree = true;
  # nixpkgs.config.allowBroken = true;

  environment.shells = [ pkgs.zsh ];
  environment.systemPackages = with pkgs;[
    gcc
    git
    man-pages
    man-pages-posix
  ];
  programs.bash.enable = true;

  users.users.dez.shell = pkgs.zsh;
  users.users.dez.home = "/Users/dez";

  programs.zsh = {
    enable = true;
    enableCompletion = false;
    interactiveShellInit = "autoload -U compinit && compinit";
  };

  services.lorri.enable = true;

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
  
  # fonts = {
  #   fontDir.enable = true; 
  #   fonts = with pkgs; [
  #     victor-mono
  #     fira-code
  #     jetbrains-mono
  #     emacs-all-the-icons-fonts
  #   ];
  # };

  # Recreate /run/current-system symlink after boot
  services.activate-system.enable = true;
}
