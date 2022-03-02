{config, pkgs, ...}:

{
  homebrew.enable = true;
  homebrew.autoUpdate = true;
  homebrew.cleanup = "zap";
  homebrew.global.brewfile = true;
  homebrew.global.noLock = true;


  homebrew.taps = [
    "homebrew/core"
    "homebrew/cask"
  ];

  homebrew.brews = [
    "python"
    "minimal-racket"
  ];

  homebrew.casks = [

    # WM
    # "amethyst"

    # Browser
    "qutebrowser"
    "google-chrome"
    
    # Applications
    "alfred"
    "dash"
    "discord"
    "downie"
    "hazeover"
    "gimp"
    "iina"
    "iterm2"
    "jetbrains-toolbox"
    "visual-studio-code"
    "zoom"
    "obs"
    "appcleaner"
    "kap"
    "soundflower"
    "ubersicht"
    "steam"
    "macfuse"
    "docker"

    # layout 
    "programmer-dvorak"
  ];
}
