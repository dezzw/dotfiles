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
    # "homebrew/services"
    # "koekeishiya/formulae"
  ];

  homebrew.brews = [
    # "yabai"
    "python"
    "minimal-racket"
  ];

  homebrew.casks = [

    # WM
    # "amethyst"

    # Browser
    "brave-browser"
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
    "ubersicht"
    "steam"
    "docker"
  ];
}
