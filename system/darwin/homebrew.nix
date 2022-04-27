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

    # Graphic Tools
    "blender"
    "gimp"
    "obs"
    "kap"

    # Developing Tools
    "dash"
    "iterm2"
    "jetbrains-toolbox"
    "visual-studio-code"
    "docker"
    "utm"

    # Applications
    "alfred"
    "discord"
    "downie"
    "hazeover"
    "iina"
    "zoom"
    "appcleaner"
    "ubersicht"
    "steam"
  ];
}
