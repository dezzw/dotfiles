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
    "adur1990/tap"
  ];

  homebrew.brews = [
    # "yabai"
    "python"
    "minimal-racket"
    "roswell"
    "jupyterlab"

  ];

  homebrew.casks = [

    # WM
    # "amethyst"

    # Browser
    "brave-browser"
    "google-chrome"

    # Pass
    "passformacos"

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
    "cleanshot"
    "bartender"
  ];
}
