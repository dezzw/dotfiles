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
    "google-chrome"
    "obs"
    "alacritty"
    "appcleaner"
    "kap"
    "soundflower"
    "ubersicht"
    "steam"
    "macfuse"
    "docker"
  ];
}
