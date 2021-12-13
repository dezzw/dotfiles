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
    "google-chrome"
    "obs"
    "alacritty"
    "appcleaner"
    "kap"
    "soundflower"
  ];
}
