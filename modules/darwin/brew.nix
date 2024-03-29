{ config, pkgs, ... }:

{
  homebrew = {

    enable = true;

    global = { brewfile = true; };

    onActivation = {
      autoUpdate = true;
      upgrade = true;
      cleanup = "zap";
    };

    taps = [ 
      "homebrew/cask-versions"
      "homebrew/cask-fonts"
      "clojure-lsp/brew"
    ];

    brews = [
    "clojure-lsp-native" 
    ];

    casks = [
      # WM
      # "amethyst"

      # Browser
      "google-chrome"
      "firefox-developer-edition"
      "microsoft-edge"
      "arc"
      "tor-browser"

      # Graphic Tools
      "blender"
      "gimp"
      "obs"

      # Developing Tools
      "iterm2"
      "jetbrains-toolbox"
      "visual-studio-code"
      "godot"
      "zed"
      "orbstack"

      # Applications
      "alfred"
      "discord"
      "downie"
      "iina"
      "zoom"
      "appcleaner"
      "steam"
      "cleanshot"
      "bartender"
      "crossover"
      "monitorcontrol"

      # fonts
      "font-maple"
      "font-victor-mono-nerd-font"
      "font-fira-code-nerd-font"
      "font-jetbrains-mono-nerd-font"
      "font-hack-nerd-font"
    ];

    masApps = { Xcode = 497799835; };
  };
}
