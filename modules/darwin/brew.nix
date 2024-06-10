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
      "mrkai77/cask"
    ];

    brews = [
    "clojure-lsp-native"
    "enchant"
    # "loop"
    ];

    casks = [
      # WM
      # "amethyst"
      "loop"

      # Browser
      "firefox@developer-edition"
      "arc"

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
      "dropbox"

      "wechat"

      # fonts
      "font-maple"
      "font-victor-mono-nerd-font"
      "font-fira-code-nerd-font"
      "font-jetbrains-mono-nerd-font"
      "font-hack-nerd-font"
    ];

    masApps = {
      Xcode = 497799835;
      Tailscale = 1475387142;
      "The Unarchiver" = 425424353;
      "Color Picker" = 1545870783;
      "Interactful" = 1528095640;
    };
  };
}
