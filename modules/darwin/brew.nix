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
    "php"
    "node"
    # "loop"
    ];

    casks = [
      # WM
      # "amethyst"
      # "loop"
      "hammerspoon"

      # Browser
      "firefox@developer-edition"
      "google-chrome"
      # "arc"

      # Graphic Tools
      "blender"
      "gimp"
      "obs"

      # Developing Tools
      # "jetbrains-toolbox"
      "visual-studio-code"
      "godot"
      "zed"
      "orbstack"
      "wezterm"
      "ollama"

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
      "Microsoft Word" = 462054704;
      "Microsoft Excel" = 462058435;
    };
  };
}
