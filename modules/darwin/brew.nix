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
      "aider"
    ];

    casks = [
      # WM
      # "amethyst"
      # "loop"
      "hammerspoon"
      "jordanbaird-ice"
      
      # Browser
      "firefox@developer-edition"
      "google-chrome"
      # "arc"

      # Graphic Tools
      "blender"
      "magicavoxel"
      # "gimp"
      "obs"

      # Developing Tools
      # "jetbrains-toolbox"
      "visual-studio-code"
      # "godot"
      "zed"
      "orbstack"
      "wezterm"
      "ollama"
      "chatgpt"

      # Applications
      "alfred"
      "discord"
      "downie"
      "iina"
      "zoom"
      "appcleaner"
      "cleanshot"
      "crossover"
      "monitorcontrol"

      "wechat"
      "libreoffice"

      # gaming
      "steam"
      "whisky"
      
      # fonts
      "font-maple-mono-nf-cn"
      # "font-victor-mono-nerd-font"
      "font-fira-code-nerd-font"
      "font-jetbrains-mono-nerd-font"
      # "font-hack-nerd-font"
      "font-monaspace-nerd-font"
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
