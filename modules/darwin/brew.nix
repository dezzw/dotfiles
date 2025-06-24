{ config, pkgs, ... }:

{
  homebrew = {

    enable = true;

    global = {
      brewfile = true;
    };

    onActivation = {
      autoUpdate = true;
      upgrade = true;
      cleanup = "zap";
    };

    taps = [
      "homebrew/cask-versions"
      "homebrew/cask-fonts"
      "clojure-lsp/brew"
      "d12frosted/emacs-plus"
      "mrkai77/cask"
    ];

    brews = [
      "clojure-lsp-native"
      "enchant"
      "php"
      "node"
      # "loop"
      "aider"
      "emacs-plus@31"
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
      "cursor"
      # "godot"
      "orbstack"
      "utm"
      "wezterm"
      "warp"
      "ollama"
      "chatgpt"

      # Applications
      "alfred"
      "discord"
      "downie"
      "iina"
      "zoom"
      "pearcleaner"
      "cleanshot"
      "monitorcontrol"

      "wechat"
      "libreoffice"

      # gaming
      "steam"
      "minecraft"

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
