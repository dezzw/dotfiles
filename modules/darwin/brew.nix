{
  config,
  pkgs,
  lib,
  ...
}:
let
  hostName = builtins.getEnv "HOST";
  isMac = hostName == "MacBookPro";
  isMini = hostName == "mini";
in
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
      # common
      "visual-studio-code"
      "orbstack"
      "utm"
      "wezterm"
      "ollama"
      "downie"

      # fonts
      "font-maple-mono-nf-cn"
      # "font-victor-mono-nerd-font"
      "font-fira-code-nerd-font"
      "font-jetbrains-mono-nerd-font"
      # "font-hack-nerd-font"
      "font-monaspace-nerd-font"
    ]
    ++ lib.optional isMac [
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
      "cursor"
      # "godot"
      "chatgpt"

      # Applications
      "alfred"
      "discord"
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
    ]
    ++ lib.optional isMini [
      "plex-media-server"
    ];

    masApps = {
      Tailscale = 1475387142;
    }
    // lib.optionalAttrs isMac {
      Xcode = 497799835;
      "The Unarchiver" = 425424353;
      "Color Picker" = 1545870783;
      "Interactful" = 1528095640;
      "Microsoft Word" = 462054704;
      "Microsoft Excel" = 462058435;
    };
  };
}
