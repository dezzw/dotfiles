{
  config,
  pkgs,
  lib,
  ...
}:
let
  hostName = config.networking.hostName or "";
  isMac = hostName == "pro";
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

    ]
    ++ lib.optionals isMac [
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
      # "godot"
      "ghostty"
      "chatgpt"

      # Applications
      "alfred"
      "discord"
      "iina"
      "zoom"
      "pearcleaner"
      "monitorcontrol"

      "wechat"
      "libreoffice"

      # gaming
      "steam"
      "minecraft"
    ]
    ++ lib.optionals isMini [
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
    };
  };
}
