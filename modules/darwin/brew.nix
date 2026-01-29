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
      "php"
      "node"

      "mole"
    ];

    casks = [
      # common
      "visual-studio-code"
      "cursor"
      "orbstack"
      "utm"
      "wezterm"
      "ollama"
      "downie"      
      "monitorcontrol"
      "wechat"

    ]
    ++ lib.optionals isMac [
      # WM
      # "amethyst"
      # "loop"
      "hammerspoon"
      "jordanbaird-ice"

      # Browser
      "google-chrome"

      # Graphic Tools
      # "blender"
      # "magicavoxel"
      # "gimp"
      "obs"

      # Developing Tools
      # "godot"
      "chatgpt"

      # Applications
      "alfred"
      "discord"
      "iina"

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
    };
  };
}
