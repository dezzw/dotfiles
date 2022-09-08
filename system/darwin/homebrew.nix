{config, pkgs, ...}:

{
  homebrew = {
    
    enable = true;
    onActivation = {      
      autoUpdate = true;
      cleanup = "zap";
    };
    global = {
      
      brewfile = true;
      lockfiles = false;
    };
  };

  homebrew.taps = [
    "homebrew/core"
    "homebrew/cask"
    "homebrew/cask-versions"
    "homebrew/cask-fonts"
    # "koekeishiya/formulae"
    "adur1990/tap"
  ];

  homebrew.brews = [
    # "yabai"
    "python"
    "minimal-racket"
    # "roswell"
    "jenv"
    "jupyterlab"
    # "deno"

    "ical-buddy"
  ];

  homebrew.casks = [

    # WM
    # "amethyst"

    # Browser
    "brave-browser"
    "google-chrome"
    "firefox-developer-edition"

    # Graphic Tools
    "blender"
    "gimp"
    "obs"
    "kap"

    # Developing Tools
    "dash"
    "iterm2"
    "hyper"
    "jetbrains-toolbox"
    "visual-studio-code"
    "docker"
    "utm"

    # Applications
    "alfred"
    "discord"
    "downie"
    "iina"
    "zoom"
    "appcleaner"
    "ubersicht"
    "steam"
    "cleanshot"
    "bartender"
    
    # fonts
    "font-maple"
    "font-victor-mono-nerd-font"
    "font-fira-code-nerd-font"
    "font-jetbrains-mono-nerd-font"
  ];
}
