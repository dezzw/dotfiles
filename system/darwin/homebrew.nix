{config, pkgs, ...}:

{
  homebrew = {
    
    enable = true;
    onActivation = {      
      upgrade = true;
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
    "koekeishiya/formulae"
    "FelixKratz/formulae"
    "adur1990/tap"
    "homebrew/services"
  ];

  homebrew.brews = [
   # {
   #   name = "yabai";
   #   restart_service = true;
   # }
    #{
    #  name = "sketchybar";
    #  restart_service = true;
    #}

    #"python"
    # "racket"
    # "jupyterlab"
    "ical-buddy"
  ];

  homebrew.casks = [

    # WM
    # "amethyst"

    # Browser
    "google-chrome"
    "firefox-developer-edition"
    "microsoft-edge-dev"

    # Graphic Tools
    "blender"
    "gimp"
    "obs"
    "figma"

    # Developing Tools
    "dash"
    "iterm2"
    "xquartz"
    "jetbrains-toolbox"
    "visual-studio-code"
    "docker"
    "utm"
    "postman"
    "gitkraken"

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
    "font-hack-nerd-font"
  ];
}
