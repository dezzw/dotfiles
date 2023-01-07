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
    {
      name = "yabai";
      #args = [ "HEAD" ];
      restart_service = true;
    }
    {
      name = "skhd";
      restart_service = true;
    }
    {
      name = "sketchybar";
      restart_service = true;
    }

    "python"
    # "racket"
    # "jupyterlab"
    "ffmpegthumbnailer"
    "ical-buddy"
  ];

  homebrew.casks = [

    # WM
    # "amethyst"

    # Browser
    "google-chrome"
    "firefox-developer-edition"

    # Graphic Tools
    "blender"
    "gimp"
    "obs"

    # Developing Tools
    "racket"
    "dash"
    "iterm2"
    "xquartz"
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
    "spotify"
    
    # fonts
    "font-maple"
    "font-victor-mono-nerd-font"
    "font-fira-code-nerd-font"
    "font-jetbrains-mono-nerd-font"
  ];
}
