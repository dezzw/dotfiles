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
    };
  };

  homebrew.taps = [
    "homebrew/cask-versions"
    "homebrew/cask-fonts"
  ];

  homebrew.brews = [
  ];

  homebrew.casks = [
    # WM
    # "amethyst"

    # Browser
    "google-chrome"
    "firefox-developer-edition"
    "microsoft-edge"
    "arc"

    # Graphic Tools
    "blender"
    "gimp"
    "obs"

    # Developing Tools
    "dash"
    "iterm2"
    "jetbrains-toolbox"
    "visual-studio-code"
    "godot"
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

    # fonts
    "font-maple"
    "font-victor-mono-nerd-font"
    "font-fira-code-nerd-font"
    "font-jetbrains-mono-nerd-font"
    "font-hack-nerd-font"
  ];
}
