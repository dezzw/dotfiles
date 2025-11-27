# Main home-manager configuration
# Composes shared modules with platform-specific overrides

{
  inputs,
  config,
  pkgs,
  username,
  lib,
  homeDirectory ? if pkgs.stdenv.isDarwin then "/Users/${username}" else "/home/${username}",
  ...
}:

{
  programs.home-manager.enable = true;
  home.enableNixpkgsReleaseCheck = lib.mkDefault false;

  home = {
    inherit username homeDirectory;
    stateVersion = "25.11";
  };

  # Import shared modules
  imports = [
    ./packages.nix
    ../shared/programs
    ../shared/environment
    ../shared/fonts.nix
    # Editor configurations (keep existing structure for now)
    ../emacs
    ../neovim
    ../zellij
    # Platform-specific dotfiles (moved to conditional below to avoid recursion)
  ];

  # Platform-specific dotfiles
  home.file = lib.optionalAttrs pkgs.stdenv.isDarwin {
    ".config/wezterm/".source = ./dotfiles/wezterm;
    ".config/wezterm/".recursive = true;
  };

  # Linux-specific: Bash configuration with work environment
  # (This is work-specific, could be moved to a host-specific config)
  programs.bash = lib.mkIf pkgs.stdenv.isLinux {
    enable = true;
    shellAliases = {
      ls = "eza";
      ll = "eza -l";
      la = "eza -a";
      cd = "z";
      python = "python3";
    };

    bashrcExtra = ''
      # Work environment variables
      export XTENSA_SYSTEM=/opt/xtensa/registry
      export LD_LIBRARY_PATH=/usr/local/lib
      export LM_LICENSE_FILE=27001@10.0.10.168
      export XTENSA_CORE=quadra_cpu_prod
      export PATH="$PATH:/opt/xtensa/XtDevTools/install/tools/RI-2019.1-linux/XtensaTools/bin/"
      export PATH="$PATH:$HOME/.cargo/bin/"
      export USER="desmond.wang"
      export LOGNAME="desmond.wang"
    '';
  };
}