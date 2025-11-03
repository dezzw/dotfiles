{
  pkgs,
  username,
  homeDirectory ? "/home/${username}",
  ...
}:
let
  fontPackages = import ../../common/fonts.nix pkgs;
in
{
  imports = [
    ../../emacs
    ../../zellij
    ../programs
  ];

  home = {
    inherit username homeDirectory;
    stateVersion = "25.11";

    sessionVariables = {
      EDITOR = "hx";
      COLORTERM = "truecolor";
      TERM = "xterm-256color";
      XDG_CURRENT_DESKTOP = "WSLG";
    };

    packages =
      with pkgs;
      [
        # System utilities
        htop
        git
        fzf
        fd
        ripgrep
      ]
      ++ fontPackages;
  };

  home.sessionPath = [
    "${homeDirectory}/.cargo/bin"
  ];

  fonts.fontconfig.enable = true;

  programs.home-manager.enable = true;

  programs.bash = {
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
