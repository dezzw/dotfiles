# Bash shell configuration (Linux-specific)

{ pkgs, lib, ... }:

{
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

  programs.zoxide = {
    enable = true;
    enableBashIntegration = true;
  };
}