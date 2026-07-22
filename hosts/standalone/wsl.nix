# WSL/Standalone Linux host configuration
# For non-NixOS Linux systems using standalone home-manager

{
  config,
  pkgs,
  lib,
  ...
}:

{
  # WSL uses /bin/bash from /etc/passwd and Nix zsh is not listed in
  # /etc/shells, so hand interactive bash sessions off to home-manager zsh.
  programs.bash.initExtra = lib.mkIf config.programs.zsh.enable ''
    if [[ -z "''${ZSH_VERSION:-}" ]]; then
      exec ${lib.getExe pkgs.zsh} -l
    fi
  '';

  home.sessionVariables = lib.mkIf config.programs.zsh.enable {
    SHELL = lib.getExe pkgs.zsh;
    XTENSA_SYSTEM = "/opt/xtensa/registry";
    LD_LIBRARY_PATH = "/usr/local/lib";
    LM_LICENSE_FILE = "27001@10.0.10.168";
    XTENSA_CORE = "quadra_cpu_prod";
    USER = "desmond.wang";
    LOGNAME = "desmond.wang";
  };

  home.sessionPath = [
    "/opt/xtensa/XtDevTools/install/tools/RI-2019.1-linux/XtensaTools/bin"
  ];
}
