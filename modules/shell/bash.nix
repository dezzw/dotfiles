# Bash shell configuration (Linux-specific)
# Kept minimal: WSL login shim only; interactive use hands off to zsh.

{ pkgs, lib, ... }:

{
  programs.bash = lib.mkIf pkgs.stdenv.isLinux {
    enable = true;
  };
}
