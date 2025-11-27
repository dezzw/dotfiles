# Platform detection and composition helpers

{ lib, pkgs, ... }:

rec {
  # Platform detection
  isDarwin = pkgs.stdenv.isDarwin;
  isLinux = pkgs.stdenv.isLinux;
  isNixOS = pkgs.stdenv.isLinux && pkgs.stdenv.hostPlatform.isNixOS;
  isWSL = pkgs.stdenv.isLinux && !isNixOS;

  # Get home directory based on platform
  getHomeDirectory = username:
    if isDarwin then "/Users/${username}"
    else "/home/${username}";

  # Platform-specific session variables
  platformSessionVars = {
    linux = {
      XDG_CURRENT_DESKTOP = "WSLG";
    };
    darwin = { };
  };

  # Platform-specific PATH additions
  platformPaths = {
    darwin = [
      "/Applications/Xcode.app/Contents/Developer/usr/bin/"
      "/Applications/Emacs.app/Contents/MacOS/bin/"
    ];
    linux = [ ];
  };
}