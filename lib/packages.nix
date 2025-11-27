# Package composition helpers
# Provides functions to combine packages across platforms

{ lib, pkgs, ... }:

rec {
  # Combine package lists, handling platform-specific packages
  combinePackages = {
    common ? [ ],
    darwin ? [ ],
    linux ? [ ],
    nixos ? [ ],
  }:
    common
    ++ lib.optionals pkgs.stdenv.isDarwin darwin
    ++ lib.optionals pkgs.stdenv.isLinux linux
    ++ lib.optionals (pkgs.stdenv.isLinux && pkgs.stdenv.hostPlatform.isNixOS) nixos;

  # Helper to conditionally include packages based on platform
  whenDarwin = packages: lib.optionals pkgs.stdenv.isDarwin packages;
  whenLinux = packages: lib.optionals pkgs.stdenv.isLinux packages;
  whenNixOS = packages: lib.optionals (pkgs.stdenv.isLinux && pkgs.stdenv.hostPlatform.isNixOS) packages;
}