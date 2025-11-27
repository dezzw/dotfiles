# NixOS module for system-wide Nix configuration
# This applies the shared Nix settings from nix-config.nix

{ config, lib, ... }:

(import ../../nix-config.nix).nixosModule { inherit config lib; }