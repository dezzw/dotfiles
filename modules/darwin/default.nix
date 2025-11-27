# Main nix-darwin configuration
# Composes shared modules with darwin-specific settings

{ pkgs, ... }:

{
  imports = [
    ../common.nix
    ./core.nix
    ./brew.nix
    # ./preferences.nix
  ];
}