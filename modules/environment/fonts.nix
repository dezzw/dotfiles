# Font configuration (platform-specific implementation)

{ pkgs, lib, ... }:

{
  # Linux: Use fontconfig
  fonts.fontconfig.enable = lib.mkIf pkgs.stdenv.hostPlatform.isLinux true;

  # Darwin: Fonts are managed via nix-darwin in modules/darwin/core.nix
}