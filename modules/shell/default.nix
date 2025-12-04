# All shell configurations

{ ... }:

{
  imports = [
    ./fish.nix
    ./zsh.nix
    ./bash.nix
  ];
}