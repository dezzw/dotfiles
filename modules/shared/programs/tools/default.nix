# All tool program configurations

{ ... }:

{
  imports = [
    ./bat.nix
    ./fzf.nix
    ./direnv.nix
    ./starship.nix
    ./eza.nix
    ./yazi.nix
  ];
}