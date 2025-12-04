# All program configurations

{ ... }:

{
  imports = [
    ./git.nix
    ./ssh.nix
    ../tools
    ../shell
  ];

  # Additional programs
  programs.nix-index.enable = true;
  programs.gpg.enable = true;
  programs.lazygit.enable = true;
}