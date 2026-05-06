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
  programs = {
    # nix-index.enable = true;
    nix-index-database.comma.enable = true;

    gpg.enable = true;
    lazygit.enable = true;
  };
}
