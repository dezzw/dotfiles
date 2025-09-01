{
  inputs,
  username,
  lib,
  pkgs,
  ...
}:
{
  # environment setup
  environment = {
    systemPackages = [ ];
    etc = {
      home-manager.source = "${inputs.home-manager}";
      unstable.source = "${inputs.unstable}";
    };
    # list of acceptable shells in /etc/shells
    shells = with pkgs; [
      fish
      bash
      zsh
    ];
    pathsToLink = [ "/libexec" ];
  };

  users.users."dez" = {
    shell = pkgs.fish;
    ignoreShellProgramCheck = true;
  };

}
