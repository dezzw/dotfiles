{
  username,
  pkgs,
  ...
}:
{
  # environment setup
  environment = {
    systemPackages = [ ];
    # list of acceptable shells in /etc/shells
    shells = with pkgs; [
      fish
      bash
      zsh
    ];
    pathsToLink = [ "/libexec" ];
  };

  users.users."${username}" = {
    shell = pkgs.fish;
    ignoreShellProgramCheck = true;
  };

}
