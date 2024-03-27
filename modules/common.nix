{ inputs, username, lib, pkgs, ... }: {
  programs.zsh = {
    enable = true;
    enableCompletion = true;
    enableBashCompletion = true;
  };

  # environment setup
  environment = {
    systemPackages = [ ];
    etc = {
      home-manager.source = "${inputs.home-manager}";
      unstable.source = "${inputs.unstable}";
    };
    # list of acceptable shells in /etc/shells
    shells = with pkgs; [ bash zsh ];
    pathsToLink = [ "/libexec" ];
  };

}
