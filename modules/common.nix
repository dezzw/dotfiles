{ inputs, username, lib, pkgs, ... }: {
  programs.zsh = {
    enable = true;
    enableCompletion = true;
    enableBashCompletion = true;
  };

  # environment setup
  environment = {
    systemPackages = [ pkgs.cachix ];
    etc = {
      home-manager.source = "${inputs.home-manager}";
      unstable.source = "${inputs.unstable}";
      stable.source = "${inputs.stable}";
    };
    # list of acceptable shells in /etc/shells
    shells = with pkgs; [ bash zsh ];
    pathsToLink = [ "/libexec" ];
  };

}
