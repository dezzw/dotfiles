{
  inputs,
  config,
  pkgs,
  lib,
  ...
}:
{
  # Used for backwards compatibility, please read the changelog before changing.
  # $ darwin-rebuild changelog
  system.primaryUser = "dez";
  system.stateVersion = 5;
  # Default nixbld GID (hosts can override when their local install differs).
  ids.gids.nixbld = lib.mkDefault 350;

  users.users.dez.home = "/Users/dez";

  environment = {
    pathsToLink = [ "/Applications" ];
    # I exclusively control homebrew from here, but it's annoying to fully qualify the path to brew binaries
    systemPath = [ "/opt/homebrew/bin" ];
    etc = {
      darwin.source = "${inputs.darwin}";
    };

    systemPackages = with pkgs; [
      git
      curl
      coreutils
    ];

    # Fix "Too many open files" problems. Based on this:
    # https://medium.com/mindful-technology/too-many-open-files-limit-ulimit-on-mac-os-x-add0f1bfddde
    # Needs reboot to take effect
    # Changes default from 256 to 524,288 (probably a bigger jump than is really necessary)
    launchDaemons.ulimitMaxFiles = {
      enable = true;
      target = "limit.maxfiles"; # suffix .plist
      text = ''
        <?xml version="1.0" encoding="UTF-8"?>
        <!DOCTYPE plist PUBLIC "-//Apple//DTD PLIST 1.0//EN"
                  "http://www.apple.com/DTDs/PropertyList-1.0.dtd">
        <plist version="1.0">
          <dict>
            <key>Label</key>
            <string>limit.maxfiles</string>
            <key>ProgramArguments</key>
            <array>
              <string>launchctl</string>
              <string>limit</string>
              <string>maxfiles</string>
              <string>524288</string>
              <string>524288</string>
            </array>
            <key>RunAtLoad</key>
            <true/>
            <key>ServiceIPC</key>
            <false/>
          </dict>
        </plist
      '';
    };
  };

  # Manage system fonts through nix-darwin.
  fonts = {
    packages = import ../common/fonts.nix pkgs;
  };

  documentation.enable = true;

  system = {
    defaults = {
      finder = {
        ShowPathbar = true;
        FXPreferredViewStyle = "clmv";
        FXRemoveOldTrashItems = true;
      };
    };
    keyboard = {
      enableKeyMapping = true;
      remapCapsLockToControl = true;
    };
  };

  security.pam.services.sudo_local.touchIdAuth = true;
}
