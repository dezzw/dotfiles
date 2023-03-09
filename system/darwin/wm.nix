{ config, pkgs, ... }:

let
  keycodes = import ./keycodes.nix;
in
{
  # Managed by Homebrew for now as latest release 4.0.0
  services.yabai = {
    enable = false;
    package = pkgs.yabai;
    enableScriptingAddition = true;
    config = {
      window_border = "off";
      # window_border_width = 5;
      # active_window_border_color = "0xffd9adad";
      # normal_window_border_color = "0xff3b4252";
      # focus_follows_mouse = "autoraise";
      focus_follows_mouse = "off";
      mouse_follows_focus = "off";
      mouse_drop_action = "stack";
      window_placement = "second_child";
      window_opacity = "on";
      window_topmost = "on";
      window_shadow = "float";
      active_window_opacity = "1.0";
      normal_window_opacity = "1.0";
      split_ratio = "0.50";
      auto_balance = "on";
      mouse_modifier = "fn";
      mouse_action1 = "move";
      mouse_action2 = "resize";
      layout = "bsp";
      top_padding = 10;
      bottom_padding = 10;
      left_padding = 10;
      right_padding = 10;
      window_gap = 10;
      # external_bar = "main:26:0";
    };
    extraConfig = pkgs.lib.mkDefault ''
      sudo yabai --load-sa
      yabai -m signal --add event=dock_did_restart action="sudo yabai --load-sa"
      # rules
      yabai -m rule --add app='System Preferences' manage=off
      yabai -m rule --add app='Dash'               manage=off
      yabai -m rule --add app='JetBrains Toolbox'  manage=off
      yabai -m rule --add app='WeChat'             manage=off
      yabai -m rule --add app='Discord'            manage=off
      yabai -m rule --add app='Steam'            manage=off
      yabai -m rule --add app='Emacs'              manage=on
    '';
  };

  services.skhd = {
    enable = true;
    package = pkgs.skhd;
    skhdConfig =
      let
        modMask = "cmd";
        moveMask = "ctrl + cmd";
        spaceMask = "alt";
        myTerminal = "emacsclient -s main -a '' -nc --eval '(eshell)'";
        myEditor = "emacsclient -s main -a '' -nc";
        myBrowser = "open /Safari.app";
        noop = "/dev/null";
        prefix = "${pkgs.yabai}/bin/yabai -m";
        fstOrSnd = { fst, snd }: domain: "${prefix} ${domain} --focus ${fst} || ${prefix} ${domain} --focus ${snd}";
        nextOrFirst = fstOrSnd { fst = "next"; snd = "first"; };
        prevOrLast = fstOrSnd { fst = "prev"; snd = "last"; };
      in
      ''
        # windows ▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁
        # select
        # ${modMask} - j                            : ${prefix} window --focus next || ${prefix} window --focus "$((${prefix} query --spaces --display next || ${prefix} query --spaces --display first) |${pkgs.jq}/bin/jq -re '.[] | select(.visible == 1)."first-window"')" || ${prefix} display --focus next || ${prefix} display --focus first
        ${modMask} - j : yabai -m window --focus south
        ${modMask} - k                            : ${prefix} window --focus prev || ${prefix} window --focus "$((yabai -m query --spaces --display prev || ${prefix} query --spaces --display last) | ${pkgs.jq}/bin/jq -re '.[] | select(.visible == 1)."last-window"')" || ${prefix} display --focus prev || ${prefix} display --focus last
        # close
        # ${moveMask} - ${keycodes.Delete}           : ${prefix} window --close && yabai -m window --focus prev
        # fullscreen
        ${moveMask} - h                            : ${prefix} window --toggle zoom-fullscreen
        # rotate
        ${moveMask} - r                            : ${prefix} window --focus smallest && yabai -m window --warp largest && yabai -m window --focus largest
        # increase region
        ${moveMask} - ${keycodes.LeftBracket}      : ${prefix} window --resize left:-20:0
        ${moveMask} - ${keycodes.RightBracket}     : ${prefix} window --resize right:-20:0

        # spaces ▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁
        # destroy space
        # ${moveMask} - ${keycodes.Delete} : yabai -m space --destroy

        # create space
        # ${moveMask} - c : yabai -m space --create

        # fast focus desktop
        ${modMask} - tab : yabai -m space --focus recent
        ${modMask} - p : yabai -m space --focus prev
        ${modMask} - n : yabai -m space --focus next
        ${modMask} - 1 : yabai -m space --focus 1
        ${modMask} - 2 : yabai -m space --focus 2
        ${modMask} - 3 : yabai -m space --focus 3
        ${modMask} - 4 : yabai -m space --focus 4
        ${modMask} - 5 : yabai -m space --focus 5
        ${modMask} - 6 : yabai -m space --focus 6
        ${modMask} - 7 : yabai -m space --focus 7
        ${modMask} - 8 : yabai -m space --focus 8
        ${modMask} - 9 : yabai -m space --focus 9
        ${modMask} - 0 : yabai -m space --focus 10

        # send window to desktop and follow focus
        ${moveMask} - tab : yabai -m window --space recent; yabai -m space --focus recent
        ${moveMask} - p : yabai -m window --space prev; yabai -m space --focus prev
        ${moveMask} - n : yabai -m window --space next; yabai -m space --focus next
        ${moveMask} - 1 : yabai -m window --space  1; yabai -m space --focus 1
        ${moveMask} - 2 : yabai -m window --space  2; yabai -m space --focus 2
        ${moveMask} - 3 : yabai -m window --space  3; yabai -m space --focus 3
        ${moveMask} - 4 : yabai -m window --space  4; yabai -m space --focus 4
        ${moveMask} - 5 : yabai -m window --space  5; yabai -m space --focus 5
        ${moveMask} - 6 : yabai -m window --space  6; yabai -m space --focus 6
        ${moveMask} - 7 : yabai -m window --space  7; yabai -m space --focus 7
        ${moveMask} - 8 : yabai -m window --space  8; yabai -m space --focus 8
        ${moveMask} - 9 : yabai -m window --space  9; yabai -m space --focus 9
        ${moveMask} - 0 : yabai -m window --space 10; yabai -m space --focus 10
        # display  ▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁
        # focus
        # ${modMask} - left                         : ${prevOrLast "display"}
        # ${modMask} - right                        : ${nextOrFirst "display"}
        # send window
        # ${moveMask} - right                       : ${prefix} window --display prev
        # ${moveMask} - left                        : ${prefix} window --display next
        # apps  ▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁
        ${modMask} + shift - return                       : ${myTerminal}
        ${modMask} - return                               : ${myEditor}
        ${modMask} - b                                    : ${myBrowser}
        # reset  ▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁
        # ${modMask} + shift - q                            : pkill yabai; pkill skhd; osascript -e 'display notification "wm restarted"'
      '';
  };
}
