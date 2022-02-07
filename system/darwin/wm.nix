{ config, pkgs, ... }:

let
  keycodes = import ./keycodes.nix;
in
{
  services.yabai = {
    enable = false;
    package = pkgs.yabai;
    enableScriptingAddition = true;
    config = {
      window_border = "off";
      # window_border_width = 5;
      # active_window_border_color = "0xffd9adad";
      # normal_window_border_color = "0xff3b4252";
      focus_follows_mouse = "autoraise";
      # focus_follows_mouse = "off";
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
      external_bar = "main:26:0";
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
        #   # windows ▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁
        #   #select
        # ${modMask} - j                            : ${prefix} window --focus next || ${prefix} window --focus "$((${prefix} query --spaces --display next || ${prefix} query --spaces --display first) |${pkgs.jq}/bin/jq -re '.[] | select(.visible == 1)."first-window"')" || ${prefix} display --focus next || ${prefix} display --focus first
        # ${modMask} - k                            : ${prefix} window --focus prev || ${prefix} window --focus "$((yabai -m query --spaces --display prev || ${prefix} query --spaces --display last) | ${pkgs.jq}/bin/jq -re '.[] | select(.visible == 1)."last-window"')" || ${prefix} display --focus prev || ${prefix} display --focus last
        # # close
        # ${modMask} - ${keycodes.Delete}           : ${prefix} window --close && yabai -m window --focus prev
        # # fullscreen
        # ${modMask} - h                            : ${prefix} window --toggle zoom-fullscreen
        # # rotate
        # ${modMask} - r                            : ${prefix} window --focus smallest && yabai -m window --warp largest && yabai -m window --focus largest
        # # increase region
        # ${modMask} - ${keycodes.LeftBracket}      : ${prefix} window --resize left:-20:0
        # ${modMask} - ${keycodes.RightBracket}     : ${prefix} window --resize right:-20:0

        # # spaces ▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁
        # # destroy space
        # ${moveMask} - ${keycodes.Delete} : yabai -m space --destroy

        # # create space
        # ${moveMask} - c : yabai -m space --create

        # # fast focus desktop
        # ${modMask} - tab : yabai -m space --focus recent
        # ${modMask} - p : yabai -m space --focus prev
        # ${modMask} - n : yabai -m space --focus next
        # ${modMask} - 1 : yabai -m space --focus 1
        # ${modMask} - 2 : yabai -m space --focus 2
        # ${modMask} - 3 : yabai -m space --focus 3
        # ${modMask} - 4 : yabai -m space --focus 4
        # ${modMask} - 5 : yabai -m space --focus 5
        # ${modMask} - 6 : yabai -m space --focus 6
        # ${modMask} - 7 : yabai -m space --focus 7
        # ${modMask} - 8 : yabai -m space --focus 8
        # ${modMask} - 9 : yabai -m space --focus 9
        # ${modMask} - 0 : yabai -m space --focus 10

        # # send window to desktop and follow focus
        # ${moveMask} - tab : yabai -m window --space recent; yabai -m space --focus recent
        # ${moveMask} - p : yabai -m window --space prev; yabai -m space --focus prev
        # ${moveMask} - n : yabai -m window --space next; yabai -m space --focus next
        # ${moveMask} - 1 : yabai -m window --space  1; yabai -m space --focus 1
        # ${moveMask} - 2 : yabai -m window --space  2; yabai -m space --focus 2
        # ${moveMask} - 3 : yabai -m window --space  3; yabai -m space --focus 3
        # ${moveMask} - 4 : yabai -m window --space  4; yabai -m space --focus 4
        # ${moveMask} - 5 : yabai -m window --space  5; yabai -m space --focus 5
        # ${moveMask} - 6 : yabai -m window --space  6; yabai -m space --focus 6
        # ${moveMask} - 7 : yabai -m window --space  7; yabai -m space --focus 7
        # ${moveMask} - 8 : yabai -m window --space  8; yabai -m space --focus 8
        # ${moveMask} - 9 : yabai -m window --space  9; yabai -m space --focus 9
        # ${moveMask} - 0 : yabai -m window --space 10; yabai -m space --focus 10
        # # display  ▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁
        # # focus
        # ${modMask} - left                         : ${prevOrLast "display"}
        # ${modMask} - right                        : ${nextOrFirst "display"}
        # # send window
        # ${moveMask} - right                       : ${prefix} window --display prev
        # ${moveMask} - left                        : ${prefix} window --display next
        # apps  ▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁
        ${modMask} - return                       : ${myTerminal}
        ${modMask} + shift - return               : ${myEditor}
        ${modMask} - b                            : ${myBrowser}
        # # reset  ▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁
        # ${modMask} + shift - q                            : pkill yabai; pkill skhd; osascript -e 'display notification "wm restarted"'
      '';
    };
    services.spacebar.enable = false;
    services.spacebar.package = pkgs.spacebar;
    services.spacebar.config = {
      debug_output = "on";
      display = "main";
      position = "top";
      # padding_left = 20;
      # padding_right = 20;
      # spacing_left = 25;
      # spacing_right = 15;
      clock_format = "%m-%d/%R";
      text_font = ''"Roboto Mono:Regular:12.0"'';
      # text_font = ''"Fira Code:Regular:12.0"'';
      icon_font = ''"Font Awesome 5 Free:Solid:12.0"'';
      background_color = "0xff222222";
      foreground_color = "0xffd8dee9";
      space_icon_color = "0xffffab91";
      dnd_icon_color = "0xffd8dee9";
      clock_icon_color = "0xffd8dee9";
      power_icon_color = "0xffd8dee9";
      battery_icon_color = "0xffd8dee9";
      power_icon_strip = " ";
      space_icon = "•";
      # space_icon_strip = "1 2 3 4 5 6 7 8 9 10";
      space_icon_strip = "I II III IV V VI VII VIII IX X";
      spaces_for_all_displays = "on";
      display_separator = "on";
      display_separator_icon = "";
      space_icon_color_secondary = "0xff78c4d4";
      space_icon_color_tertiary = "0xfffff9b0";
      clock_icon = "";
      dnd_icon = "";
      # right_shell = "on";
      # right_shell_icon = "";
      # right_shell_icon_color = "0xffd8dee9";
      right_shell = "on";
      right_shell_icon = "";
      right_shell_command = "whoami";
    };

  }
