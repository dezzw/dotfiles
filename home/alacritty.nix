{ pkgs, ... }:
{
    programs.alacritty = {
      enable = true;
      settings = {
        window.padding.x = 15;
        window.padding.y = 28;
        window.decorations = "transparent";
        window.dynamic_title = true;
        scrolling.history = 100000;
        live_config_reload = true;
        selection.save_to_clipboard = true;
        mouse.hide_when_typing = true;
        use_thin_strokes = true;

        font = {
          size = 12;
          normal.family = "Roboto Mono";
        };

        colors = {
          cursor.cursor = "#81a1c1";
          primary.background = "#2e3440";
          primary.foreground = "#d8dee9";

          normal = {
            black = "#3b4252";
            red = "#bf616a";
            green = "#a3be8c";
            yellow = "#ebcb8b";
            blue = "#81a1c1";
            magenta = "#b48ead";
            cyan = "#88c0d0";
            white = "#e5e9f0";
          };

          bright = {
            black = "#4c566a";
            red = "#bf616a";
            green = "#a3be8c";
            yellow = "#ebcb8b";
            blue = "#81a1c1";
            magenta = "#b48ead";
            cyan = "#8fbcbb";
            white = "#eceff4";
          };
        };

        key_bindings = [
          { key = "V"; mods = "Command"; action = "Paste"; }
          { key = "C"; mods = "Command"; action = "Copy"; }
          { key = "Q"; mods = "Command"; action = "Quit"; }
          { key = "Q"; mods = "Control"; chars = "\\x11"; }
          { key = "F"; mods = "Alt"; chars = "\\x1bf"; }
          { key = "B"; mods = "Alt"; chars = "\\x1bb"; }
          { key = "D"; mods = "Alt"; chars = "\\x1bd"; }
          { key = "Key3"; mods = "Alt"; chars = "#"; }
          { key = "Slash"; mods = "Control"; chars = "\\x1f"; }
          { key = "Period"; mods = "Alt"; chars = "\\e-\\e."; }
          {
            key = "N";
            mods = "Command";
            command = {
              program = "open";
              args = [ "-nb" "io.alacritty" ];
            };
          }
        ];
      };
  };
}
