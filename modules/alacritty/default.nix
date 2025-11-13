{ pkgs, ... }:

{
  programs.alacritty = {
    enable = true;
    package = pkgs.alacritty; # switching to unstable so i get 0.11 with undercurl support
    settings = {
      window = {
        decorations = "Buttonless";
        dynamic_title = true;
        opacity = 0.7;
        blur = true;
        option_as_alt = "Both";
      };
      scrolling.history = 3000;
      font = {
        normal = {
          family = "FiraCode Nerd Font Mono";
          style = "Regular";
        };
        bold.style = "Bold";
        italic.style = "Italic";
        bold_italic.style = "Bold Italic";
        size = if pkgs.stdenvNoCC.isDarwin then 14 else 9;
      };
      shell.program = "${pkgs.zsh}/bin/zsh";
      live_config_reload = true;
      cursor.vi_mode_style = "Underline";
      colors.draw_bold_text_with_bright_colors = true;
      keyboard.bindings = [
      ];
    };
  };
}
