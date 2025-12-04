# Git configuration

{ config, ... }:

{
  programs.git = {
    enable = true;
    settings = {
      user.name = "dezzw";
      user.email = "dw@dezzw.com";
    };
  };

  programs.delta = {
    enable = true;
    options = {
      features = "decorations";

      interactive = {
        keep-plus-minus-markers = false;
      };

      decorations = {
        commit-decoration-style = "blue ol";
        commit-style = "raw";
        file-style = "omit";
        hunk-header-decoration-style = "blue box";
        hunk-header-file-style = "red";
        hunk-header-line-number-style = "#067a00";
        hunk-header-style = "file line-number syntax";
      };
    };
  };
}