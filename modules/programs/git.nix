# Git configuration

{ pkgs, ... }:

{
  programs.git = {
    enable = true;
    package = pkgs.gitFull;

    maintenance.enable = true;

    settings = {
      user.name = "dezzw";
      user.email = "dw@dezzw.com";

      init.defaultBranch = "main";

      fetch.prune = true;

      pull.rebase = true;
      rebase.autoStash = true;
      branch.autoSetupRebase = "always";

      merge.conflictstyle = "zdiff3";
      rerere.enabled = true;
      help.autocorrect = "prompt";
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
