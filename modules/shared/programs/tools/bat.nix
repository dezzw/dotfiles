# Bat configuration

{ ... }:

{
  programs.bat = {
    enable = true;
    config = {
      italic-text = "always";
      style = "plain";
      theme = "OneHalfDark";
    };
  };
}