# PATH additions

{ pkgs, lib, ... }:

{
  home.sessionPath = [
    "$HOME/.cargo/bin"
  ]
  ++ lib.optionals pkgs.stdenv.isDarwin [
    "/Applications/Xcode.app/Contents/Developer/usr/bin/"
    "/Applications/Emacs.app/Contents/MacOS/bin/"
  ];
}