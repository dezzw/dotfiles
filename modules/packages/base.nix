# Base packages - essential tools available on all platforms

{ pkgs, ... }:

with pkgs; [
  # Filesystem utilities
  fd
  ripgrep
  curl
  tree
  htop
  git
  fzf

  # Compression tools
  unzip
  gzip
  xz
  zip

  # Utilities
  jq
  comma
  cachix
  tig
  lazysql
  slumber
  just
]
++ (with pkgs; lib.optionals stdenv.isLinux [
  # serpl currently fails to build on Darwin via ast-grep test failures.
  serpl
])
