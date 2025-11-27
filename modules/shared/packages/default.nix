# Package composition - combines all shared packages
# Inlined to avoid recursion issues with imports

{ pkgs, lib, ... }:

let
  # Base packages
  basePkgs = with pkgs; [
    fd ripgrep curl tree htop git fzf
    unzip gzip xz zip
    jq comma cachix tig serpl lazysql slumber just
  ];

  # Development tools
  devPkgs = with pkgs; [
    git just
  ];

  # Language packages
  langPkgs = with pkgs; [
    # Clojure
    clojure leiningen babashka
    # Python
    pipx
    # Node.js
    nodejs
    # Java
    zulu
    # Lua
    lua5_4_compat
  ];

  # AI tools
  aiPkgs = with pkgs; [
    claude-code codex claude-code-acp codex-acp cursor-agent
  ];

  combinePackages = import ../../../lib/packages.nix { inherit lib pkgs; };
in
combinePackages.combinePackages {
  common = basePkgs ++ devPkgs ++ langPkgs ++ aiPkgs;
  darwin = [ ];
  linux = [ ];
  nixos = [ ];
}