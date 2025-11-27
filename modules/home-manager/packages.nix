# Package composition for home-manager
# Combines shared packages with platform-specific additions

{ pkgs, lib, ... }:

let
  # Base packages - inlined to avoid recursion
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

  # Combine all shared packages
  sharedPackages = basePkgs ++ devPkgs ++ langPkgs ++ aiPkgs;
  
  # Linux-specific fonts (inlined to avoid import recursion)
  linuxFonts = with pkgs; [
    maple-mono.truetype
    maple-mono.NF
    maple-mono.NF-CN
    symbola
    nerd-fonts.fira-code
    nerd-fonts.jetbrains-mono
    nerd-fonts.monaspace
  ];
in
{
  home.packages = sharedPackages
    ++ lib.optionals pkgs.stdenv.isDarwin [ ]
    ++ lib.optionals pkgs.stdenv.isLinux linuxFonts;
}