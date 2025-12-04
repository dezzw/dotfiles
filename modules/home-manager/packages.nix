# Package composition for home-manager
# Combines packages with platform-specific additions

{ pkgs, lib, ... }:

let
  # Base packages
  basePkgs = import ../packages/base.nix { inherit pkgs; };

  # Development tools
  devPkgs = import ../packages/dev.nix { inherit pkgs; };

  # Language packages
  langPkgs =
    (import ../lang/clojure.nix { inherit pkgs; })
    ++ (import ../lang/python.nix { inherit pkgs; })
    ++ (import ../lang/nodejs.nix { inherit pkgs; })
    ++ (import ../lang/java.nix { inherit pkgs; })
    ++ (import ../lang/lua.nix { inherit pkgs; });

  # AI tools
  aiPkgs = import ../packages/ai.nix { inherit pkgs; };

  # Combine all packages
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