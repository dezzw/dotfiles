# Package composition - combines all packages
# Inlined to avoid recursion issues with imports

{ pkgs, lib, ... }:

let
  # Base packages
  basePkgs = import ./base.nix { inherit pkgs; };

  # Development tools
  devPkgs = import ./dev.nix { inherit pkgs; };

  # Language packages
  langPkgs =
    (import ../lang/clojure.nix { inherit pkgs; })
    ++ (import ../lang/python.nix { inherit pkgs; })
    ++ (import ../lang/nodejs.nix { inherit pkgs; })
    ++ (import ../lang/java.nix { inherit pkgs; })
    ++ (import ../lang/lua.nix { inherit pkgs; });

  # AI tools
  aiPkgs = import ./ai.nix { inherit pkgs; };

  combinePackages = import ../../lib/packages.nix { inherit lib pkgs; };
in
combinePackages.combinePackages {
  common = basePkgs ++ devPkgs ++ langPkgs ++ aiPkgs;
  darwin = [ ];
  linux = [ ];
  nixos = [ ];
}