{
  description = "systems configuration";

  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs/nixpkgs-unstable";
    darwin.url = "github:lnl7/nix-darwin";
    emacs.url = "github:cmacrae/emacs";
    home-manager.url = "github:nix-community/home-manager";
    emacs-overlay.url = "github:nix-community/emacs-overlay";
    # rnix-lsp.url = "github:nix-community/rnix-lsp";
    spacebar.url = "github:cmacrae/spacebar";

    # Follows
    darwin.inputs.nixpkgs.follows = "nixpkgs";
    home-manager.inputs.nixpkgs.follows = "nixpkgs";
    # rnix-lsp.inputs.nixpkgs.follows = "nixpkgs";
  };

  outputs = { self, nixpkgs, darwin, home-manager, ... }@inputs:
    let
      commonDarwinConfig = [
        ./nixpkgs/darwin/macintosh.nix
        home-manager.darwinModules.home-manager

        {
          nixpkgs.overlays = with inputs; [
            emacs.overlay
            emacs-overlay.overlay
            spacebar.overlay
          ];
        }
      ];
    in
      {
        darwinConfigurations = {
          Desmonds-MBP = darwin.lib.darwinSystem {
            system = "x86_64-darwin";

            modules = commonDarwinConfig ++ [
              # ./nixpkgs/darwin/macintosh.nix
              ./nixpkgs/darwin/wm.nix
              # ./darwin/macintosh.nix
              # ./darwin/wm.nix
              ./nixpkgs/home.nix
              
            ];
          };
        };
      };
      }
