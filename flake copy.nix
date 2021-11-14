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
    nix-direnv.url = "github:nix-community/nix-direnv";

    # Follows
    darwin.inputs.nixpkgs.follows = "nixpkgs";
    home-manager.inputs.nixpkgs.follows = "nixpkgs";
    # rnix-lsp.inputs.nixpkgs.follows = "nixpkgs";

    comma = { url = "github:Shopify/comma"; flake = false; };
  };

  outputs = { self, nixpkgs, darwin, home-manager, ... }@inputs:
    let
      commonDarwinConfig = [
        ./system/macintosh.nix
        home-manager.darwinModules.home-manager
        {
            home-manager.useGlobalPkgs = true;
            home-manager.useUserPackages = true;
            home-manager.users.jdoe = import ./home/home.nix;
          }
	      {
          nixpkgs.overlays = with inputs; [
            emacs.overlay
            emacs-overlay.overlay
            spacebar.overlay
            nix-direnv.overlay
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
              # ./nixpkgs/darwin/wm.nix
              # ./darwin/macintosh.nix
              # ./darwin/wm.nix 
            ];
          };
        };
      };
}
