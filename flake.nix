{
  description = "systems configuration";

  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs/nixpkgs-unstable";
    darwin.url = "github:lnl7/nix-darwin";
    home-manager.url = "github:nix-community/home-manager";

    neovim-nightly-overlay.url = "github:nix-community/neovim-nightly-overlay";
    emacs.url = "github:cmacrae/emacs";
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
            users.users.dez.home = "/Users/dez";
            home-manager.users.dez = import ./home/home.nix;
          }
	      {
          nixpkgs.overlays = with inputs; [
            emacs.overlay
            emacs-overlay.overlay
            spacebar.overlay
            nix-direnv.overlay
            neovim-nightly-overlay.overlay
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