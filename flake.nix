{
  description = "systems configuration";

  inputs = {
    nixos.url = "github:NixOS/nixpkgs/nixos-21.11";
    nixpkgs.url = "github:Nixos/nixpkgs/staging-next";
    darwin = {
      url = "github:LnL7/nix-darwin";
      inputs.nixpkgs.follows = "nixpkgs";
    };
    home = {
      url = "github:nix-community/home-manager";
      inputs.nixpkgs.follows = "nixpkgs";
    };

    neovim-nightly-overlay.url = "github:nix-community/neovim-nightly-overlay";
    emacs.url = "github:cmacrae/emacs";
    emacs-overlay.url = "github:nix-community/emacs-overlay";
    # rnix-lsp.url = "github:nix-community/rnix-lsp";
    spacebar.url = "github:cmacrae/spacebar";
    nix-direnv.url = "github:nix-community/nix-direnv";

    # Follows
    # rnix-lsp.inputs.nixpkgs.follows = "nixpkgs";

    comma = { url = "github:Shopify/comma"; flake = false; };
  };

  outputs = { self, nixpkgs, darwin, home, ... }@inputs:
    let
      homeManagerCommonConfig = {
        imports = [
          ./home
          ./modules
        ];
      };
      
      commonDarwinConfig = [
        ./system/macintosh.nix
        home.darwinModules.home-manager
        {
          home-manager.useGlobalPkgs = true;
          home-manager.useUserPackages = true;
          users.users.dez.home = "/Users/dez";
          home-manager.users.dez = homeManagerCommonConfig;
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
              ./system/homebrew.nix
              ./system/wm.nix
            ];
          };
        };
      };
}
