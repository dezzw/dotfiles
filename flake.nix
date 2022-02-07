{
  description = "systems configuration";

  inputs = {
    # Package sets
    nixpkgs-master.url = github:NixOS/nixpkgs/master;
    nixpkgs-stable.url = github:NixOS/nixpkgs/nixpkgs-21.11-darwin;
    nixpkgs-unstable.url = github:NixOS/nixpkgs/nixpkgs-unstable;
    nixos-stable.url = github:NixOS/nixpkgs/nixos-21.11;

    # Environment/system management
    darwin.url = github:LnL7/nix-darwin;
    darwin.inputs.nixpkgs.follows = "nixpkgs-stable";
    home-manager.url = github:nix-community/home-manager;
    home-manager.inputs.nixpkgs.follows = "nixpkgs-stable";

    flake-utils.url = github:numtide/flake-utils;
    flake-compat = { url = github:edolstra/flake-compat; flake = false; };

    neovim-nightly-overlay.url = "github:nix-community/neovim-nightly-overlay";
    emacs.url = "github:cmacrae/emacs";
    emacs-overlay.url = "github:nix-community/emacs-overlay";
    # rnix-lsp.url = "github:nix-community/rnix-lsp";
    spacebar.url = "github:cmacrae/spacebar";
    nix-direnv.url = "github:nix-community/nix-direnv";

    # Follows
    # rnix-lsp.inputs.nixpkgs.follows = "nixpkgs";
  };

  outputs = { self, darwin, home-manager, flake-utils, ... }@inputs:
    let
      inherit (darwin.lib) darwinSystem;
      inherit (inputs.nixpkgs-stable.lib) attrValues makeOverridable optionalAttrs singleton;

      # Configuration for `nixpkgs`
      nixpkgsConfig = {
        config = { allowUnfree = true; };
        overlays = with inputs; [
          emacs.overlay
          emacs-overlay.overlay
          spacebar.overlay
          nix-direnv.overlay
          neovim-nightly-overlay.overlay
        ];
      };

      # Personal configuration shared between `nix-darwin` and plain `home-manager` configs.
      homeManagerStateVersion = "22.05";
      homeManagerCommonConfig = {
        imports = attrValues self.homeManagerModules ++ [
          ./home
          { home.stateVersion = homeManagerStateVersion; }
        ];
      };

      # Modules shared by most `nix-darwin` personal configurations.
      nixDarwinCommonModules = attrValues self.darwinModules ++ [
        # Main `nix-darwin` config
        ./system/darwin
        # `home-manager` module
        home-manager.darwinModules.home-manager
        (
          { config, lib, pkgs, ... }:
          let
            inherit (config.users) primaryUser;
          in
            {
              nixpkgs = nixpkgsConfig;
              # Hack to support legacy worklows that use `<nixpkgs>` etc.
              nix.nixPath = { nixpkgs = "$HOME/.config/nixpkgs/nixpkgs.nix"; };
              # `home-manager` config
              users.users.${primaryUser}.home = "/Users/${primaryUser}";
              home-manager.useGlobalPkgs = true;
              home-manager.users.${primaryUser} = homeManagerCommonConfig;
              # Add a registry entry for this flake
              nix.registry.my.flake = self;
            }
        )
      ];
    in
      {
        darwinConfigurations = rec {
          Desmonds-MBP = darwinSystem {
            system = "x86_64-darwin";
            modules = nixDarwinCommonModules ++ [
              {
                users.primaryUser = "dez";
              }
            ];
          };
        };

        nixos = home-manager.lib.homeManagerConfiguration {
          system = "x86_64-linux";
          stateVersion = homeManagerStateVersion;
          homeDirectory = "/home/dez";
          username = "dez";
          configuration = {
            imports = [ homeManagerCommonConfig ];
            nixpkgs = nixpkgsConfig;
          };
        };
      };
}

