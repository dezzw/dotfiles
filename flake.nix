{
  description = "systems configuration";

  inputs = {
    # Package sets
    master.url = "github:NixOS/nixpkgs/master";
    unstable.url = "github:NixOS/nixpkgs/nixos-unstable";
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-unstable";

    # Environment/system management
    darwin = {
      url = "github:LnL7/nix-darwin";
      inputs.nixpkgs.follows = "nixpkgs";
    };

    home-manager = {
      url = "github:nix-community/home-manager";
      inputs.nixpkgs.follows = "nixpkgs";
    };

    neovim-nightly-overlay = {
      url = "github:nix-community/neovim-nightly-overlay";
    };

    demacs = {
      url = "github:dezzw/demacs";
      inputs.nixpkgs.follows = "nixpkgs";
    };

    emacs-lsp-booster = {
      url = "github:slotThe/emacs-lsp-booster-flake";
      inputs.nixpkgs.follows = "unstable";
    };

    # Tool to make mac aliases without needing Finder scripting permissions for home-manager app linking
    mkalias = {
     url = "github:reckenrode/mkalias";
     inputs.nixpkgs.follows = "nixpkgs";
    };
  };

  outputs = inputs@{ self, nixpkgs, darwin
    , home-manager, ... }:
    let
      inherit (home-manager.lib) homeManagerConfiguration;

      mkPkgs = system:
        import nixpkgs {
          inherit system;
          overlays = with inputs; [
            neovim-nightly-overlay.overlay
            emacs-lsp-booster.overlays.default
            (final: prev: {
              inherit (inputs.mkalias.packages.${final.system}) mkalias;
              inherit (inputs.demacs.packages.${final.system}) demacs;
            })
          ];
          config = import ./config.nix;
        };

      mkHome = username: modules: {
        home-manager = {
          useGlobalPkgs = true;
          useUserPackages = true;
          backupFileExtension = "bak";
          extraSpecialArgs = { inherit inputs username; };
          users."${username}".imports = modules;
        };
      };
    in {
      darwinConfigurations = let username = "dez";
      in {
        Desmonds-MBP = darwin.lib.darwinSystem rec{
          system = "aarch64-darwin";
          pkgs = mkPkgs "aarch64-darwin";
          specialArgs = {
            inherit inputs nixpkgs username;
          };
          modules = [
            {
              nix = import ./nix-settings.nix { inherit inputs system nixpkgs username; };
            }
            ./modules/darwin
            home-manager.darwinModules.home-manager
            (mkHome username [
              ./modules/home-manager
              ./modules/home-manager/home-darwin.nix
              # ./modules/home-manager/home-security.nix
            ])
          ];
        };

        Desmonds-Mac-mini = darwin.lib.darwinSystem {
          system = "aarch64-darwin";
          pkgs = mkPkgs "aarch64-darwin";
          specialArgs = {
            inherit inputs nixpkgs username;
          };
          modules = [
            ./modules/darwin
            home-manager.darwinModules.home-manager
            (mkHome username [
              ./modules/home-manager
              ./modules/home-manager/home-darwin.nix
              # ./modules/home-manager/home-security.nix
            ])
          ];
        };
      };

      nixosConfigurations = {
        nixos = nixpkgs.lib.nixosSystem {
          system = "x86_64-linux";

          modules = [
            ./system/nixos

            home-manager.nixosModules.home-manager
            {
              home-manager.useGlobalPkgs = true;
              home-manager.useUserPackages = true;
              users.users.dez.home = "/home/dez";
              home-manager.users.dez.imports =
                [ ./home ./home/home-darwin.nix ];
            }
          ];
        };
      };
    };
}

