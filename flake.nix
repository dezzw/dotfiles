{
  description = "systems configuration";

  inputs = {
    # Package sets
    nixpkgs-master.url = "github:NixOS/nixpkgs/master";
    nixpkgs-stable.url = "github:NixOS/nixpkgs/nixos-22.11";
    nixpkgs-unstable.url = "github:NixOS/nixpkgs/nixos-unstable";

    # Environment/system management
    darwin = {
      url = "github:LnL7/nix-darwin";
      inputs.nixpkgs.follows = "nixpkgs-unstable";
    };

    home-manager = {
      url = "github:nix-community/home-manager";
      inputs.nixpkgs.follows = "nixpkgs-unstable";
    };

    emacs-darwin = { url = "github:dezzw/emacs-darwin"; };

    neovim-nightly-overlay = {
      url = "github:nix-community/neovim-nightly-overlay";
      # Pin to a nixpkgs revision that doesn't have NixOS/nixpkgs#208103 yet
      # inputs.nixpkgs.url = "github:nixos/nixpkgs?rev=fad51abd42ca17a60fc1d4cb9382e2d79ae31836";
    };

    emacs-overlay = {
      url = "github:nix-community/emacs-overlay";
      inputs = {
        nixpkgs.follows = "nixpkgs-unstable";
      };
    };

    nix-direnv.url = "github:nix-community/nix-direnv";

    # Tool to make mac aliases without needing Finder scripting permissions for home-manager app linking
    mkalias.url = "github:reckenrode/mkalias";
    mkalias.inputs.nixpkgs.follows = "nixpkgs-unstable";
  };

  outputs = inputs@{ self, nixpkgs, nixpkgs-stable, nixpkgs-unstable, darwin
    , home-manager, emacs-darwin, ... }:
    let
      inherit (home-manager.lib) homeManagerConfiguration;

      mkPkgs = system:
        import nixpkgs {
          inherit system;
          overlays = with inputs; [
            emacs-overlay.overlay
            emacs-darwin.overlay
            nix-direnv.overlay
            neovim-nightly-overlay.overlay
	    (final: prev: {
      		inherit (inputs.mkalias.packages.${final.system}) mkalias;
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
        Desmonds-MBP = darwin.lib.darwinSystem {
          system = "aarch64-darwin";
          pkgs = mkPkgs "aarch64-darwin";
	  specialArgs = {
            inherit inputs nixpkgs-stable nixpkgs-unstable username;
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
        nixos = nixpkgs-unstable.lib.nixosSystem {
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

