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

    # Editor configurations
    nixvim = {
      url = "github:nix-community/nixvim";
      inputs.nixpkgs.follows = "nixpkgs";
    };

    demacs.url = "github:dezzw/demacs";

    yazelix-hm = {
      url = "path:/home/desmond/dotfiles/yazelix/home_manager";
      inputs.nixpkgs.follows = "nixpkgs";
    };

    # macOS utilities
    mac-app-util.url = "github:hraban/mac-app-util";
  };

  outputs =
    inputs@{
      self,
      nixpkgs,
      darwin,
      home-manager,
      mac-app-util,
      ...
    }:
    let
      # Helper functions
      mkPkgs =
        system:
        import nixpkgs {
          inherit system;
          overlays = [
            (final: prev: {
              inherit (inputs.demacs.packages.${final.system}) demacs;
            })
          ];
          config = import ./config.nix;
        };

      mkHome =
        {
          username,
          modules,
          extraSpecialArgs ? { },
        }:
        {
          home-manager = {
            useGlobalPkgs = true;
            useUserPackages = true;
            backupFileExtension = "bak";
            extraSpecialArgs =
              {
                inherit inputs username;
              }
              // extraSpecialArgs;
            users."${username}".imports = modules;
          };
        };
      # Common module sets for reuse
      commonDarwinModules = username: [
        mac-app-util.darwinModules.default
        ./modules/darwin
        home-manager.darwinModules.home-manager
      ];

      darwinHomeModules = [
        mac-app-util.homeManagerModules.default
        inputs.nixvim.homeModules.default
        ./modules/home-manager
      ];

      linuxHomeModules = [
        ./modules/home-manager/linux
      ];

      mkLinuxHome =
        {
          username,
          system,
          homeDirectory ? "/home/${username}",
          extraModules ? [ ],
          extraSpecialArgs ? { },
        }:
        home-manager.lib.homeManagerConfiguration {
          pkgs = mkPkgs system;
          extraSpecialArgs =
            {
              inherit inputs username homeDirectory;
            }
            // extraSpecialArgs;
          modules =
            linuxHomeModules
            ++ []
            ++ extraModules;
        };

      mkDarwinSystem =
        {
          hostname,
          username,
          extraModules ? [ ],
        }:
        darwin.lib.darwinSystem rec {
          system = "aarch64-darwin";
          pkgs = mkPkgs system;
          specialArgs = {
            inherit inputs nixpkgs username;
          };
          modules = [
            {
              nix = import ./nix-settings.nix {
                inherit
                  inputs
                  system
                  nixpkgs
                  username
                  ;
              };
            }
          ]
          ++ (commonDarwinModules username)
          ++ [
            (mkHome {
              inherit username;
              modules = darwinHomeModules;
            })
          ]
          ++ extraModules;
        };
    in
    {
      # macOS configurations
      darwinConfigurations =
        let
          username = "dez";
        in
        {
          Desmonds-MBP = mkDarwinSystem {
            hostname = "Desmonds-MBP";
            inherit username;
          };

          mini = mkDarwinSystem {
            hostname = "mini";
            inherit username;
            # extraModules = [ ./modules/home-manager/home-security.nix ];
          };
        };

      # Linux home-manager configurations
      homeConfigurations =
        let
          username = "desmond";
          system = "x86_64-linux";
          homeDirectory = "/home/${username}";
        in
        {
          ${username} = mkLinuxHome {
            inherit username system homeDirectory;
          };
        };
    };
}
