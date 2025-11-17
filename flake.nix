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

    # macOS utilities
    mac-app-util.url = "github:hraban/mac-app-util";

    # AI coding tools
    nix-ai-tools.url = "github:numtide/nix-ai-tools";
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
              inherit (inputs.demacs.packages.${final.stdenv.hostPlatform.system}) demacs;
            })
            (final: prev: {
              inherit (inputs.nix-ai-tools.packages.${final.stdenv.hostPlatform.system})
                claude-code
                codex
                claude-code-acp
                codex-acp
                cursor-agent
                ;
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
            extraSpecialArgs = {
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

      commonHomeModules = [
        inputs.nixvim.homeModules.default
        ./modules/home-manager
      ];

      darwinHomeModules = commonHomeModules ++ [
        mac-app-util.homeManagerModules.default
      ];

      linuxHomeModules = commonHomeModules;

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
          extraSpecialArgs = {
            inherit inputs username homeDirectory;
          }
          // extraSpecialArgs;
          modules = linuxHomeModules ++ [ ] ++ extraModules;
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
          pro = mkDarwinSystem {
            hostname = "pro";
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
