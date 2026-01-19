{
  description = "systems configuration";

  nixConfig = {
    substituters = [
      "https://cache.nixos.org"
      "https://nix-community.cachix.org"
      "https://demacs.cachix.org"
      "https://numtide.cachix.org"
      "https://cache.numtide.com"
    ];
    trusted-public-keys = [
      "cache.nixos.org-1:6NCHdD59X431o0gWypbMrAURkbJ16ZPMQFGspcDShjY="
      "nix-community.cachix.org-1:mB9FSh9qf2dCimDSUo8Zy7bkq5CX+/rkCWyvRCYg3Fs="
      "demacs.cachix.org-1:KwSnWI5wdJm4TGdeUfmksk59098voqdDkBVNrUS7yN4="
      "numtide.cachix.org-1:2ps1kLBUWjxIneOy1Ik6cQjb41X0iXVXeHigGmycPPE="
      "niks3.numtide.com-1:DTx8wZduET09hRmMtKdQDxNNthLQETkc/yaX7M4qK0g="
    ];
    experimental-features = [
      "flakes"
      "nix-command"
    ];
    accept-flake-config = true;
  };

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
    llm-agents.url = "github:numtide/llm-agents.nix";

    # Custom Rust packages (tracked as flake inputs for automatic hash management)
    # Tracking HEAD (latest commit) - no version numbers needed!
    # Update with: nix flake update devicon-lookup
    devicon-lookup = {
      url = "github:coreyja/devicon-lookup";
      flake = false;
    };
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
      lib = nixpkgs.lib;
      # Helper functions
      mkPkgs =
        system:
        import nixpkgs {
          inherit system;
          overlays = [
            (final: prev: {
              inherit (inputs.demacs.packages.${final.stdenv.hostPlatform.system}) demacs;
              inherit (inputs.demacs.packages.${final.stdenv.hostPlatform.system}) demacs-igc-patched;
              inherit (inputs.demacs.packages.${final.stdenv.hostPlatform.system}) demacs-master;
              inherit (inputs.demacs.packages.${final.stdenv.hostPlatform.system}) demacs-master-patched;
            })
            (final: prev: {
              inherit (inputs.llm-agents.packages.${final.stdenv.hostPlatform.system})
                claude-code
                codex
                claude-code-acp
                codex-acp
                cursor-agent
                opencode
                gemini-cli
                ;
            })
            (import ./overlays/rust-packages.nix inputs)
            (import ./overlays/aider.nix)
          ];
          config = (import ./nix-config.nix).nixpkgsConfig;
        };

      mkHome =
        {
          username,
          modules,
          homeDirectory ? null,
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
              // lib.optionalAttrs (homeDirectory != null) {
                inherit homeDirectory;
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
          hostname ? null,
          extraModules ? [ ],
          extraSpecialArgs ? { },
        }:
        home-manager.lib.homeManagerConfiguration {
          pkgs = mkPkgs system;
          extraSpecialArgs = {
            inherit inputs username homeDirectory;
          }
          // extraSpecialArgs;
          modules = linuxHomeModules ++ [
            {
              nix = (import ./nix-config.nix).mkNixSettings {
                inherit inputs system nixpkgs username;
                isHomeManager = true;
              };
            }
          ]
          # Host-specific configuration (if exists)
          ++ lib.optionals (hostname != null && builtins.pathExists ./hosts/standalone/${hostname}.nix)
            [ ./hosts/standalone/${hostname}.nix ]
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
              nix = (import ./nix-config.nix).mkNixSettings {
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
            (
              mkHome {
                inherit username;
                modules = darwinHomeModules;
                homeDirectory = "/Users/${username}";
              }
            )
          ]
          # Host-specific configuration (if exists)
          ++ lib.optional (builtins.pathExists ./hosts/darwin/${hostname}.nix)
            ./hosts/darwin/${hostname}.nix
          ++ extraModules;
        };

      # NixOS system builder - applies shared Nix configuration
      mkNixOSSystem =
        {
          hostname,
          system,
          username,
          extraModules ? [ ],
        }:
        nixpkgs.lib.nixosSystem {
          inherit system;
          specialArgs = {
            inherit inputs nixpkgs username;
          };
          modules = [
            ./modules/nixos/nix.nix
            home-manager.nixosModules.home-manager
            (
              mkHome {
                inherit username;
                modules = linuxHomeModules;
                homeDirectory = "/home/${username}";
              }
            )
          ]
          # Host-specific configuration (if exists)
          ++ lib.optional (builtins.pathExists ./hosts/nixos/${hostname}.nix)
            ./hosts/nixos/${hostname}.nix
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
            hostname = "wsl";  # Matches hosts/standalone/wsl.nix
          };
        };

      # NixOS configurations (example - uncomment and customize as needed)
      # nixosConfigurations = {
      #   myhost = mkNixOSSystem {
      #     hostname = "myhost";
      #     system = "x86_64-linux";
      #     username = "desmond";
      #     # extraModules = [ ./hosts/myhost ];
      #   };
      # };
    };
}
