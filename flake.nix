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

    nixvim = {
      url = "github:nix-community/nixvim";
      inputs.nixpkgs.follows = "nixpkgs";
    };

    demacs.url = "github:dezzw/demacs";

    helix-steel.url = "github:mattwparas/helix/steel-event-system";
    helix-steel.inputs.nixpkgs.follows = "nixpkgs";

    mac-app-util.url = "github:hraban/mac-app-util";
  };

  outputs =
    inputs@{
      self,
      nixpkgs,
      darwin,
      home-manager,
      mac-app-util,
      helix-steel,
      ...
    }:
    let
      inherit (home-manager.lib) homeManagerConfiguration;

      mkPkgs =
        system:
        import nixpkgs {
          inherit system;
          overlays = with inputs; [
            # (import ./overlays/aider.nix)
            helix-steel.overlays.default
            (final: prev: {
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
    in
    {
      darwinConfigurations =
        let
          mkDarwin =
            hostname: username:
            darwin.lib.darwinSystem rec {
              system = "aarch64-darwin";
              pkgs = mkPkgs system;

              specialArgs = {
                inherit
                  inputs
                  nixpkgs
                  hostname
                  username
                  ;
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

                  networking.hostName = hostname;
                }

                mac-app-util.darwinModules.default

                ./modules/darwin
                home-manager.darwinModules.home-manager
                (mkHome username [
                  mac-app-util.homeManagerModules.default
                  inputs.nixvim.homeModules.nixvim
                  ./modules/home-manager
                ])
              ];
            };
        in
        {
          pro = mkDarwin "pro" "dez";
          mini = mkDarwin "mini" "dez";
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
              home-manager.users.dez.imports = [
                ./home
                ./home/home-darwin.nix
              ];
            }
          ];
        };
      };
    };
}
