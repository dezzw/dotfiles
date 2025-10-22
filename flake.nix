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
        username: modules:
        {
          home-manager = {
            useGlobalPkgs = true;
            useUserPackages = true;
            backupFileExtension = "bak";
            extraSpecialArgs = {
              inherit inputs username;
            };
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
        mac-app-util.homeManagerModules.default
        inputs.nixvim.homeManagerModules.nixvim
        ./modules/home-manager
      ];

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
          modules =
            [
              {
                nix = import ./nix-settings.nix {
                  inherit inputs system nixpkgs username;
                };
              }
            ]
            ++ (commonDarwinModules username)
            ++ [ (mkHome username commonHomeModules) ]
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

          Desmonds-Mac-mini = mkDarwinSystem {
            hostname = "Desmonds-Mac-mini";
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
          ${username} = home-manager.lib.homeManagerConfiguration {
            pkgs = mkPkgs system;
            extraSpecialArgs = {
              inherit inputs username;
            };
            modules = [
              # Custom module imports
              ./modules/emacs
              ./modules/zellij
              inputs.yazelix-hm.homeManagerModules.default

              # Main configuration
              (
                { pkgs, ... }:
                {
                  # Home configuration
                  home = {
                    username = username;
                    homeDirectory = homeDirectory;
                    stateVersion = "25.11";

                    sessionVariables = {
                      EDITOR = "hx";
                      COLORTERM = "truecolor";
                      TERM = "xterm-256color";
                      XDG_CURRENT_DESKTOP = "WSLG";
                    };

                    packages = with pkgs; [
                      # System utilities
                      htop
                      git
                      fzf
                      fd
                      ripgrep

                      # Fonts
                      maple-mono.truetype
                      maple-mono.NF
                      maple-mono.NF-CN
                      symbola
                    ];
                  };

                  # Font configuration
                  fonts.fontconfig.enable = true;

                  # Program configurations
                  programs = {
                    home-manager.enable = true;

                    # Shell configuration
                    bash = {
                      enable = true;
                      shellAliases = {
                        ls = "eza";
                        ll = "eza -l";
                        la = "eza -a";
                        cd = "z";
                        python = "python3";
                      };

                      bashrcExtra = ''
                        # Work environment variables
                        export XTENSA_SYSTEM=/opt/xtensa/registry
                        export LD_LIBRARY_PATH=/usr/local/lib
                        export LM_LICENSE_FILE=27001@10.0.10.168
                        export XTENSA_CORE=quadra_cpu_prod
                        export PATH="$PATH:/opt/xtensa/XtDevTools/install/tools/RI-2019.1-linux/XtensaTools/bin/"
                        export PATH="$PATH:$HOME/.cargo/bin/"
                        export USER="desmond.wang"
                        export LOGNAME="desmond.wang"
                      '';
                    };

                    # Development tools
                    claude-code.enable = true;
                    yazelix.enable = true;
                    lazygit.enable = true;

                    # File management
                    yazi = {
                      enable = true;
                      enableBashIntegration = true;
                    };

                    # Shell enhancements
                    starship = {
                      enable = true;
                      enableBashIntegration = true;
                    };

                    eza = {
                      enable = true;
                      enableBashIntegration = true;
                    };

                    zoxide = {
                      enable = true;
                      enableBashIntegration = true;
                    };

                    direnv = {
                      enable = true;
                      enableBashIntegration = true;
                      nix-direnv.enable = true;
                    };
                  };
                }
              )
            ];
          };
        };
    };
}
