# Combined Nix configuration file
# Exports configurations for all contexts: NixOS, nix-darwin, home-manager, and standalone

rec {
  # Shared base configuration - single source of truth
  baseConfig = {
    substituters = [
      "https://cache.nixos.org"
      "https://nix-community.cachix.org"
      "https://demacs.cachix.org"
      "https://numtide.cachix.org"
    ];
    trusted-public-keys = [
      "cache.nixos.org-1:6NCHdD59X431o0gWypbMrAURkbJ16ZPMQFGspcDShjY="
      "nix-community.cachix.org-1:mB9FSh9qf2dCimDSUo8Zy7bkq5CX+/rkCWyvRCYg3Fs="
      "demacs.cachix.org-1:KwSnWI5wdJm4TGdeUfmksk59098voqdDkBVNrUS7yN4="
      "numtide.cachix.org-1:2ps1kLBUWjxIneOy1Ik6cQjb41X0iXVXeHigGmycPPE="
    ];
    experimental-features = [
      "flakes"
      "nix-command"
    ];
    accept-flake-config = true;
  };

  # Nixpkgs configuration (for importing nixpkgs)
  nixpkgsConfig = {
    allowUnsupportedSystem = true;
    allowBroken = true;
    allowUnfree = true;
    keep-derivations = true;
    keep-outputs = true;
  };


  # NixOS module - for system-wide Nix configuration on NixOS
  nixosModule = { config, lib, ... }: {
    nix = {
      settings = {
        inherit (baseConfig) substituters experimental-features accept-flake-config;
        trusted-public-keys = baseConfig.trusted-public-keys;
        trusted-substituters = baseConfig.substituters;
        max-jobs = "auto";
        builders-use-substitutes = true;
        http-connections = 0;
        keep-outputs = true;
        keep-derivations = true;
      };
      extraOptions = ''
        builders-use-substitutes = true
        http-connections = 0
      '';
      optimise.automatic = true;
    };
  };

  # Nix daemon/system settings function for nix-darwin and home-manager
  mkNixSettings = {
    inputs,
    system,
    nixpkgs,
    username,
    isHomeManager ? false,
    ...
  }:
  {
    nixPath = [ "nixpkgs=${nixpkgs}" ];
    package = inputs.master.legacyPackages.${system}.nix;

    registry = {
      system.flake = inputs.self;
      default.flake = nixpkgs;
      home-manager.flake = inputs.home-manager;
    };

      settings = {
        inherit (baseConfig) accept-flake-config substituters experimental-features;
        trusted-public-keys = baseConfig.trusted-public-keys;
        trusted-substituters = baseConfig.substituters;
        max-jobs = "auto";
        trusted-users = [
          "${username}"
          "root"
          "@admin"
          "@wheel"
        ];
        # sandbox = false;
        # extra-sandbox-paths = [
        #   "/Applications/Xcode.app/Contents/Developer/usr/bin/"
        # ];
      };

      extraOptions = ''
        keep-outputs = true
        keep-derivations = true
        builders-use-substitutes = true
        http-connections = 0
      ''
    + (nixpkgs.lib.optionalString (system == "aarch64-darwin") ''
      extra-platforms = aarch64-darwin x86_64-darwin
    '');
  }
  // nixpkgs.lib.optionalAttrs (!isHomeManager) {
    # Only available in system configs (NixOS/nix-darwin), not home-manager
    optimise.automatic = true;
  };

  # Generate nix.conf content for standalone systems (non-NixOS, no home-manager)
  # Usage: Write the output to /etc/nix/nix.conf or ~/.config/nix/nix.conf
  mkStandaloneNixConf = username: ''
    # Generated Nix configuration
    # Source: nix-config.nix

    # Substituters
    substituters = ${builtins.concatStringsSep " " (map (s: "${s}") baseConfig.substituters)}

    # Trusted public keys
    ${builtins.concatStringsSep "\n" (map (k: "trusted-public-keys = ${k}") baseConfig.trusted-public-keys)}

    # Experimental features
    experimental-features = ${builtins.concatStringsSep " " baseConfig.experimental-features}

    # Accept flake config
    accept-flake-config = true

    # Performance settings
    max-jobs = auto
    builders-use-substitutes = true
    http-connections = 0
    keep-outputs = true
    keep-derivations = true

    # Trusted users
    trusted-users = ${username} root @admin @wheel
  '';
}
