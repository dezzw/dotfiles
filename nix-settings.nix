{
  inputs,
  system,
  nixpkgs,
  username,
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

  optimise.automatic = true;

  settings = {
    accept-flake-config = true;

    substituters = [
      "https://cache.nixos.org"
      "https://nix-community.cachix.org"
      "https://demacs.cachix.org"
    ];

    experimental-features = [
      "auto-allocate-uids"
      "ca-derivations"
      "flakes"
      "nix-command"
    ];

    max-jobs = "auto";

    # home-manager will attempt to rebuild the world otherwise...
    trusted-substituters = [
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
    auto-allocate-uids = false
    builders-use-substitutes = true
    http-connections = 0
  ''
  + (nixpkgs.lib.optionalString (system == "aarch64-darwin") ''
    extra-platforms = aarch64-darwin x86_64-darwin
  '');
}
