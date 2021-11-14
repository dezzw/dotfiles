{ config, pkgs, ... }:

{
  nix.package = pkgs.nixFlakes;
  nix.extraOptions = ''
    experimental-features = nix-command flakes
    keep-derivations = true
    keep-outputs = true
  '';

  nix.binaryCaches = [
    "https://cachix.org/api/v1/cache/nix-community"
    
    "https://cachix.org/api/v1/cache/emacs"
  ];

  nix.binaryCachePublicKeys = [
    "emacs.cachix.org-1:b1SMJNLY/mZF6GxQE+eDBeps7WnkT0Po55TAyzwOxTY="
    "nix-community.cachix.org-1:mB9FSh9qf2dCimDSUo8Zy7bkq5CX+/rkCWyvRCYg3Fs="
  ];

  nix.trustedBinaryCaches = config.nix.binaryCaches;
  
  nixpkgs.overlays = [
    (import ../overlays)
  ];


  # Used for backwards compatibility, please read the changelog before changing.
  # $ darwin-rebuild changelog
  system.stateVersion = 4;

  nix.trustedUsers = [ "root" "dez" ];
  users.users.dez.home = "/Users/dez";

  # List packages installed in system profile. To search by name, run:
  # $ nix-env -qaP | grep wget
  environment.shells = [ pkgs.zsh ];
  environment.systemPackages = [ pkgs.zsh pkgs.gcc pkgs.git ];
  programs.bash.enable = false;

  # Create /etc/bashrc that loads the nix-darwin environment.
  programs.zsh.enable = true;  # default shell on catalina
  # programs.fish.enable = true;
 
  environment.variables.EDITOR = "nvim";

  # Use a custom configuration.nix location.
  # $ darwin-rebuild switch -I darwin-config=$HOME/.config/nixpkgs/darwin/configuration.nix
  environment.darwinConfig = "$HOME/.dotfiles/nixpkgs/darwin/macintosh.nix";

  # Auto upgrade nix package and the daemon service.
  services.nix-daemon.enable = true;
  # nix.package = pkgs.nix;


  fonts.enableFontDir = true;
  fonts.fonts = with pkgs; [ cantarell-fonts roboto roboto-mono mononoki ];

  homebrew.enable = true;
  homebrew.autoUpdate = true;
  homebrew.cleanup = "zap";
  homebrew.global.brewfile = true;
  homebrew.global.noLock = true;

  homebrew.taps = [
    "homebrew/core"
    "homebrew/cask"
    "homebrew/cask-fonts"
  ];

  homebrew.casks = [
    # fonts
    "font-fira-code"
    "font-hack-nerd-font"
    "font-jetbrains-mono"

    # Applications
    "alfred"
    "dash"
    "discord"
    "downie"
    "eclipse-java"
    "flycut"
    "hazeover"
    "gimp"
    "iina"
    "iterm2"
    "jetbrains-toolbox"
    "visual-studio-code"
    "zoom"
    "spotify"
    "google-chrome"
    "obs"
    "alacritty"
    "appcleaner"
  ];

  # home-manager.users.dez = {
  #   home.stateVersion = "21.05";

  #   home.packages = with pkgs; [

  #   nixUnstable

  #   # c/c++
  #   ccls
  #   cmake

  #   # nodejs
  #   nodejs
  #   nodePackages.typescript
  #   nodePackages.typescript-language-server
  #   nodePackages.bash-language-server
  #   nodePackages.vscode-css-languageserver-bin
  #   nodePackages.vscode-html-languageserver-bin

  #   # nix
  #   rnix-lsp

  #   #cach
  #   cachix

  #   # direnv
  #   direnv

  #   # tools
  #   jump
  #   exa
  #   stow
  #   ripgrep

  #   # tex
  #   texlive.combined.scheme-full
  # ];

  # programs.git = {
  #   enable = true;
  #   userName  = "UncleAlone";
  #   userEmail = "desmond.pc.w@gmail.com";
  #   delta = {
  #     enable = true;
  #     options = {
  #       features = "decorations";

  #       interactive = {
  #         keep-plus-minus-markers = false;
  #       };

  #       decorations = {
  #         commit-decoration-style = "blue ol";
  #         commit-style = "raw";
  #         file-style = "omit";
  #         hunk-header-decoration-style = "blue box";
  #         hunk-header-file-style = "red";
  #         hunk-header-line-number-style = "#067a00";
  #         hunk-header-style = "file line-number syntax";
  #       };
  #     };
  #   };
  # };

  # programs.bat.enable = true;
  # programs.bat.config = {
  #   theme = "ansi";
  # };

  # programs.zsh = {
  #   enable = true;
  #   shellAliases = {
  #     ls = "exa";
  #     la = "exa -la";
  #     lt = "exa -laT";
  #   };
  #   zplug = {
  #     enable = true;
  #     plugins = [
  #       { name = "zsh-users/zsh-autosuggestions"; } # Simple plugin installation
	#       { name = "zsh-users/zsh-syntax-highlighting"; }
  #       # { name = "spwhitt/nix-zsh-completions"; }
	#       { name = "romkatv/powerlevel10k"; tags = [ as:theme depth:1 ]; }
  #     ];
  #   };
  #   initExtra = ''
  #    . $HOME/.p10k.zsh
  #    . $HOME/.dotfiles/Shells/emacs-cmds.sh
  #    . $HOME/.dotfiles/Shells/doom-emacs-cmds.sh
  #    eval "$(jump shell)"
  #    eval "$(direnv hook zsh)"
  #   '';
  # };


  # programs.emacs.enable = true;

  # programs.emacs.package =
  #   (
  #     pkgs.emacsWithPackagesFromUsePackage {
  #       alwaysEnsure = true;
  #       alwaysTangle = true;

  #       # Custom overlay derived from 'emacs' flake input
  #       package = pkgs.emacs;
  #       config = ../../Emacs/emacs-configs/demacs/init.el;

  #     }
  #   );

  # home.file.".emacs-profiles.el".text = ''
  #     (("default" . ((user-emacs-directory . "~/.dotfiles/Emacs/emacs-configs/demacs")))
  #      ("doom" . ((user-emacs-directory . "~/.dotfiles/Emacs/emacs-configs/doom-core")
	#       ;;(env . (("DOOMDIR" . "~/.dotfiles/Emacs/emacs-configs/doom")))))
	#     ))
  #      ("beta" . ((user-emacs-directory . "~/.dotfiles/Emacs/emacs-configs/beta_emacs")))
  #     )
  # '';
  # };
}
