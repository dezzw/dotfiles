{
  pkgs ? import <nixpkgs> { },
}:
pkgs.mkShell {
  name = "nixosbuildshell";
  nativeBuildInputs = with pkgs; [
    git
    git-crypt
  ];

  shellHook = ''
    PATH=${pkgs.writeShellScriptBin "nix" ''
      ${pkgs.nixVersions.stable}/bin/nix --experimental-features "nix-command flakes" "$@"
    ''}/bin:$PATH
  '';
}
