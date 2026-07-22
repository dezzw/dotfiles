{
  lib,
  pkgs,
  ...
}:
{
  # stolen from https://github.com/nix-community/home-manager/issues/1341
  # with modifications so full disk access isn't needed (no tmp dir) and finder shell script permissions aren't needed (using mkalias flake)
  targets.darwin.copyApps.enable = false;
  targets.darwin.linkApps.enable = false;

  home.activation.aliasApplications = lib.mkIf pkgs.stdenv.hostPlatform.isDarwin (
    lib.hm.dag.entryAfter [ "writeBoundary" ] ''
      app_folder="Home Manager Apps"
      app_path="$(echo ~/Applications)/$app_folder"

      $DRY_RUN_CMD [ -e "$app_path" ] && rm -r "$app_path"
      $DRY_RUN_CMD mkdir -p "$app_path"

      OIFS="$IFS"
      IFS=$'\n'
      for app in \
        $(find "$newGenPath/home-path/Applications" -type l -exec \
          readlink -f {} \;)
      do
        app_name=$(basename $app)
        #echo source = $app
        #echo destination = $app_path/$app_name
        $DRY_RUN_CMD ${pkgs.mkalias}/bin/mkalias "$app" "$app_path/$app_name"
      done
      IFS="$OIFS"
    ''
  );
}
