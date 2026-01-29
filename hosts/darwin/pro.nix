# Mac Pro host configuration
# Minimal host-specific overrides

{ ... }:

{
  # Host-specific customizations for Mac Pro
  # Most configuration comes from shared modules

  # The Mac Pro's nixbld group is created with GID 30000 by the installer.
  ids.gids.nixbld = 30000;
}
