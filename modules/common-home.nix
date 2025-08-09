{ host, inputs, ... }:

{
  # Use the same package set for system and home.  This ensures
  # packages installed in your user environment come from the same
  # nixpkgs revision as the system packages.
  home-manager.useGlobalPkgs = true;
  home-manager.useUserPackages = true;

  # Expose the `host` and `inputs` attributes to all home modules.
  # These values let you write conditional logic (e.g. enable a
  # service only on the desktop) and access extra inputs such as
  # sops‑nix.
  home-manager.extraSpecialArgs = {
    inherit host inputs;
  };

  # Make the sops‑nix Home‑Manager module available.  Without this
  # shared module, importing sops.nix in your home profile would
  # require specifying an absolute path to the store.  If you do not
  # manage secrets, you can remove this line.
  # home-manager.sharedModules = [
  #   inputs.sops-nix.homeManagerModules.sops
  # ];

  # Attach the main user configuration.  This file lives under
  # home/navi/home.nix and is shared by all hosts.
  home-manager.users.navi = import ../home/navi/home.nix;
}
