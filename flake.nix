{
  description = "NixOS 25.05: Multi‑host configuration with integrated Home‑Manager and a server‑safe profile.";

  # Pin all inputs to the latest stable release (25.05) and keep
  # home‑manager and sops‑nix in lockstep with nixpkgs.  This
  # ensures reproducibility and avoids mismatched versions.
  inputs = {
    # Provide a simple way to manage secrets.  If you don’t need
    # secrets yet you can remove sops‑nix from your inputs and
    # comment out the related configuration in home profiles.
    # sops-nix.url = "github:Mic92/sops-nix";
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-25.05";
    home-manager.url = "github:nix-community/home-manager/release-25.05";
    home-manager.inputs.nixpkgs.follows = "nixpkgs";
  };

  outputs = inputs@{ nixpkgs, home-manager, ... }:
  let
    lib = nixpkgs.lib;
    # Helper to build a host.  Pass the host name and the type of
    # acceleration ("cuda" for NVIDIA GPUs, "rocm" for AMD GPUs,
    # "cpu" otherwise).  The specialArgs expose these values to
    # modules.  Each host pulls in the common system configuration,
    # optional GPU configuration, integrated home‑manager, hardware
    # configuration, and any host‑specific overrides.
     lib = nixpkgs.lib;
    mkHost = { name, accel ? "cpu" }:
      lib.nixosSystem {
        system = "x86_64-linux";
        specialArgs = { inherit inputs; host = { inherit name accel; }; };
        modules = [
          ./modules/common-system.nix
          # Integrated HM
          home-manager.nixosModules.home-manager
          ./modules/common-home.nix
          ./hosts/${name}/hardware-configuration.nix
          ./hosts/${name}/host.nix
          ./hosts/${name}/home-overrides.nix
        ];
      };
 in {
    # This host name matches your current config: networking.hostName = "nixos"
    nixosConfigurations.nixos = mkHost { name = "laptop"; accel = "cpu"; };

    # Standalone HM (no sudo) profile for servers
    homeConfigurations."navi@server" =
      let pkgs = import nixpkgs { system = "x86_64-linux"; };
      in home-manager.lib.homeManagerConfiguration {
        inherit pkgs;
        modules = [ ./home/navi/server-cli.nix ];
        extraSpecialArgs = { host = { name = "server"; accel = "cpu"; }; inputs = inputs; };
      };
  };
}

