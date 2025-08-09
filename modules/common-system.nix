{ config, pkgs, lib, host, ... }:

{
  # Use the host name defined in flake.nix.  This makes it easy to
  # replicate nearly identical systems while still distinguishing
  # between your laptop and desktop.
  networking.hostName = host.name;

  # Enable the new CLI and flakes functionality.  Without these
  # options Nix will refuse to evaluate flakes.
  nix.settings = {
    experimental-features = [ "nix-command" "flakes" ];
    # Automatically optimise the store on build completion to save
    # space.  Remove if you prefer to run nix-store --optimise
    # manually.
    auto-optimise-store = true;
  };

  # Allow unfree software.  If you never need proprietary drivers or
  # applications you can set this to false.
  nixpkgs.config.allowUnfree = true;

  # Create your primary user.  You can add additional users by
  # extending this attribute set.  The user will be added to the
  # wheel group so that they can use sudo.
  users.users.navi = {
    isNormalUser = true;
    extraGroups = [ "wheel" "networkmanager" "input" "video"];
  };

  # Set a consistent timezone.  Adjust if you live outside
  # Europe/Bucharest.
  time.timeZone = "Europe/Bucharest";

  # Enable SSH so you can log in remotely.  It is disabled by
  # default on NixOS.  Consider hardening or restricting access
  # further in a production environment.
  services.openssh.enable = true;

  # Install a handful of useful utilities system‑wide.  These are
  # available to all users.  Add packages here that you want
  # accessible at the system level (e.g. network debugging tools).
  environment.systemPackages = with pkgs; [
    git
    vim
    wget
    bat
  ];

  # Configure the bootloader.  systemd‑boot works on UEFI
  # installations.  If you dual boot with another OS or need BIOS
  # support, you may switch to GRUB.
  boot.loader.systemd-boot.enable = true;
  boot.loader.efi.canTouchEfiVariables = true;
  boot.loader.systemd-boot.configurationLimit = 5;

  # Label your generations in the boot menu so you can quickly
  # identify which host they belong to.  This string will appear
  # alongside the generation number and date.
  system.nixos.label = "nixos-${host.name}";

  # The state version must match the release you initially
  # installed your system with.  Do not bump this when you
  # upgrade NixOS, otherwise some services may revert to their
  # defaults.  See: https://nixos.org/manual/nixos/stable/#sec-upgrading
  system.stateVersion = "25.05";
}
