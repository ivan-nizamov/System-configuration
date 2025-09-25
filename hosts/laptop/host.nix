{ config, pkgs, lib, host, ... }:

{
  imports = lib.optional (builtins.pathExists /etc/nixos/hardware-configuration.nix) /etc/nixos/hardware-configuration.nix;

  # Laptop‑specific NixOS configuration.  Enable power management,
  # touchpad support or other hardware tweaks here.  Examples are
  # commented out below.  Adjust as needed for your laptop.

  # Example: Enable TLP for power saving
  # services.tlp.enable = true;

  # Example: Enable libinput touchpad support for Xorg
  # services.xserver.libinput.enable = true;

  # Wayland desktop: Hyprland + XWayland
  programs.hyprland.enable = true;
  programs.xwayland.enable = true;

  # Use greetd for Wayland-native auto-login into Hyprland.
  # (This replaces services.displayManager.autoLogin.*)
  services.greetd = {
    enable = true;
    settings = {
      default_session = {
        command = "Hyprland";
        user = "navi";
      };
    };
  };

  # NetworkManager for connectivity
  networking.networkmanager.enable = true;

  # OPTIONAL: enable the firewall; 
  # networking.firewall.enable = true;

  # Sound and screen sharing
  services.pipewire = {
    enable = true;
    alsa.enable = true;
    alsa.support32Bit = true; # For 32-bit applications
    pulse.enable = true; # For PulseAudio compatibility
  };

  security.rtkit.enable = true;

  # Enable power profiles daemon for power management
  services.power-profiles-daemon.enable = true;

  # Your Wayland/Electron env & dmenu theme vars
  environment.sessionVariables = {
    NIXOS_OZONE_WL = "1";
    DMENU_NB = "#282828"; # Normal background
    DMENU_NF = "#ebdbb2"; # Normal foreground
    DMENU_SB = "#458588"; # Selected background
    DMENU_SF = "#fbf1c7"; # Selected foreground
  };

  # System packages (add the rest of your list back here)
  environment.systemPackages = with pkgs; [
    git wget tree usbutils iproute2 nix-index gparted power-profiles-daemon
    # Your themed dmenu wrappers from the current config:
    (pkgs.writeScriptBin "dmenu" ''
      #!${pkgs.bash}/bin/bash
      exec ${pkgs.dmenu}/bin/dmenu -nb "#282828" -nf "#ebdbb2" -sb "#458588" -sf "#fbf1c7" "$@"
    '')
    (pkgs.writeScriptBin "dmenu_run" ''
      #!${pkgs.bash}/bin/bash
      exec ${pkgs.dmenu}/bin/dmenu_run -nb "#282828" -nf "#ebdbb2" -sb "#458588" -sf "#fbf1c7" "$@"
    '')
    dmenu
  ];

  # (Optional) If you want SSH daemon:
  services.openssh.enable = true;
}