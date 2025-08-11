{ config, pkgs, lib, host, ... }:

{
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

  # OPTIONAL: enable the firewall; the curfew units below only toggle NM,
  # not iptables. If you really want hard-cut, keep firewall disabled OR
  # add nftables rules here instead of ad-hoc iptables in scripts.
  # networking.firewall.enable = true;

  # Network Curfew System - Enable and configure
  services.networkCurfew = {
    enable = true;
    startTime = "20:30:00";  # Networking disabled at 20:30
    endTime = "06:00:00";    # Networking enabled at 06:00
    persistent = true;       # Catches up missed executions after reboots
  };

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
    git wget tree usbutils iproute2 nix-index gparted
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