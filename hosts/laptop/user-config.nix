{ config, pkgs, lib, host, inputs, pkgs-unstable, ... }:

{
  # Laptop‑specific Home‑Manager settings.  Use this file to add
  # aliases or user‑level packages that apply only on the laptop.
  # This is a proper home-manager module, so you can use any home-manager options here.
  
  # Laptop display configuration - native 1920x1080 with scale 1.0 (no scaling)
  wayland.windowManager.hyprland.settings.monitor = [
    "eDP-1,1920x1080@60,0x0,1"  # Built-in laptop display at native resolution
  ];
  
  # Laptop cursor size override (20% larger than base 26 → 31)
  home.pointerCursor.size = lib.mkForce 31;
  home.sessionVariables.XCURSOR_SIZE = lib.mkForce "31";
  wayland.windowManager.hyprland.settings.env = lib.mkForce [
    "XCURSOR_THEME,macOS"
    "XCURSOR_SIZE,31"
    "XDG_CURRENT_DESKTOP,Hyprland"
    "XDG_SESSION_TYPE,wayland"
    "QT_QPA_PLATFORM,wayland"
    "GDK_BACKEND,wayland"
    "WEB_PIPEWIRE_CANARY,1"
  ];
  wayland.windowManager.hyprland.settings.exec-once = lib.mkForce [
    "hyprctl setcursor macOS 31"
    "waybar"
    "gammastep -o"
    "kitty"
  ];
  
  # Example: laptop-specific packages
  # home.packages = with pkgs; [
  #   # Add laptop-specific packages here
  # ];
  
  # Example: laptop-specific aliases
  # programs.zsh.shellAliases = {
  #   # Add laptop-specific aliases here
  # };
}
