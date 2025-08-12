{ config, pkgs, lib, host, inputs, pkgs-unstable, ... }:

{
  # Desktop‑specific Home‑Manager settings.  Use this file to add
  # aliases or user‑level packages that apply only on the desktop.
  # This is a proper home-manager module, so you can use any home-manager options here.
  

  # Desktop-only cursor override (~15% larger than base 26 -> 30), then +10% -> 33
  home.pointerCursor.size = lib.mkForce 33;
  home.sessionVariables.XCURSOR_SIZE = lib.mkForce "33";
  wayland.windowManager.hyprland.settings.env = lib.mkForce [
    "XCURSOR_THEME,macOS"
    "XCURSOR_SIZE,33"
  ];
  wayland.windowManager.hyprland.settings.exec-once = lib.mkForce [
    "dbus-update-activation-environment --systemd WAYLAND_DISPLAY XDG_CURRENT_DESKTOP"
    "hyprctl setcursor macOS 33"
    "waybar"
    "gammastep -o"
    "kitty"
  ];
  
  # Example: desktop-specific packages (GPU tools, etc.)
  # home.packages = with pkgs; [
  #   # Add desktop-specific packages here
  # ];
  
  # Example: desktop-specific aliases
  # programs.zsh.shellAliases = {
  #   # Add desktop-specific aliases here
  # };
}
