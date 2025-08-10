{ config, pkgs, lib, host, inputs, pkgs-unstable, ... }:

{
  # Desktop‑specific Home‑Manager settings.  Use this file to add
  # aliases or user‑level packages that apply only on the desktop.
  # This is a proper home-manager module, so you can use any home-manager options here.
  
  # Desktop display configuration: 2560x1440@165Hz
  wayland.windowManager.hyprland.settings.monitor = ",2560x1440@165,auto,1";
  
  # Example: desktop-specific packages (GPU tools, etc.)
  # home.packages = with pkgs; [
  #   # Add desktop-specific packages here
  # ];
  
  # Example: desktop-specific aliases
  # programs.zsh.shellAliases = {
  #   # Add desktop-specific aliases here
  # };
}
