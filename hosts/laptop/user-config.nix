{ config, pkgs, lib, host, inputs, pkgs-unstable, ... }:

{
  # Laptop‑specific Home‑Manager settings.  Use this file to add
  # aliases or user‑level packages that apply only on the laptop.
  # This is a proper home-manager module, so you can use any home-manager options here.
  
  # Laptop display configuration: 1920x1080@60Hz
  wayland.windowManager.hyprland.settings.monitor = ",1920x1080@60,auto,1";
  
  # Example: laptop-specific packages
  # home.packages = with pkgs; [
  #   # Add laptop-specific packages here
  # ];
  
  # Example: laptop-specific aliases
  # programs.zsh.shellAliases = {
  #   # Add laptop-specific aliases here
  # };
}
