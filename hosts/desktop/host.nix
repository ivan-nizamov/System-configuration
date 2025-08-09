{ config, lib, pkgs, host, ... }:

{
  # Desktop‑specific NixOS configuration.  You can enable GPU drivers,
  # additional disks, or other services here.  Examples are
  # commented out below.  Un‑comment and adjust to match your
  # hardware.

  # Example: NVIDIA drivers for CUDA acceleration
  # hardware.nvidia = {
  #   modesetting.enable = true;
  #   nvidiaSettings = true;
  #   open = false;
  # };
  # services.xserver.videoDrivers = [ "nvidia" ];

  # Example: Additional filesystems
  # fileSystems."/mnt/data" = {
  #   device = "/dev/disk/by-uuid/XXXX-XXXX";
  #   fsType = "ext4";
  # };
}
