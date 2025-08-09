{ config, lib, pkgs, host, ... }:

{
  # Desktop‑specific NixOS configuration with GPU support

  # NVIDIA drivers for CUDA acceleration (uncomment when you have real hardware)
  # hardware.nvidia = {
  #   modesetting.enable = true;
  #   nvidiaSettings = true;
  #   open = false;  # Use proprietary drivers
  #   package = config.boot.kernelPackages.nvidiaPackages.stable;
  # };
  # hardware.opengl = {
  #   enable = true;
  #   driSupport = true;
  #   driSupport32Bit = true;  # For 32-bit applications
  # };
  # services.xserver.videoDrivers = [ "nvidia" ];

  # Wayland desktop: Hyprland + XWayland
  programs.hyprland.enable = true;
  programs.xwayland.enable = true;

  # Use greetd for Wayland-native auto-login into Hyprland
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

  # Desktop-specific environment variables
  environment.sessionVariables = {
    NIXOS_OZONE_WL = "1";  # Enable Wayland for Electron apps
    # Add CUDA paths when GPU is enabled
    # CUDA_PATH = "${pkgs.cudatoolkit}";
  };

  # Desktop-specific system packages
  environment.systemPackages = with pkgs; [
    # GPU monitoring tools (uncomment when GPU is enabled)
    # nvtop
    # nvidia-smi
  ];

  # Example: Additional filesystems for desktop storage
  # fileSystems."/mnt/data" = {
  #   device = "/dev/disk/by-uuid/XXXX-XXXX";
  #   fsType = "ext4";
  #   options = [ "defaults" "noatime" ];
  # };

  # Enable SSH
  services.openssh.enable = true;
}
