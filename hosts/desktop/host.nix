{ config, lib, pkgs, host, ... }:

{

  # Desktop‑specific NixOS configuration with GPU support

  # NVIDIA drivers for CUDA acceleration (proprietary)
  hardware.nvidia = {
    modesetting.enable = true;
    nvidiaSettings = true;
    open = false;  # Use proprietary drivers for best Wayland/GBM support
    package = config.boot.kernelPackages.nvidiaPackages.stable;
    # Better runtime power behavior on modern GPUs
    powerManagement.enable = true;
    # finegrained requires offload; this is a desktop dGPU, so leave it disabled
  };

  # Kernel parameters recommended for Wayland + NVIDIA (GBM)
  boot.kernelParams = [
    "nvidia.NVreg_PreserveVideoMemoryAllocations=1"
    "nvidia_drm.modeset=1"
    "nvidia_drm.fbdev=1"
  ];

  # New graphics options (replace deprecated hardware.opengl.*)
  hardware.graphics = {
    enable = true;
    enable32Bit = true;  # For 32-bit applications (Steam/Wine)
    extraPackages = with pkgs; [
      nvidia-vaapi-driver  # VA-API over NVDEC for hardware-accelerated video decode
    ];
  };
  services.xserver.videoDrivers = [ "nvidia" ];

  # Enable PipeWire for audio and video
  services.pipewire = {
    enable = true;
    alsa.enable = true;
    alsa.support32Bit = true;
    pulse.enable = true;
    wireplumber.enable = true; # WirePlumber is the session manager for PipeWire
  };

  # Optional: NVIDIA container toolkit for CUDA-enabled containers (Docker/Podman)
  hardware.nvidia-container-toolkit.enable = true;

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

  # Tablet support prerequisites: uinput and udev rules for OpenTabletDriver
  hardware.uinput.enable = true;
  services.udev.packages = [ pkgs.opentabletdriver ];
  # Ensure user has the right groups for input/uinput access (append safely)
  users.users.navi.extraGroups = lib.mkAfter [ "input" "uinput" ];

  # NetworkManager for connectivity
  networking.networkmanager.enable = true;

  environment.systemPackages = with pkgs; [
    # GPU tools
    nvtopPackages.full
    libva-utils  # provides 'vainfo' to inspect VA-API
    # curl for the scripts
    curl
    # nix is required for npx to work
    nix
  ];

  environment.sessionVariables.XDG_CURRENT_DESKTOP = "Hyprland";

  # Example: Additional filesystems for desktop storage
  # fileSystems."/mnt/data" = {
  #   device = "/dev/disk/by-uuid/XXXX-XXXX";
  #   fsType = "ext4";
  #   options = [ "defaults" "noatime" ];
  # };

  # Enable SSH
  services.openssh.enable = true;
}