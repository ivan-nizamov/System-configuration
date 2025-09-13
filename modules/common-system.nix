# System configuration using nixpkgs-unstable as the primary source
{ config, pkgs, lib, host, ... }:

{
  # Import additional modules
  imports = [
    ./network-curfew.nix
    ./syncthing.nix
  ];
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
    shell = pkgs.zsh;
    extraGroups = [ "wheel" "networkmanager" "input" "video" "bluetooth" ];
  };

  # Enable zsh system-wide
  programs.zsh.enable = true;

  # PipeWire audio system configuration
  services.pipewire = {
    enable = true;
    alsa.enable = true;
    alsa.support32Bit = true;
    pulse.enable = true;
    wireplumber.enable = true;
    jack.enable = true;  # Optional: JACK compatibility
  };
  
  # Disable PulseAudio (conflicts with PipeWire)
  services.pulseaudio.enable = false;
  
    
  # Real-time audio group (for low-latency audio)
  security.rtkit.enable = true;

  # XDG Desktop Portal configuration for screen sharing (applies to all hosts)
  xdg.portal = {
    enable = true;
    wlr.enable = true; # Enable wlr portal for Wayland screen sharing
    extraPortals = with pkgs; [
      xdg-desktop-portal-wlr # For screen sharing and screenshots
      xdg-desktop-portal-gtk # For file dialogs
    ];
    config = {
      common = {
        default = [ "wlr" "gtk" ];
        "org.freedesktop.impl.portal.ScreenCast" = [ "wlr" ];
        "org.freedesktop.impl.portal.Screenshot" = [ "wlr" ];
      };
    };
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
  # 
  # If any package is broken or unusable in unstable, you can use pkgs-stable.package-name
  # For example: pkgs-stable.broken-package
  environment.systemPackages = with pkgs; [
    git
    vim
    wget
    bat
    gh
    usbutils  # for lsusb
    # Screen sharing and recording tools (added for all hosts)
    grim
    slurp
    wl-clipboard
    wf-recorder
    xdg-desktop-portal
    # Ollama for local AI models
    ollama
    file
  ];

  # Ollama as a system service (localhost only)
  services.ollama = {
    enable = true;
    host = "127.0.0.1";
    port = 11434;
    acceleration = if host.accel != "cpu" then "cuda" else null;
  };

  # Load udev rules from packages (grants device access to user without sudo)
  services.udev.packages = [ pkgs.libsForQt5.xp-pen-deco-01-v2-driver ];
  
  # Enable Bluetooth service
  hardware.bluetooth = {
    enable = true;
    powerOnBoot = true;
    # Enable support for experimental features like LE Audio
    settings = {
      General = {
        Experimental = true;
      };
    };
  };
  
  # AmneziaVPN client (from nixpkgs for this system)
  programs.amnezia-vpn.enable = true;

  # Declarative udev rule for Vial-compatible keyboards
  services.udev.extraRules = ''
    KERNEL=="hidraw*", SUBSYSTEM=="hidraw", ATTRS{serial}=="*vial:f64c2b3c*", MODE="0660", GROUP="users", TAG+="uaccess", TAG+="udev-acl"
  '';

  # System-wide Git configuration managed by common-system
  # This writes /etc/gitconfig so all users inherit these defaults
  environment.etc."gitconfig".text = ''
    [user]
      name = navi
      email = ivan.nizamov@proton.com
    [init]
      defaultBranch = main
  '';

  # Configure the bootloader.  systemd‑boot works on UEFI
  # installations.  If you dual boot with another OS or need BIOS
  # support, you may switch to GRUB.
  boot.loader.systemd-boot.enable = true;
  boot.loader.efi.canTouchEfiVariables = true;
  # boot.loader.systemd-boot.configurationLimit = 3;  # Commented out to keep all generations

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
