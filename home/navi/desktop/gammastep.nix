# Gammastep configuration for permanent red tint/blue light filter
{ pkgs, config, ... }:

{
  # --------------------------------------------------------------------------
  # Packages
  # --------------------------------------------------------------------------
  home.packages = with pkgs; [
    gammastep
  ];

  # --------------------------------------------------------------------------
  # Gammastep Configuration for Permanent Red Tint
  # --------------------------------------------------------------------------
  xdg.configFile."gammastep/config.ini".text = ''
    ; Global settings for permanent red tint
    [general]
    ; Set screen temperature manually and keep it there
    ; 0: Redshift won't adjust temperature based on time/location
    ; 1: Redshift will adjust temperature (we'll set it once)
    temp-enabled=0 ; Enable temperature setting

    ; Disable automatic brightness adjustment
    brightness-enabled=0

    ; Set a single, constant screen temperature (Kelvin)
    ; Lower values are more red. Experiment to find what you like.
    ; Examples:
    ;   4500K: Mildly warm
    ;   3700K: Noticeably warm/reddish
    ;   3000K: Quite red
    ;   2500K: Very red
    temp-day=4500 ; We'll use temp-day as the constant temperature
    temp-night=4500 ; Set to the same value as temp-day

    ; Disable transitions as it's a constant setting
    transition=0

    ; Location provider is irrelevant as we're not cycling
    ; but set it to manual to avoid any attempts to fetch location.
    location-provider=manual

    ; Manual location (not strictly needed as we're not cycling, but good to have)
    [manual]
    lat=0.0
    lon=0.0

    ; Method to use for screen adjustments
    ; Gammastep should auto-detect 'wlroots' for Hyprland.
    ; adjustment-method=wlroots
  '';

  # Alternative: Systemd user service for Gammastep (more robust for daemon mode)
  # If you prefer daemon mode, use this and remove gammastep from exec-once.
  # systemd.user.services.gammastep = {
  #   Unit = {
  #     Description = "Gammastep screen temperature adjustment (permanent red)";
  #     PartOf = [ "graphical-session.target" ];
  #     After = [ "graphical-session.target" ];
  #   };
  #   Service = {
  #     ExecStart = "${pkgs.gammastep}/bin/gammastep"; # Runs as a daemon
  #     Restart = "on-failure";
  #   };
  #   Install = {
  #     WantedBy = [ "graphical-session.target" ];
  #   };
  # };
}
