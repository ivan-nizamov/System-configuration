{ config, pkgs, lib, ... }:

let
  cfg = config.services.networkCurfew;
in
{
  options.services.networkCurfew = {
    enable = lib.mkEnableOption "user-based network curfew";
    
    startTime = lib.mkOption {
      type = lib.types.str;
      default = "20:30:00";
      description = "Time when network apps should be terminated for user (24-hour format)";
    };
    
    endTime = lib.mkOption {
      type = lib.types.str;
      default = "06:00:00";
      description = "Time when network apps can be used again by user (24-hour format)";
    };
    
    persistent = lib.mkOption {
      type = lib.types.bool;
      default = true;
      description = "Whether to persist timer across reboots";
    };
    
    user = lib.mkOption {
      type = lib.types.str;
      default = "navi";
      description = "Username to apply curfew to";
    };
  };

  config = lib.mkIf cfg.enable {
    # User-based network curfew service to terminate network apps
    systemd.services.net-curfew-off = {
      description = "Terminate user network applications during curfew";
      serviceConfig = { Type = "oneshot"; };
      script = ''
        # Create log directory
        mkdir -p /var/log
        
        # Get the user ID
        USER_ID=$(${pkgs.coreutils}/bin/id -u ${cfg.user} 2>/dev/null || echo "1000")
        
        # Terminate network applications for the specific user
        ${pkgs.procps}/bin/pkill -f -u $USER_ID "vivaldi|qbittorrent" || true
        
        # Send notification to user if they're logged in
        if ${pkgs.procps}/bin/pgrep -u $USER_ID "Hyprland|gnome-session|plasma" >/dev/null 2>&1; then
          sudo -u ${cfg.user} DISPLAY=:0 ${pkgs.libnotify}/bin/notify-send "Network Curfew" "Network applications have been terminated. Curfew is now active." --icon=network-offline 2>/dev/null || true
        fi
        
        # Log the action
        echo "$(${pkgs.coreutils}/bin/date): User ${cfg.user} network curfew ON (apps terminated)" >> /var/log/net-curfew.log
      '';
    };
    
    # User-based network curfew service to notify curfew end
    systemd.services.net-curfew-on = {
      description = "Notify user that network curfew has ended";
      serviceConfig = { Type = "oneshot"; };
      script = ''
        # Create log directory
        mkdir -p /var/log
        
        # Send notification to user if they're logged in
        if ${pkgs.procps}/bin/pgrep -u ${cfg.user} "Hyprland|gnome-session|plasma" >/dev/null 2>&1; then
          sudo -u ${cfg.user} DISPLAY=:0 ${pkgs.libnotify}/bin/notify-send "Network Curfew" "Network curfew has ended. You can now use network applications." --icon=network-online 2>/dev/null || true
        fi
        
        # Log the action
        echo "$(${pkgs.coreutils}/bin/date): User ${cfg.user} network curfew OFF (apps can be started)" >> /var/log/net-curfew.log
      '';
    };
    
    # Timer to start user curfew
    systemd.timers.net-curfew-off = {
      description = "Start user network curfew at ${cfg.startTime}";
      wantedBy = [ "timers.target" ];
      timerConfig = {
        OnCalendar = "*-*-* ${cfg.startTime}";
        Persistent = cfg.persistent;
      };
    };
    
    # Timer to end user curfew
    systemd.timers.net-curfew-on = {
      description = "End user network curfew at ${cfg.endTime}";
      wantedBy = [ "timers.target" ];
      timerConfig = {
        OnCalendar = "*-*-* ${cfg.endTime}";
        Persistent = cfg.persistent;
      };
    };
  };
}
