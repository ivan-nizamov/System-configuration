{ config, pkgs, lib, ... }:

let
  cfg = config.services.networkCurfew;
in
{
  options.services.networkCurfew = {
    enable = lib.mkEnableOption "network curfew";
    
    startTime = lib.mkOption {
      type = lib.types.str;
      default = "20:30:00";
      description = "Time when networking should be disabled (24-hour format)";
    };
    
    endTime = lib.mkOption {
      type = lib.types.str;
      default = "06:00:00";
      description = "Time when networking should be enabled again (24-hour format)";
    };
    
    persistent = lib.mkOption {
      type = lib.types.bool;
      default = true;
      description = "Whether to persist timer across reboots";
    };
  };

  config = lib.mkIf cfg.enable {
    # Network curfew service to disable networking
    systemd.services.net-curfew-off = {
      description = "Disable networking during curfew";
      serviceConfig = { Type = "oneshot"; };
      script = ''
        ${pkgs.networkmanager}/bin/nmcli networking off || true
        echo "$(${pkgs.coreutils}/bin/date): NM networking OFF" >> /var/log/net-curfew.log
      '';
    };
    
    # Network curfew service to enable networking
    systemd.services.net-curfew-on = {
      description = "Enable networking after curfew";
      serviceConfig = { Type = "oneshot"; };
      script = ''
        ${pkgs.networkmanager}/bin/nmcli networking on || true
        echo "$(${pkgs.coreutils}/bin/date): NM networking ON" >> /var/log/net-curfew.log
      '';
    };
    
    # Timer to turn networking off
    systemd.timers.net-curfew-off = {
      description = "Turn networking off at ${cfg.startTime}";
      wantedBy = [ "timers.target" ];
      timerConfig = {
        OnCalendar = "*-*-* ${cfg.startTime}";
        Persistent = cfg.persistent;
      };
    };
    
    # Timer to turn networking on
    systemd.timers.net-curfew-on = {
      description = "Turn networking on at ${cfg.endTime}";
      wantedBy = [ "timers.target" ];
      timerConfig = {
        OnCalendar = "*-*-* ${cfg.endTime}";
        Persistent = cfg.persistent;
      };
    };
  };
}
