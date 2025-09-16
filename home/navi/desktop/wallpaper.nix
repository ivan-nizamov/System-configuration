# wallpaper.nix - Home Manager module for wallpaper management
{ config, lib, pkgs, ... }:

with lib;

let
  cfg = config.services.wallpaper;
  
  wallpaperScript = pkgs.writeShellScriptBin "set-random-wallpaper" ''
    #!/usr/bin/env bash
    set -euo pipefail
    
    # Log start
    echo "Starting wallpaper script..."
    
    DIR="$HOME/Pictures/Wallpapers"
    echo "Using wallpaper directory: $DIR"
    
    # Ensure directory exists and is accessible
    if [[ ! -d "$DIR" ]]; then
      echo "Creating wallpaper directory..."
      mkdir -p "$DIR"
      chmod 700 "$DIR"
    fi
    
    # List directory contents for logging
    echo "Directory contents:"
    ls -la "$DIR" || echo "Failed to list directory"
    echo "Resolved symlink path:"
    readlink -f "$DIR" || echo "Failed to resolve symlink"
    
    # Find a random wallpaper with more verbose error handling
    echo "Searching for wallpapers..."
    echo "Running find command with symlink following..."
    WALLPAPERS=$(find -L "$DIR" -type f \( -iname '*.png' -o -iname '*.jpg' -o -iname '*.jpeg' \) || echo "")
    
    if [[ -z "$WALLPAPERS" ]]; then
      echo "No wallpapers found in $DIR"
      exit 1
    fi
    
    IMG=$(echo "$WALLPAPERS" | shuf -n1)
    
    if [[ ! -f "$IMG" ]]; then
      echo "Selected wallpaper file does not exist: $IMG"
      exit 1
    fi
    
    if [[ ! -r "$IMG" ]]; then
      echo "Selected wallpaper is not readable: $IMG"
      chmod 644 "$IMG" || echo "Failed to change permissions"
    fi
    
    echo "Selected wallpaper: $IMG"
    
    # Kill previous swaybg instance if running
    if [[ -f "$XDG_RUNTIME_DIR/swaybg.pid" ]]; then
      if kill -0 "$(cat "$XDG_RUNTIME_DIR/swaybg.pid")" 2>/dev/null; then
        echo "Killing previous swaybg instance..."
        kill "$(cat "$XDG_RUNTIME_DIR/swaybg.pid")"
      else
        echo "Removing stale PID file..."
        rm "$XDG_RUNTIME_DIR/swaybg.pid"
      fi
    fi
    
    echo "Starting swaybg with mode: ${cfg.mode}"
    # Start new swaybg instance with configured mode
    ${pkgs.swaybg}/bin/swaybg -m ${cfg.mode} -i "$IMG" &
    SWAYBG_PID=$!
    echo $SWAYBG_PID > "$XDG_RUNTIME_DIR/swaybg.pid"
    echo "Wallpaper set successfully with PID $SWAYBG_PID"
    
    # Detach from the process to properly fork
    disown $SWAYBG_PID
    exit 0
  '';

in {
  options.services.wallpaper = {
    enable = mkEnableOption "Random wallpaper switching service";
    
    interval = mkOption {
      type = types.str;
      default = "5m";
      description = "Interval between wallpaper changes";
    };
    
    mode = mkOption {
      type = types.enum [ "fill" "fit" "center" "tile" "stretch" ];
      default = "fill";
      description = "Wallpaper display mode for swaybg";
    };
  };

  config = mkIf cfg.enable {
    home.packages = [ pkgs.swaybg wallpaperScript ];

    # Deploy repo wallpapers into user Pictures/Wallpapers
    home.file."Pictures/Wallpapers" = {
      source = ./../Wallpapers;
      recursive = true;
    };

    systemd.user.services.random-wallpaper = {
      Unit = {
        Description = "Random wallpaper via swaybg";
        PartOf = "graphical-session.target";
        After = [ "graphical-session.target" "hyprland-session.target" ];
        Requires = "graphical-session.target";
      };

      Service = {
        Type = "oneshot";  # Changed back to oneshot for better timer integration
        RemainAfterExit = false;  # Changed from true to false
        KillMode = "none";  # Keep this to prevent systemd from killing swaybg
        Environment = [
          "PATH=${pkgs.swaybg}/bin:${pkgs.coreutils}/bin:${pkgs.findutils}/bin:${pkgs.procps}/bin"
          "DISPLAY=:0"
          "WAYLAND_DISPLAY=wayland-1"
          "XDG_RUNTIME_DIR=/run/user/1000"
          "HOME=/home/navi"
        ];
        ExecStartPre = "${pkgs.coreutils}/bin/sleep 2";
        ExecStart = "${wallpaperScript}/bin/set-random-wallpaper";
        Restart = "on-failure";  # Changed from no to on-failure
      };

      Install = {
        WantedBy = [ "hyprland-session.target" "graphical-session.target" ];
      };
    };

    systemd.user.timers.random-wallpaper = {
      Unit = {
        Description = "Timer for random wallpaper switching (${cfg.interval} interval)";
      };

      Timer = {
        OnBootSec = "1min";
        OnCalendar = "*:0/5:0";  # Run every 5 minutes
        AccuracySec = "1s";
      };

      Install = {
        WantedBy = [ "timers.target" ];
      };
    };
  };
}

