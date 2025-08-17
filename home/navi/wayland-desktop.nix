# Wayland Desktop Environment configuration for Hyprland and related tools
# This module configures the user-side of the Wayland desktop environment
{ pkgs, config, lib, host, ... }:

let
  mod = "SUPER";
in
{  
  imports = [
    ./desktop/gammastep.nix
    ./desktop/waybar.nix
    ./desktop/rofi.nix
    ./desktop/wallpaper.nix
  ];

  # Enable the wallpaper service
  services.wallpaper = {
    enable = true;
    interval = "5m";
    mode = "fill";
  };

  home.packages = with pkgs; [
    brightnessctl # brightness control
    swaybg        # wallpaper manager
    libnotify     # notifications (notify-send)
    # Desktop applications
    # warp-terminal - removed to avoid collision (defined in home.nix from unstable)
    # kitty - now configured via programs.kitty in user-base.nix
    vscode
    yazi
    # emacs - now configured via programs.emacs in emacs.nix
    # pavucontrol - moved to user-base.nix common packages
    nerd-fonts.jetbrains-mono  # For waybar and rofi icons (keeping for icon support)
    mako  # Notification daemon for Wayland
    xdg-desktop-portal-wlr  # For screen sharing
    xdg-desktop-portal-gtk  # For file dialogs
    grim
    slurp
    wl-clipboard
    wf-recorder
    xdg-desktop-portal
  ];

  # macOS-like notifications (mako) with Gruvbox Dark palette
  services.mako = {
    enable = true;
    settings = {
      # Layout and positioning
      anchor = "top-right";
      layer = "overlay";
      width = 400;
      height = 160;
      margin = "12,12,12,12";
      padding = "12";

      # Appearance
      font = "Inter 11";
      "background-color" = "#282828E6";  # Gruvbox bg with ~90% opacity
      "text-color" = "#ebdbb2";           # Gruvbox fg
      "border-color" = "#458588CC";       # Gruvbox blue accent w/ opacity
      "border-size" = 2;
      "border-radius" = 14;                # Rounded corners (macOS-like)
      "progress-color" = "over #83a598CC"; # Softer blue progress
      icons = 1;
      "max-icon-size" = 64;

      # Behavior
      "default-timeout" = 5000;
      "ignore-timeout" = 0;
      "max-history" = 10;

      # Urgency overrides as nested sections
      "urgency=low" = {
        "border-color" = "#3c3836CC";
      };
      "urgency=normal" = {
        "border-color" = "#458588CC";
      };
      "urgency=critical" = {
        "border-color" = "#fb4934CC";
      };
    };
  };

  systemd.user.services.polkit-gnome-authentication-agent-1 = {
    Unit = {
      Description = "polkit-gnome-authentication-agent-1";
      Wants = ["graphical-session.target"];
      After = ["graphical-session.target"];
    };
    Service = {
      Type = "simple";
      ExecStart = "${pkgs.polkit_gnome}/libexec/polkit-gnome-authentication-agent-1";
      Restart = "on-failure";
      RestartSec = 1;
      TimeoutStopSec = 10;
    };
    Install = {
      WantedBy = ["graphical-session.target"];
    };
  };

  home.sessionVariables = {
    NIXOS_OZONE_WL = "1"; # Enable Wayland for Electron apps
    # MOZ_ENABLE_WAYLAND = "1"; # If you want Firefox to use Wayland
    XDG_CURRENT_DESKTOP = "Hyprland";
    XDG_SESSION_TYPE = "wayland";
    XDG_SESSION_DESKTOP = "Hyprland";
    # Enable portals for screen sharing
    GTK_USE_PORTAL = "1";
  };
  
  # Hyprland Window Manager
  wayland.windowManager.hyprland = {
      enable = true;
      
      xwayland.enable = true;

      settings = {

        # Wallpaper is now managed by home-manager wallpaper service
        exec-once = [
          "dbus-update-activation-environment --systemd WAYLAND_DISPLAY XDG_CURRENT_DESKTOP"
          "waybar"
          "gammastep -o"  # Apply red tint once at startup
          "kitty"  # Launch kitty terminal on startup
        ];

        # Monitor configuration is now handled in hosts/desktop/user-config.nix
        # to allow for host-specific settings
        monitor = [ ];


        input = {
          kb_layout = "us";
          follow_mouse = 1;
          touchpad = {
            natural_scroll = true;
            disable_while_typing = true;
          };
        };

        general = {
          gaps_in = 6;
          gaps_out = 8;
          border_size = 2;
          "col.active_border" = "rgba(458588ff)";
          "col.inactive_border" = "rgba(282828ff)";
          layout = "dwindle";
        };

        decoration = {
          rounding = 12;
          rounding_power = 3.5;
          active_opacity = 0.9;
          inactive_opacity = 0.85;
          fullscreen_opacity = 1.0;
          blur = {
            enabled = true;
            size = 8;
            passes = 3;
            noise = 0.015;
            contrast = 0.9;
            brightness = 0.9;
            vibrancy = 0.18;
            vibrancy_darkness = 0.25;
            ignore_opacity = false;
            new_optimizations = true;
          };
        };

        # Make Hyprland apply blur to the Waybar layer-surface
        layerrule = [
          "blur, waybar"
          "ignorealpha 0.70, waybar"
          "ignorealpha 0.70, gtk-layer-shell"
        ];

        gestures = {
          workspace_swipe = true;
          workspace_swipe_fingers = 3;
        };


        bind = [
          "${mod}, RETURN, exec, kitty"
          "${mod}, C, exec, ${pkgs.vscode}/bin/code"
          "${mod}, F, exec, ${pkgs.nautilus}/bin/nautilus"
          "${mod}, T, exec, ${pkgs.rofi}/bin/rofi -show drun -theme ~/.config/rofi/theme"
          "${mod}, B, exec, ${pkgs.vivaldi}/bin/vivaldi"
          "${mod}, E, exec, emacs"
          "${mod} SHIFT, R, exec, bash -c 'hyprctl reload'"
          "${mod}, S, killactive,"
          "${mod} SHIFT, M, exit,"
          "${mod} SHIFT, F, togglefloating,"
          "${mod}, Space, fullscreen, 1"
          "${mod}, Left, movefocus, l"
          "${mod}, Right, movefocus, r"
          "${mod}, Up, movefocus, u"
          "${mod}, Down, movefocus, d"
          "${mod} SHIFT, Left, swapwindow, l"
          "${mod} SHIFT, Right, swapwindow, r"
          "${mod} SHIFT, Up, swapwindow, u"
          "${mod} SHIFT, Down, swapwindow, d"
          "${mod}, 1, workspace, 1"
          "${mod}, 2, workspace, 2"
          "${mod}, 3, workspace, 3"
          "${mod}, 4, workspace, 4"
          "${mod}, 5, workspace, 5"
          "${mod}, 6, workspace, 6"
          "${mod} Shift, 1, movetoworkspace, 1"
          "${mod} Shift, 2, movetoworkspace, 2"
          "${mod} Shift, 3, movetoworkspace, 3"
          "${mod} Shift, 4, movetoworkspace, 4"
          "${mod} Shift, 5, movetoworkspace, 5"
          "${mod} Shift, 6, movetoworkspace, 6"
          ",Print,exec,${config.home.homeDirectory}/bin/screenshot-capture.sh region"
          "Shift,Print,exec, ${config.home.homeDirectory}/bin/screenshot-capture.sh"
          "${mod}, Print, exec, ${config.home.homeDirectory}/bin/screenshot-save.sh"
          # Brightness control keys
          ", XF86MonBrightnessUp, exec, brightnessctl set 5%+"
          ", XF86MonBrightnessDown, exec, brightnessctl set 5%-"
          # Volume control keys
          ", XF86AudioRaiseVolume, exec, wpctl set-volume @DEFAULT_AUDIO_SINK@ 5%+"
          ", XF86AudioLowerVolume, exec, wpctl set-volume @DEFAULT_AUDIO_SINK@ 5%-"
          ", XF86AudioMute, exec, wpctl set-mute @DEFAULT_AUDIO_SINK@ toggle"
          # Mic toggle
          ", XF86AudioMicMute, exec, wpctl set-mute @DEFAULT_AUDIO_SOURCE@ toggle"

          # MOVED: Mako keybindings now live here, where they belong.
          "SUPER, semicolon, exec, makoctl dismiss"
          "SUPER SHIFT, semicolon, exec, makoctl dismiss -a"
          "SUPER, quoteleft, exec, makoctl restore"
          
          "${mod}, l, exec, ${config.home.homeDirectory}/bin/org-sync.sh"
          
        ];

        bindm = [
          "${mod}, mouse:272, movewindow"
          "${mod}, mouse:273, resizewindow"
        ];

        # Window rules
        windowrulev2 = [
          "float, title:^(btop)$"  # Make btop float by default
        ];
      };
      extraConfig = ''
        device {
          name=razer-razer-abyssus-lite-1
          natural_scroll=1
          sensitivity=-3.4
        }
        device {
          name=razer-razer-abyssus-lite
          natural_scroll=1
          sensitivity=-3.4
        }
      '';
  };
}
