# Waybar configuration imported from prototype (Gruvbox + icon set)
{ pkgs, lib, config, ... }:

{
  programs.waybar = {
    enable = true;

    settings = {
      mainBar = {
        layer = "top";
        position = "top";
        height = 32;
        spacing = 0;
        margin-left = 8;
        margin-right = 8;
        margin-top = 8;
        margin-bottom = 0;

        modules-left = [ "hyprland/workspaces" ];
        modules-center = [ "clock" ];
        modules-right = [ "pulseaudio" "network" "battery" "tray" ];

        "hyprland/workspaces" = {
          format = "{icon}";
          persistent-workspaces = {
            "1" = []; "2" = []; "3" = []; "4" = []; "5" = []; "6" = [];
          };
          format-icons = {
            "1" = ""; "2" = ""; "3" = ""; "4" = ""; "5" = ""; "6" = "";
            "default" = ""; "urgent" = ""; "focused" = ""; "empty" = "";
          };
          sort-by-number = true;
        };

        "clock" = {
          format = "{:%H:%M:%S}";
          format-alt = " {:%Y-%m-%d}  {:%H:%M:%S}";
          tooltip-format = "<big>{:%Y %B}</big>\n<tt><small>{calendar}</small></tt>";
          interval = 1;
        };

        "pulseaudio" = {
          format = "{volume}% {icon} {format_source}";
          format-bluetooth = "{volume}% {icon} {format_source}";
          format-bluetooth-muted = "󰖁 {icon} {format_source}";
          format-muted = "Muted 󰖁 {format_source}";
          format-source = "  {volume}%";
          format-source-muted = "   Muted";
          format-icons = {
            headphone = ""; hands-free = "󰋎"; headset = "󰋎"; phone = "";
            portable = ""; car = ""; default = ["󰕿" "󰖀" "󰕾"];
          };
          on-click = "pavucontrol";
        };

        "custom/floatterm" = {
            format = "";                         # needs a Nerd Font / Font Awesome
            tooltip = "Open floating kitty";
            # Use the hyprctl from Nix store so PATH isn’t an issue
            on-click = ''
            ${pkgs.hyprland}/bin/hyprctl dispatch exec "[float] bluetouthie"'';
      };


        "network" = {
          format-wifi = "{essid} ({signalStrength}%)";
          format-ethernet = "{ifname}: {ipaddr}/{cidr} 󰈀";
          tooltip-format = "{ifname} via {gwaddr} ";
          format-linked = "{ifname} (No IP) 󰈀";
          format-disconnected = "Disconnected ⚠";
          on-click = "hyprctl dispatch exec '[float; center; size=1024x600] kitty nmtui'";
        };

        "battery" = {
          states = { good = 95; warning = 30; critical = 15; };
          format = "{capacity}% {icon}";
          format-charging = "{capacity}% 󰂄";
          format-plugged = "{capacity}% ";
          format-alt = "{time} {icon}";
          format-icons = ["󰁺" "󰁻" "󰁼" "󰁽" "󰁾" "󰁿" "󰂀" "󰂁" "󰂂" "󰁹"];
        };

        "tray" = {
          icon-size = 18;
          spacing = 10;
        };
      };
    };

    style = ''
      /* Gruvbox Dark Theme Variables */
      @define-color bg0_h     #1d2021;
      @define-color bg0       #282828;
      @define-color bg1       #3c3836;
      @define-color bg2       #504945;
      @define-color bg3       #665c54;
      @define-color bg4       #7c6f64;

      @define-color fg0       #fbf1c7;
      @define-color fg1       #ebdbb2;
      @define-color fg2       #d5c4a1;
      @define-color fg3       #bdae93;

      @define-color red       #cc241d;
      @define-color green     #98971a;
      @define-color yellow    #d79921;
      @define-color blue      #458588;
      @define-color purple    #b16286;
      @define-color aqua      #689d6a;
      @define-color orange    #d65d0e;
      @define-color gray      #928374;

      /* Theme application */
      @define-color gruvbox_bar_bg         @bg0;       /* Bar background */
      @define-color gruvbox_module_bg      @bg1;       /* Module background */
      @define-color gruvbox_tooltip_bg     @bg2;       /* Tooltip background */
      @define-color gruvbox_text           @fg1;       /* Main text */
      @define-color gruvbox_text_dim       @fg2;       /* Dim text */
      @define-color gruvbox_accent         @blue;      /* Focused workspace, primary selection */
      @define-color gruvbox_accent_hover   @aqua;      /* Hover effects, secondary accent */
      @define-color gruvbox_urgent         @red;       /* Urgent state */
      @define-color gruvbox_warning        @yellow;    /* Warning state */
      @define-color gruvbox_text_on_accent @fg0;       /* Text on dark accents */
      @define-color gruvbox_text_on_warning @bg0;      /* Text on bright yellow warning */

      * {
        border: none;
        border-radius: 0;
        font-family: "Maple Mono Variable", FontAwesome, sans-serif;
        font-size: 14px;
        min-height: 0;
      }

      /* main glass bar */
      window#waybar {
        margin: 8px 8px 0 8px;     /* matches gaps_out = 8 */
        padding-left: 10px;        /* gaps_out + border_size if you want pixel-perfect */
        padding-right: 10px;

        /* Darker, more opaque "mac" glass */
        background-color: rgba(18, 19, 21, 0.75);  /* lowered from 0.80 for more transparency */

        border-radius: 12px;
        border: 1px solid rgba(255,255,255,0.06);  /* faint outer stroke */
        box-shadow: 0 8px 24px rgba(0,0,0,0.35);   /* soft drop shadow */
      }

      /* text/icon color tuning */
      window#waybar,
      window#waybar * {
        color: rgba(255,255,255,0.86);
      }

      /* workspace pills (mac-ish chips) - flat and non-interactive */
      #workspaces button {
        background: transparent;
        border-radius: 9px;
        padding: 4px 10px;
        transition: none; /* Remove any transition effects */
      }

      /* Remove all hover and active effects */
      #workspaces button:hover,
      #workspaces button:focus,
      #workspaces button:active {
        background: transparent;
        box-shadow: none;
        border: none;
      }

      /* Remove opaque module boxes; keep spacing only */
      #workspaces,
      #clock,
      #battery,
      #pulseaudio,
      #network,
      #tray,
      #custom-*,
      #window {
        background: transparent;
        margin: 0 3px;
        padding: 0 10px;
        border-radius: 8px;
      }

      /* Tooltips should also feel glassy */
      tooltip {
        background: rgba(30,30,32,0.60);
        border: 1px solid rgba(255,255,255,0.10);
        border-radius: 10px;
        box-shadow: 0 8px 20px rgba(0,0,0,0.35);
      }
      tooltip label {
        color: @gruvbox_text;
        padding: 5px;
      }

      #tray > .passive { -gtk-icon-effect: dim; color: @gruvbox_text_dim; }
      #tray > .needs-attention {
        -gtk-icon-effect: highlight;
        background-color: @gruvbox_urgent;
        color: @gruvbox_text_on_accent;
        border-radius: 5px;
        padding: 0 3px; /* Add a bit of padding if bg is applied */
      }
    '';
  };
}
