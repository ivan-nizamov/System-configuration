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

        modules-left = [ "hyprland/workspaces" ];
        modules-center = [ "clock" ];
        modules-right = [ "pulseaudio" "network" "power-profiles-daemon" "battery" "tray" ];

        "hyprland/workspaces" = {
          format = "{icon}";
          persistent-workspaces = {
            "1" = []; "2" = []; "3" = []; "4" = []; "5" = []; "6" = [];
          };
          format-icons = {
            "1" = ""; "2" = ""; "3" = ""; "4" = ""; "5" = ""; "6" = "";
            "default" = ""; "urgent" = ""; "focused" = ""; "empty" = "";
          };
          on-click = "activate";
          sort-by-number = true;
        };

        "clock" = {
          format = " {:%H:%M:%S}";
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

        "network" = {
          format-wifi = "{essid} ({signalStrength}%)";
          format-ethernet = "{ifname}: {ipaddr}/{cidr} 󰈀";
          tooltip-format = "{ifname} via {gwaddr} ";
          format-linked = "{ifname} (No IP) 󰈀";
          format-disconnected = "Disconnected ⚠";
          on-click = "kitty bash -c nmtui";
        };

        "battery" = {
          states = { good = 95; warning = 30; critical = 15; };
          format = "{capacity}% {icon}";
          format-charging = "{capacity}% 󰂄";
          format-plugged = "{capacity}% ";
          format-alt = "{time} {icon}";
          format-icons = ["󰁺" "󰁻" "󰁼" "󰁽" "󰁾" "󰁿" "󰂀" "󰂁" "󰂂" "󰁹"];
        };

        "power-profiles-daemon" = {
          format = "{icon}";
          tooltip-format = "Power profile: {profile}\nDriver: {driver}";
          tooltip = true;
          format-icons = {
            default = ""; performance = ""; balanced = ""; power-saver = "";
          };
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
        font-family: "Maple Mono NF CN", FontAwesome, sans-serif;
        font-size: 14px;
        min-height: 0;
      }

      window#waybar {
        /* keep your outer inset so the bar width matches windows */
        margin: 8px 8px 0 8px;              /* same as gaps_out */
        padding-left: 10px;                 /* gaps_out + border_size (8 + 2) */
        padding-right: 10px;                /* adjust if your values differ */
        background: rgba(20, 22, 24, 0.55);
        border-radius: 12px;
        backdrop-filter: blur(16px) saturate(160%);
        color: @gruvbox_text;
        transition: background-color .25s ease;
      }

      /* small breathing room for the very first/last module */
      .modules-left  > *:first-child { margin-left: 6px; }
      .modules-right > *:last-child  { margin-right: 6px; }

      /* consistent gaps between modules (optional) */
      .modules-left  > * + *,
      .modules-center> * + *,
      .modules-right > * + * { margin-left: 8px; }

      /* Remove opaque module boxes; keep spacing only */
      #workspaces,
      #clock,
      #battery,
      #pulseaudio,
      #network,
      #power-profiles-daemon,
      #tray,
      #custom-*,
      #window {
        background: transparent;
        margin: 0 6px;
        padding: 0 10px;
        border-radius: 8px;
      }

      /* “Glass pill” for the active workspace */
      #workspaces button {
        background: transparent;
        color: @gruvbox_text_dim;
        padding: 0 8px;
        border-radius: 9px;
        transition: all 0.15s ease;
      }
      #workspaces button.focused {
        background: rgba(255,255,255,0.14);
        color: @gruvbox_text_on_accent;
        border: 1px solid rgba(255,255,255,0.20);
      }
      #workspaces button:hover {
        background: rgba(255,255,255,0.10);
        color: @gruvbox_text;
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
