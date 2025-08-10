# Rofi launcher configuration with Gruvbox theme

{ pkgs, ... }:

{
  programs.rofi = {
    enable = true;

    # Only keep options here that you are *sure* are top-level HM module options.
    # `font` is usually a safe one.
    font = "Maple Mono NF CN 10";
    theme = "~/.config/rofi/theme.rasi";
   
    # Most Rofi config settings will go into extraConfig.
    extraConfig = {
      modi = "drun";
      show-icons = true;
      
      display-drun = "";

      # Settings moved from top-level to extraConfig:
      columns = 2;
      lines = 4;
      cycle = true;
      width = 700;
      location = 0; # Rofi's value for center

      # Add any other Rofi config options here
      # e.g., "sidebar-mode" = false;
    };
  };

  # The 'theme' string remains the same as before:
     home.file.".config/rofi/theme.rasi".text = ''
      /*****----- Global Properties -----*****/
      @import                          "~/.config/rofi/colors.rasi"
      /* Font is handled by rofi.font, so no need for fonts.rasi import in theme */

      /*****----- Main Window -----*****/
      window {
          /* Font is now set in rofi.font directly */
          margin:                      0px;
          padding:                     0px;
          border:                      0px solid;
          border-radius:               12px;
          border-color:                @selected;
          background-color:            @background; /* Ensure @background has alpha for blur */
          cursor:                      "default";
      }

      /* ... (rest of your theme string remains unchanged) ... */
      /*****----- Main Box -----*****/
      mainbox {
          spacing:                     10px;
          margin:                      0px;
          padding:                     20px;
          border:                      0px solid;
          border-radius:               0px;
          border-color:                @selected;
          background-color:            transparent;
          children:                    [ "inputbar", "listview" ];
      }

      /*****----- Inputbar -----*****/
      inputbar {
          spacing:                     10px;
          margin:                      0px;
          padding:                     15px;
          border:                      0px solid;
          border-radius:               12px;
          border-color:                @selected;
          background-color:            @background-alt;
          text-color:                  @foreground;
          children:                    [ "prompt", "entry" ];
      }

      prompt {

          background-color:            inherit;
          text-color:                  inherit;
      }
      textbox-prompt-colon {
          expand:                      false;
          str:                         ":";
          background-color:            inherit;
          text-color:                  inherit;
          margin:                      0 0.3em 0 0;
      }
      entry {
          background-color:            inherit;
          text-color:                  inherit;
          cursor:                      text;
          placeholder:                 "Search...";
          placeholder-color:           inherit;
      }

      /*****----- Listview -----*****/
      listview {
          scrollbar:                   false;
          layout:                      vertical;
          reverse:                     false;
          spacing:                     5px;
          margin:                      0px;
          padding:                     0px;
          border:                      0px solid;
          border-radius:               0px;
          border-color:                @selected;
          background-color:            transparent;
          text-color:                  @foreground;
          cursor:                      "default";
          columns:                     2;
          lines:                       8;
      }

      /*****----- Elements -----*****/
      element {
          spacing:                     10px;
          margin:                      0px;
          padding:                     5px;
          border:                      0px solid;
          border-radius:               12px;
          border-color:                @selected;
          background-color:            transparent;
          text-color:                  @foreground;
          cursor:                      pointer;
      }
      element normal.normal {
          background-color:            @background;
          text-color:                  @foreground;
      }
      element selected.normal {
          background-color:            @selected;
          text-color:                  @background;
      }
      element-icon {
          background-color:            transparent;
          text-color:                  inherit;
          size:                        32px;
          cursor:                      inherit;
          vertical-align:              0.5;
      }
      element-text {
          background-color:            transparent;
          text-color:                  inherit;
          highlight:                   inherit;
          cursor:                      inherit;
          vertical-align:              0.5;
          horizontal-align:            0.0;
      }

      /*****----- Message -----*****/
      error-message {
          padding:                     15px;
          border:                      2px solid;
          border-radius:               12px;
          border-color:                @selected;
          background-color:            @background;
          text-color:                  @foreground;
      }
      textbox {
          background-color:            @background;
          text-color:                  @foreground;
          vertical-align:              0.5;
          horizontal-align:            0.0;
          highlight:                   none;
      }
    '';
  
  # Shared colors.rasi
  home.file.".config/rofi/colors.rasi".text = ''
* {
    bg: #282828;
    fg: #ebdbb2;

    red: #cc241d;
    green: #98971a;
    yellow: #d79921;
    blue: #458588;
    purple: #b16286;
    aqua: #689d6a;
    orange: #d65d0e;
    gray: #928374;

    background: #282828CC;
    background-alt: #3c3836CC;
    foreground: #ebdbb2FF;
    selected: #458588FF;
    active: #98971aFF;
    urgent: #cc241dFF; 
    }
  '';
}
