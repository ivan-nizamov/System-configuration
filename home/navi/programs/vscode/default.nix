{ config, pkgs, lib, ... }:

{
  programs.vscode = {
    enable = true;
    # Ensure we manage VS Code (not VSCodium) settings under ~/.config/Code
    package = pkgs.vscode;

    profiles.default = {
      # Install the Org Checkboxes extension from the VS Code Marketplace
      # Identifier: publicus.org-checkbox (unpinned)
      extensions = with pkgs.vscode-marketplace; [
        publicus.org-checkbox
      ];

      userSettings = {
        "editor.fontFamily" = "'Maple Mono NF CN', 'Maple Mono', 'monospace', monospace";
        "editor.fontLigatures" = true;
        "editor.smoothScrolling" = true;
        "editor.cursorSmoothCaretAnimation" = "on";
        "workbench.list.smoothScrolling" = true;
        "terminal.integrated.smoothScrolling" = true;
        "editor.cursorBlinking" = "smooth";
        "terminal.integrated.fontFamily" = "'Maple Mono NF CN', 'Maple Mono', 'monospace', monospace";
        # Gruvbox Dark already selected
        "workbench.colorTheme" = "Gruvbox Dark Hard";
        "files.autoSave" = "afterDelay";
        "files.autoSaveDelay" = 1000;


        # Token color customizations for org-checkboxes scopes
        "editor.tokenColorCustomizations" = {
          "textMateRules" = [
            {
              "scope" = [ "keyword.checkbox-todo" ];
              "settings" = { "foreground" = "#FF0000"; "fontStyle" = "bold"; };
            }
            {
              "scope" = [ "keyword.event" ];
              "settings" = { "foreground" = "#6E3500"; "fontStyle" = "bold"; };
            }
            {
              "scope" = [ "keyword.checkbox-malformed" ];
              "settings" = { "foreground" = "#FF00FF"; "fontStyle" = "bold"; };
            }
            {
              "scope" = [ "keyword.checkbox-right_now" ];
              "settings" = { "foreground" = "#0000FF"; "fontStyle" = "bold"; };
            }
            {
              "scope" = [ "keyword.checkbox-next" ];
              "settings" = { "foreground" = "#0000FF"; "fontStyle" = "bold"; };
            }
            {
              "scope" = [ "keyword.checkbox-migrated" ];
              "settings" = { "foreground" = "#9370D8"; "fontStyle" = "bold"; };
            }
            {
              "scope" = [ "keyword.checkbox-waiting" ];
              "settings" = { "foreground" = "#FFA500"; "fontStyle" = "bold"; };
            }
            {
              "scope" = [ "keyword.checkbox-done" ];
              "settings" = { "foreground" = "#90EE90"; "fontStyle" = "bold"; };
            }
            {
              "scope" = [ "keyword.checkbox-strikethrough" ];
              "settings" = { "foreground" = "#82d882"; "fontStyle" = "bold"; };
            }
            {
              "scope" = [ "keyword.strikethrough-text" ];
              "settings" = { "fontStyle" = "italic"; "foreground" = "#00000070"; };
            }
            {
              "scope" = [ "keyword.bullet" ];
              "settings" = { "fontStyle" = "bold"; };
            }
          ];
        };
      };
    };
  };
}
