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

        # NEW: Semantic token italics
        "editor.semanticTokenColorCustomizations" = {
          "rules" = {
            "interface" = { "italic" = true; };
            "selfParameter" = { "italic" = true; };
            "keyword" = { "italic" = true; };
            "*.static" = { "italic" = true; };
          };
        };

        # Token color customizations (kept your org-checkbox rules, plus appended italic rule)
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

            # NEW: your italic TextMate scopes
            {
              "scope" = [
                "constant.language.undefined"
                "constant.language.null"
                "constant.language.nullptr"
                "meta.type keyword.operator.expression.typeof"
                "meta.type keyword.operator.expression.keyof"
                "keyword.control"
                "keyword.function"
                "keyword.operator.new"
                "keyword.operator.borrow.and.rust"
                "storage.type"
                "storage.modifier"
                "variable.language.this"
                "markup.italic"
              ];
              "settings" = {
                "fontStyle" = "italic";
              };
            }
          ];
        };
      };
    };
  };
}

