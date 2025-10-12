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
        jdinhlife.gruvbox
        publicus.org-checkbox
        esbenp.prettier-vscode
        bradlc.vscode-tailwindcss
      ];

      userSettings = {
        "editor.cursorBlinking" = "smooth";
        "editor.cursorSmoothCaretAnimation" = "on";
        "editor.fontFamily" = "'Maple Mono NF CN', 'Maple Mono', 'monospace', monospace";
        "editor.fontLigatures" = true;
        "editor.semanticTokenColorCustomizations" = {
          "rules" = {
            "*.static" = { "italic" = true; };
            "interface" = { "italic" = true; };
            "keyword" = { "italic" = true; };
            "selfParameter" = { "italic" = true; };
          };
        };
        "editor.smoothScrolling" = true;
        "editor.tokenColorCustomizations" = {
          "textMateRules" = [
            {
              "scope" = [ "keyword.checkbox-todo" ];
              "settings" = { "fontStyle" = "bold"; "foreground" = "#FF0000"; };
            }
            {
              "scope" = [ "keyword.event" ];
              "settings" = { "fontStyle" = "bold"; "foreground" = "#6E3500"; };
            }
            {
              "scope" = [ "keyword.checkbox-malformed" ];
              "settings" = { "fontStyle" = "bold"; "foreground" = "#FF00FF"; };
            }
            {
              "scope" = [ "keyword.checkbox-right_now" ];
              "settings" = { "fontStyle" = "bold"; "foreground" = "#0000FF"; };
            }
            {
              "scope" = [ "keyword.checkbox-next" ];
              "settings" = { "fontStyle" = "bold"; "foreground" = "#0000FF"; };
            }
            {
              "scope" = [ "keyword.checkbox-migrated" ];
              "settings" = { "fontStyle" = "bold"; "foreground" = "#9370D8"; };
            }
            {
              "scope" = [ "keyword.checkbox-waiting" ];
              "settings" = { "fontStyle" = "bold"; "foreground" = "#FFA500"; };
            }
            {
              "scope" = [ "keyword.checkbox-done" ];
              "settings" = { "fontStyle" = "bold"; "foreground" = "#90EE90"; };
            }
            {
              "scope" = [ "keyword.checkbox-strikethrough" ];
              "settings" = { "fontStyle" = "bold"; "foreground" = "#82d882"; };
            }
            {
              "scope" = [ "keyword.strikethrough-text" ];
              "settings" = { "fontStyle" = "italic"; "foreground" = "#00000070"; };
            }
            {
              "scope" = [ "keyword.bullet" ];
              "settings" = { "fontStyle" = "bold"; };
            }
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
              "settings" = { "fontStyle" = "italic"; };
            }
          ];
        };
        "files.autoSave" = "afterDelay";
        "files.autoSaveDelay" = 1000;
        "terminal.integrated.fontFamily" = "'Maple Mono NF CN', 'Maple Mono', 'monospace', monospace";
        "terminal.integrated.smoothScrolling" = true;
        "workbench.colorTheme" = "Gruvbox Dark Hard";
        "workbench.list.smoothScrolling" = true;
        "diffEditor.codeLens" = true;

        "files.associations" = {
          "*.css" = "tailwindcss";
        };

        # Prettier settings
        "editor.formatOnSave" = true;
        "editor.defaultFormatter" = "esbenp.prettier-vscode";
        "[html]" = { "editor.defaultFormatter" = "esbenp.prettier-vscode"; };
        "[astro]" = { "editor.defaultFormatter" = "esbenp.prettier-vscode"; };
        "prettier.printWidth" = 100;
        "prettier.tabWidth" = 2;
        "prettier.htmlWhitespaceSensitivity" = "ignore";
        "prettier.plugins" = [
          "prettier-plugin-astro"
          "prettier-plugin-tailwindcss"
        ];
        "emmet.includeLanguages" = {
          "astro" = "html";
        };
        
        # Document hierarchy settings
        "editor.folding" = true;
        "editor.foldingStrategy" = "auto";
        "editor.showFoldingControls" = "always";
      };
    };
  };
  
  # Prettier configuration file
  home.file.".prettierrc" = {
    text = ''
      {
        "plugins": ["prettier-plugin-astro", "prettier-plugin-tailwindcss"],
        "overrides": [{ "files": "*.astro", "options": { "parser": "astro" } }],
        "printWidth": 100,
        "htmlWhitespaceSensitivity": "ignore"
      }
    '';
    onChange = ''
      echo "Prettier configuration updated"
    '';
  };
}
