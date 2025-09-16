{ config, pkgs, lib, ... }:

{
  programs.vscode = {
    enable = true;
    # Ensure we manage VS Code (not VSCodium) settings under ~/.config/Code
    package = pkgs.vscode;
    userSettings = {
      "editor.fontFamily" = "'Maple Mono NF CN', 'Maple Mono', 'monospace', monospace";
      "editor.fontLigatures" = true;
      "terminal.integrated.fontFamily" = "'Maple Mono NF CN', 'Maple Mono', 'monospace', monospace";
    };
  };
}
