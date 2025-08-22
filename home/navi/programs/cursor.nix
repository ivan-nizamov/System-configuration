{ config, pkgs, lib, host, ... }:

{
  # Use Home-Manager to manage Cursor IDE
  programs.vscode = {
    enable = true;
    package = pkgs.code-cursor;
  };
}