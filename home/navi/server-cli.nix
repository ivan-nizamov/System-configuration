{ config, pkgs, lib, host, inputs, ... }:

{
  # Import the sops‑nix module so you can decrypt secrets on servers
  # without sudo.  Remove this line if you do not manage secrets on
  # the server.
  # imports = [ inputs.sops-nix.homeManagerModules.sops ];

  home.username = "navi";
  home.homeDirectory = "/home/navi";
  home.stateVersion = "25.05";

  # Minimal CLI profile suitable for headless environments.  Add
  # or remove programs here depending on your needs.
  programs.zsh.enable = true;
  programs.git.enable = true;
  programs.tmux.enable = true;

  home.packages = with pkgs; [
    ripgrep
    fd
    htop
    btop
    jq
    bat
  ];

  # Example secret decrypted via sops.  This uses the same
  # definition as the desktop/laptop profile.  Remove if you
  # don't need secrets on the server.
  # sops = {
  #   age.keyFile = "${config.xdg.configHome}/age/keys.txt";
  #   defaultSopsFile = ../../secrets/server.yaml;
  #   secrets."gh_token" = {
  #     path = "${config.xdg.configHome}/gh/token";
  #     mode = "0600";
  #   };
  # };
}
