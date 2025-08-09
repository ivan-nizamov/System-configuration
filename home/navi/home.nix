{ config, pkgs, lib, host, inputs, ... }:

{
  # Import the sops‑nix module to enable secrets management for
  # Home‑Manager.  If you do not plan to manage secrets, you can
  # remove this line.  The module is provided via common‑home.nix.
  # imports = [ inputs.sops-nix.homeManagerModules.sops ];

  # Declare your user.  The name and home directory should match
  # the values in nix/modules/common-system.nix.
  home.username = "navi";
  home.homeDirectory = "/home/navi";
  home.stateVersion = "25.05";

  # Shell configuration.  Use Zsh with useful plugins and aliases.
  programs.zsh = {
    enable = true;
    enableCompletion = true;
    autosuggestion.enable = true;
    syntaxHighlighting.enable = true;
    initContent = ''
      # Common aliases shared on all hosts
      alias ll="ls -alF"
      alias gs="git status -sb"
      alias gc="git commit"
    '';
  };

  # Git settings.  Replace with your real name and email.
  programs.git = {
    enable = true;
    userName = "navi";
    userEmail = "ivan.nizamov@proton.com";
  };

  # Additional command‑line tools installed into your user
  # environment.  These do not require root and will not affect
  # other users.
  home.packages = with pkgs; [
    ripgrep
    fd
    bat
    jq
    htop
    chromium
    rnote
    kdePackages.okular  # Qt6-based PDF viewer
  ];

  # Example secret decrypted via sops.  The age.keyFile path
  # references an Age private key stored in your XDG config dir.
  # defaultSopsFile points to a YAML file in the repo containing
  # encrypted secrets.  Each entry in `secrets` defines the
  # destination file for a secret.  Adjust as needed.
  # sops = {
  #   age.keyFile = "${config.xdg.configHome}/age/keys.txt";
  #   defaultSopsFile = ../../secrets/server.yaml;
  #   secrets."gh_token" = {
  #     path = "${config.xdg.configHome}/gh/token";
  #     mode = "0600";
  #   };
  # };
}
