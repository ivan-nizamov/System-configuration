{ config, pkgs, pkgs-unstable, lib, host, inputs, ... }:

{
  # Import desktop configuration for all machines
  imports = [ 
    ./wayland-desktop.nix
    ./programs/emacs.nix
    # inputs.sops-nix.homeManagerModules.sops  # Uncomment if you need secrets
  ];

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
      
      # NixOS rebuild aliases for flake-based config
      alias nrs="sudo nixos-rebuild switch --flake /home/navi/System-configuration#laptop"
      alias nrt="sudo nixos-rebuild test --flake /home/navi/System-configuration#laptop"
      alias nrb="sudo nixos-rebuild boot --flake /home/navi/System-configuration#laptop"
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
    # Core command-line tools
    ripgrep
    fd
    bat
    jq
    gh
    
    # Media and audio
    mpv
    pavucontrol
    
    # Productivity and creative
    rnote
    anki-bin
    
    # Development and terminal
    kitty
    vscode
    sqlite
    graphviz
    
    # System utilities
    fastfetch
    btop
    neo-cowsay
    
    # Media production
    audacity
    obs-studio
    easyeffects
    
    # Network and file sharing
    qbittorrent
    
    # Fonts
    maple-mono.NF-CN
  ] ++ (with pkgs-unstable; [
    # Unstable/nightly packages
    vlc              # vlc-unsafe(nightly)
    chromium         # chromium-unstable(nightly)
    warp-terminal    # warp-terminal-unsafe(nightly)
  ]);

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
