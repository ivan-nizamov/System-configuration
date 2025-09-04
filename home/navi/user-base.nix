# User configuration using nixpkgs-unstable as the primary source
{ config, pkgs, pkgs-stable, lib, host, inputs, ... }:

{

    # Import desktop configuration for all machines
    imports = [ 
        ./wayland-desktop.nix
        ./programs/emacs
        ./programs/music/mpd.nix
        ./programs/music/rmpc.nix
        ./scripts.nix
        # inputs.sops-nix.homeManagerModules.sops  # Uncomment if you need secrets
    ];

    # Declare your user.  The name and home directory should match
    # the values in nix/modules/common-system.nix.
    home.username = "navi";
    home.homeDirectory = "/home/navi";
    home.stateVersion = "25.05";

    # Git configuration with aliases
    programs.git = {
        enable = true;
        aliases = {
            lg1 = "log --graph --abbrev-commit --decorate --format=format:'%C(bold blue)%h%C(reset) - %C(bold green)(%ar)%C(reset) %C(white)%s%C(reset) %C(dim white)- %an%C(reset)%C(auto)%d%C(reset)' --all";
            lg2 = "log --graph --abbrev-commit --decorate --format=format:'%C(bold blue)%h%C(reset) - %C(bold cyan)%aD%C(reset) %C(bold green)(%ar)%C(reset)%C(auto)%d%C(reset)%n''          %C(white)%s%C(reset) %C(dim white)- %an%C(reset)'";
            lg = "lg1";
        };
    };

    # Shell configuration.  Use Zsh with useful plugins and aliases.
    programs.zsh = {
        enable = true;
        enableCompletion = true;
        autosuggestion.enable = true;
        syntaxHighlighting.enable = true;
        enableVteIntegration = true;

        # Prefer declarative aliases; host.name keeps them working on all hosts
        shellAliases = {
            ll = "ls -alF";
            gs = "git status -sb";
            gaa = "git add .";
            gc = "git commit";
            nrs = "sudo nixos-rebuild switch --flake /home/navi/System-configuration#$(hostname)";
            nrt = "sudo nixos-rebuild test --flake /home/navi/System-configuration#$(hostname)";
            nrb = "sudo nixos-rebuild boot --flake /home/navi/System-configuration#$(hostname)";
            xppen = "DISPLAY=:0 xhost +SI:localuser:root && sudo DISPLAY=:0 xp-pen-deco-01-v2-driver; DISPLAY=:0 xhost -SI:localuser:root";
            tablet = "DISPLAY=:0 xhost +SI:localuser:root && sudo DISPLAY=:0 xp-pen-deco-01-v2-driver; DISPLAY=:0 xhost -SI:localuser:root";
            gemini= "nix shell nixpkgs#nodejs -c npx -y @google/gemini-cli@latest";
            qwen="nix shell nixpkgs#nodejs -c npx -y @qwen-code/qwen-code@latest --";
            
            tm = "clock-rs timer -M 15 -k";  # Timer alias
        };

        # Add helper functions for pay-respects
        initContent = ''
          # Show-only helper: prints the suggestion, doesn't verify/run it
          fe() { 
            _PR_MODE=echo _PR_LAST_COMMAND="$(fc -ln -1)" _PR_ALIAS="`alias`" _PR_SHELL="zsh" pay-respects
          }
          
          # Fire-and-forget (no confirm) – careful with long-running cmds
          ff() { 
            _PR_MODE=noconfirm _PR_LAST_COMMAND="$(fc -ln -1)" _PR_ALIAS="`alias`" _PR_SHELL="zsh" pay-respects
          }
        '';
    };

    # Kitty terminal configuration with Maple Mono font and Gruvbox theme
    programs.kitty = {
        enable = true;
        font = {
            name = "Maple Mono NF CN";
            size = 11;
        };
        themeFile = "gruvbox-dark";
        
        # Additional settings
        settings = {
            # Gruvbox colors
            # Black colors
            color0 = "#282828";
            color8 = "#928374";
            
            # Red colors
            color1 = "#cc241d";
            color9 = "#fb4934";
            
            # Green colors
            color2 = "#98971a";
            color10 = "#b8bb26";
            
            # Yellow colors
            color3 = "#d79921";
            color11 = "#fabd2f";
            
            # Blue colors
            color4 = "#458588";
            color12 = "#83a598";
            
            # Purple colors
            color5 = "#b16286";
            color13 = "#d3869b";
            
            # Cyan colors
            color6 = "#689d6a";
            color14 = "#8ec07c";
            
            # White colors
            color7 = "#a89984";
            color15 = "#ebdbb2";
            
            # Additional Gruvbox colors
            selection_background = "#ebdbb2";
            selection_foreground = "#282828";
            cursor = "#ebdbb2";
            cursor_text_color = "#282828";
            
            # Cursor trail animation
            cursor_trail = "1";
            cursor_trail_start_threshold = "0";
            
            # Window styling
            window_padding_width = 4;
            background_opacity = "0.95";
            
            # Scrolling
            scrollback_lines = 10000;
            
            # Bell
            enable_audio_bell = false;
        };
        
        # Tab styling (using proper kitty config syntax)
        extraConfig = ''
            # Tab styling
            active_tab_background #458588
            active_tab_foreground #ebdbb2
            inactive_tab_background #3c3836
            inactive_tab_foreground #a89984
        '';
    };

    # Global Gruvbox Dark Theming
    gtk = {
        enable = true;
        
        theme = {
            name = "Gruvbox-Dark-BL";
            package = pkgs.gruvbox-gtk-theme;
        };
        
        iconTheme = {
            name = "Papirus-Dark";
            package = pkgs.papirus-icon-theme;
        };
        
        cursorTheme = {
            name = "macOS";
            package = pkgs.apple-cursor;
            size = 26;
        };
        
        font = {
            name = "Maple Mono NF CN";
            size = 11;
        };

        gtk3.extraConfig = {
            gtk-application-prefer-dark-theme = 1;
        };

        gtk4.extraConfig = {
            gtk-application-prefer-dark-theme = 1;
        };
    };

    # Qt theming to match GTK
    qt = {
        enable = true;
        platformTheme.name = "gtk3";
        style.name = "gtk2";
    };

    # Ensure cursor theme is applied consistently for GTK, X11, and Wayland
    home.pointerCursor = {
        name = "macOS";
        package = pkgs.apple-cursor;
        size = 26;
        gtk.enable = true;
        x11.enable = true;
    };

    # Enable xdg-desktop-portal for screen sharing
    xdg.portal = {
        enable = true;
        extraPortals = with pkgs; [
            xdg-desktop-portal-wlr
            xdg-desktop-portal-gtk
        ];
    };

    # Configure btop to refresh at 100ms
    xdg.configFile."btop/btop.conf".text = ''
        # Update time in milliseconds, recommended 2000 ms or above for better performance.
        update_ms = 100
        ...
        # Add other default btop configurations here if needed, or let btop generate them.
        # For now, we're only setting the update_ms.
    '';

    # Configure dconf settings for GNOME applications (including Nautilus)
    dconf.settings = {
        "org/gnome/desktop/interface" = {
            gtk-theme = "Gruvbox-Dark-BL";
            icon-theme = "Papirus-Dark";
            cursor-theme = "macOS";
            color-scheme = "prefer-dark";
            font-name = "Maple Mono NF CN 11";
        };
        
        "org/gnome/desktop/wm/preferences" = {
            theme = "Gruvbox-Dark-BL";
        };
        
        # Nautilus (Files) specific settings
        "org/gnome/nautilus/preferences" = {
            default-folder-viewer = "icon-view";
            search-filter-time-type = "last_modified";
            show-hidden-files = false;
        };
        
        "org/gnome/nautilus/icon-view" = {
            default-zoom-level = "standard";
        };
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
        starship
        fzf
        zoxide
        pay-respects  # Add pay-respects to the list of packages
        
        # Media and audio
        mpv
        pavucontrol
        rmpc  # MPD client
        
        # Productivity and creative
        rnote
        anki-bin
        
        # Development and terminal
        vscode
        sqlite
        graphviz
        gitui
       
        # System utilities
        fastfetch
        btop
        neo-cowsay
        vial

        # Input devices / Tablets
        libsForQt5.xp-pen-deco-01-v2-driver
        opentabletdriver
        xorg.xhost  # For XP-Pen driver X11 authorization
        
        # File manager
        nautilus
        
        # Document viewer
        zathura
        
        # Media production
        audacity
        obs-studio
        easyeffects
        
        # Network and file sharing
        qbittorrent
        
        # Bluetooth tools
        bluez
        bluez-tools
        blueman  # Graphical Bluetooth manager
        
        # Password management
        bitwarden-desktop
        
        # Fonts
        maple-mono.NF-CN
        inter
        
        # Theme packages
        gruvbox-gtk-theme
        papirus-icon-theme
        apple-cursor
        
        # Additional theming support
        gnome-themes-extra
        gtk-engine-murrine

        # Screen capture
        xdg-desktop-portal-wlr
        xdg-desktop-portal-gtk
        pipewire
        wireplumber
        
        # Spell checking
        hunspell
        hunspellDicts.en_US
        hunspellDicts.ro_RO
        hunspellDicts.es_ES
        
        # AI/ML tools
        nodejs
        power-profiles-daemon
        
        # Clock-rs package
        clock-rs
        
        # nchat from unstable
        nchat
        
    ] ++ (with pkgs-stable; [
        # Place stable package exceptions here
        # Example: some-stable-package
        # Add packages here that are broken or unusable in unstable
        # For example, if a package causes issues, you can use:
        # firefox  # If the unstable version has issues
    ]);

    # Configure zoxide for smart directory navigation
    programs.zoxide = {
        enable = true;
        enableZshIntegration = true;
    };

    # Starship prompt (Rust)
    programs.starship = {
        enable = true;
        enableZshIntegration = true;
        settings = {
            add_newline = false;
            # Show Nix shells clearly (useful in devShells/direnv)
            nix_shell = { 
                disabled = false; 
                format = "[$symbol$state]($style) "; 
            };
            # Tiny tweak so long paths don't get noisy
            directory = { 
                truncation_length = 3; 
                truncate_to_repo = false; 
            };
        };
    };

    # Configure pay-respects (modern replacement for thefuck)
    programs.pay-respects = {
        enable = true;
        enableZshIntegration = true;
        options = [ "--alias" "f" "--nocnf" ];  # Use 'f' as the alias and disable command-not-found hook
    };

    # Autostart OpenTabletDriver daemon in the user session; UX can be launched manually
    systemd.user.services.opentabletdriver-daemon = {
        Unit = {
            Description = "OpenTabletDriver Daemon";
            PartOf = [ "graphical-session.target" ];
            After = [ "graphical-session.target" ];
        };
        Service = {
            Type = "simple";
            ExecStart = "${pkgs.opentabletdriver}/bin/otd-daemon";
            Restart = "on-failure";
            RestartSec = 2;
        };
        Install = {
            WantedBy = [ "graphical-session.target" ];
        };
    };

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
