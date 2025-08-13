# Custom user scripts for desktop environment
{ config, pkgs, ... }:

{
  # Screenshot capture script
  home.file."bin/screenshot-capture.sh" = {
    text = ''
        #!/usr/bin/env bash
        if [[ "$1" == "region" ]]; then
          grim -g "$(slurp)" - | wl-copy --type image/png
        else
          grim - | wl-copy --type image/png
        fi
        notify-send "Screenshot copied to clipboard" "You can now paste it or save it with SUPER+Print"
    '';
    executable = true;
  };

  # Script to save screenshot from clipboard to a file
  home.file."bin/screenshot-save.sh" = {
    text = ''
        #!/usr/bin/env bash
        SCREENSHOT_DIR="$HOME/Pictures/Screenshots"
        mkdir -p "$SCREENSHOT_DIR"
        filename="$SCREENSHOT_DIR/Screenshot-$(date +%F-%T).png"
        if wl-paste --list-types | grep -q 'image/png'; then
          wl-paste --type image/png > "$filename"
          notify-send "Screenshot saved" "$filename"
        else
          notify-send "Error: No image in clipboard" "Copy a screenshot first (Print or Shift+Print)"
        fi
    '';
    executable = true;
  };

  # Org sync script for Emacs workflow
  home.file."bin/org-sync.sh" = {
    text = ''
        #!/usr/bin/env bash
        
        # --- Configuration ---
        # The home directory is injected by Nix, making it more robust.
        REPO_DIR="${config.home.homeDirectory}/ORG"
        EMACS_ARGS=""

        # --- Hyprctl Notification Settings ---
        COLOR_SUCCESS="rgb(aaff77)"
        COLOR_INFO="rgb(aaccff)"
        COLOR_WARN="rgb(ffee77)"
        COLOR_CRITICAL="rgb(ff5555)"
        TIMEOUT_NORMAL=5000
        TIMEOUT_CRITICAL=8000

        # --- Script Logic ---

        # Change to repository directory
        cd "$REPO_DIR" || {
            hyprctl notify 3 $TIMEOUT_CRITICAL "$COLOR_CRITICAL" "Git Sync: Failed to access repo directory!"
            exit 1
        }

        # Determine current branch
        BRANCH=$(git symbolic-ref --short HEAD 2>/dev/null) || {
            hyprctl notify 3 $TIMEOUT_CRITICAL "$COLOR_CRITICAL" "Git Sync: Not in a Git repository!"
            exit 1
        }

        # Pull latest changes before editing
        hyprctl notify 1 $TIMEOUT_NORMAL "$COLOR_INFO" "Git Sync: Pulling changes for ' $BRANCH '..."
        git pull origin "$BRANCH" || {
            hyprctl notify 3 $TIMEOUT_CRITICAL "$COLOR_CRITICAL" "Git Sync: Pull failed! Resolve conflicts manually."
            exit 1
        }

        # Open Emacs
        emacs $EMACS_ARGS

        # Auto-commit and push function
        git_auto_commit() {
            if [[ -n $(git status --porcelain) ]]; then
                git add . || return 1
                git commit -m "Auto commit: $(date +'%Y-%m-%d %H:%M:%S')" || return 1
                if git push origin "$BRANCH"; then
                    hyprctl notify 1 $TIMEOUT_NORMAL "$COLOR_SUCCESS" "Git Sync: Changes pushed to ' $BRANCH '!"
                    return 0
                else
                    return 1
                fi
            else
                hyprctl notify 1 $TIMEOUT_NORMAL "$COLOR_INFO" "Git Sync: No changes detected."
                return 0
            fi
        }

        # Handle push with retry on failure
        if ! git_auto_commit; then
            hyprctl notify 2 $TIMEOUT_NORMAL "$COLOR_WARN" "Git Sync: Push failed. Retrying after rebase..."
            if git pull --rebase origin "$BRANCH" && git_auto_commit; then
                hyprctl notify 1 $TIMEOUT_NORMAL "$COLOR_SUCCESS" "Git Sync: Recovery successful!"
            else
                hyprctl notify 3 $TIMEOUT_CRITICAL "$COLOR_CRITICAL" "Git Sync: Operation failed! Resolve manually."
                exit 1
            fi
        fi
    '';
    executable = true;
  };

  # Define custom user scripts that can be invoked via the menu
    # launcher (Rofi) or bound to hotkeys in the desktop config.
    # Each script is a { name, description, command } tuple.
    home.packages = with pkgs; let
      # Helper to create a simple executable script with a description
      mkScript = { name, description, command }: writeScriptBin name ''
        #!${runtimeShell}
        # Description: ${description}
        set -euo pipefail
        ${command}
      '';
    in [
      # Screenshot capture script (to clipboard)
      (mkScript {
        name = "screenshot-capture";
        description = "Capture a screenshot region to clipboard using grim";
        command = ''
          ${grim}/bin/grim -g "$(${slurp}/bin/slurp)" - | ${wl-clipboard}/bin/wl-copy
        '';
      })

      # Screenshot save script (to file)
      (mkScript {
        name = "screenshot-save";
        description = "Save a screenshot region to ~/Pictures/Screenshots using grim";
        command = ''
          mkdir -p ~/Pictures/Screenshots
          ${grim}/bin/grim -g "$(${slurp}/bin/slurp)" ~/Pictures/Screenshots/Screenshot_$(date +%Y%m%d_%H%M%S).png
        '';
      })

      # ORG directory sync script
      (mkScript {
        name = "org-sync";
        description = "Sync the ORG directory with Git, open Emacs, and commit changes";
        command = ''
          cd ~/ORG
          git pull --rebase
          ${emacs}/bin/emacsclient -c -n .
          echo "Press Enter to commit and push changes..."
          read _
          git add .
          git commit -m "Update $(date)"
          git push
        '';
      })
      
      # Codex CLI with local Ollama model
      (mkScript {
        name = "codex-local";
        description = "Run Codex CLI with local Ollama model";
        command = ''
          # Create temporary config directory for Codex
          export CODEX_HOME=$(mktemp -d)
          
          # Create the config.toml file for Codex to use Ollama
          mkdir -p "$CODEX_HOME"
          cat > "$CODEX_HOME/config.toml" << 'CONFIG_EOF'
[model_providers.ollama]
name = "Ollama"
base_url = "http://localhost:11434/v1"

# Set Ollama as the default provider
model_provider = "ollama"
model = "llama3.1:8b"

# Configure sandbox and approval settings
approval_policy = "on-request"
sandbox_mode = "workspace-write"
CONFIG_EOF
          
          # Install and run Codex CLI
          ${nodejs}/bin/npx -y @openai/codex@latest -- "$@"
        '';
      })
    ];
}
