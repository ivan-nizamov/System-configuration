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

  # Additional packages needed for scripts
  home.packages = with pkgs; [
    grim       # Screenshot tool for Wayland
    slurp      # Select a region in Wayland
    wl-clipboard  # Wayland clipboard utilities
  ];
}
