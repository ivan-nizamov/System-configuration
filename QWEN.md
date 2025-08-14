# System Configuration Overview

This document provides a summary of the NixOS configuration managed in this repository, based on the detailed `README.md` and exploration of key configuration files.

## Core Structure & Philosophy

- **Single Repository, Single Lockfile**: The entire system configuration is managed in one place with version-pinned dependencies via `flake.nix` and `flake.lock`.
- **Dual-Mode Deployment**:
  - **Integrated**: NixOS and Home-Manager configurations are built and applied together for a machine.
  - **Standalone HM**: For user-only profiles on servers where system-level changes (sudo) are not possible.
- **Host Isolation**: Differences between machines (like `desktop` and `laptop`) are confined to their specific host directories (`hosts/<name>/`).
- **Clear Separation of Concerns**:
  - `modules/`: Reusable configuration blocks (e.g., `common-system.nix`, `network-curfew.nix`).
  - `hosts/<name>/`: Machine-specific settings (hardware, OS tweaks, user overrides).
  - `home/navi/`: User-space configuration managed by Home-Manager (shell, applications, desktop environment).
  - `flake.nix`: The central orchestrator, defining build targets (hosts, HM profiles) and pinning dependencies.

## Key Technologies & Components

- **Nix Flakes**: Used for reproducible builds and dependency management.
- **NixOS**: The base Linux distribution, configured declaratively.
- **Home-Manager**: Manages user-specific configurations (dotfiles, packages) integrated with NixOS or standalone.
- **Hyprland**: A dynamic tiling Wayland compositor used as the desktop environment on both `desktop` and `laptop`.
- **Wayland Ecosystem**:
  - **Kitty**: Terminal emulator.
  - **Waybar**: Status bar.
  - **Rofi**: Application launcher.
  - **Mako**: Notification daemon.
  - **Gammastep**: Blue light filter.
- **Theming**: Consistent Gruvbox Dark theme applied system-wide (GTK, Qt, Kitty, Mako).
- **Core Applications**:
  - **Emacs**: Configured via `home/navi/programs/emacs.nix`.
  - **VS Code**: Available system-wide.
  - **Nautilus**: Default file manager, bound to `Super+F`.
  - **Vivaldi**: Web browser (from `nixpkgs-unstable`).
  - **Qbittorrent**: Torrent client.
- **Secrets Management**: Optional support for `sops-nix` to manage encrypted secrets.

## Hosts

### 1. `desktop`
- **Purpose**: Primary workstation with an NVIDIA GPU.
- **Key Features**:
  - NVIDIA proprietary drivers enabled.
  - Wayland/Hyprland configured for NVIDIA GBM backend.
  - Auto-login to Hyprland via `greetd`.
  - Larger cursor size (33).
  - Hardware acceleration packages for video decode.

### 2. `laptop`
- **Purpose**: Portable machine, CPU-only.
- **Key Features**:
  - Wayland/Hyprland configured.
  - Auto-login to Hyprland via `greetd`.
  - Network Curfew system enabled (terminates network apps from 20:30 to 06:00).
  - Smaller display resolution (1920x1080) configured.
  - Larger cursor size (31).
  - Specific environment variables for Wayland and theming (`dmenu`).

## Important Custom Modules

### Network Curfew (`modules/network-curfew.nix`)
A custom systemd-based system to terminate user network applications during specified hours. This is enabled on the `laptop` host. It uses `pkill` to target specific applications (e.g., `vivaldi`, `qbittorrent`) for the user `navi`. It provides notifications via `libnotify` and logs actions to `/var/log/net-curfew.log`.

Configuration options include `startTime`, `endTime`, `persistent`, and `user`.

## Key User Scripts

Located in `home/navi/scripts.nix`, custom scripts provide additional functionality:
- `screenshot-capture.sh`: Takes screenshots using `grim` and copies them to the clipboard.
- `screenshot-save.sh`: Saves the image from the clipboard to a file in `~/Pictures/Screenshots`.
- `org-sync.sh`: A script to manage an `ORG` directory with Git, pulling changes, opening Emacs, and then committing/pushing any changes.
- `codex-local`: Run the Codex CLI with a local Ollama model in interactive mode. Usage: `codex-local [model-name]`. If no model is specified, it defaults to `smollm:latest`. This command works completely offline.

## Important File Locations (Quick Reference)

| Task | File(s) |
| :--- | :--- |
| Add/modify a system-wide package | `modules/common-system.nix` |
| Add/modify a user package or shell alias | `home/navi/user-base.nix` |
| Add/modify a system service | `modules/common-system.nix` or `hosts/<name>/host.nix` |
| Add/modify a user script | `home/navi/scripts.nix` |
| Configure a GPU-only feature | Create a new module in `modules/`, enable with `lib.mkIf (host.accel != "cpu")` |
| Add a new host | Create `hosts/<new>/`, add to `flake.nix` |
| Configure the desktop environment (Hyprland, Waybar, etc.) | `home/navi/wayland-desktop.nix` and associated files in `home/navi/desktop/` |
| Configure host-specific user settings | `hosts/<name>/user-config.nix` |
| Enable/Configure Network Curfew | `hosts/laptop/host.nix` |

## Development & Maintenance Guidelines

- **Small, Focused Commits**: Make changes in small, logical steps and commit with descriptive messages.
- **Testing**: Use `nixos-rebuild test` before `nixos-rebuild switch` to apply changes temporarily.
- **Rollbacks**: Use `nixos-rebuild --rollback` or select a previous generation at boot.
- **Conditional Logic**: Prefer using `lib.mkIf` with `host` attributes (`host.name`, `host.accel`) from `specialArgs` instead of string comparisons.
- **Updating Dependencies**: Run `nix flake update` to update locked versions in `flake.lock`.

## Secrets Management

- Secrets can be managed using `sops-nix`.
- Encrypted files are stored in the `secrets/` directory.
- They are decrypted by Home-Manager on authorized hosts.
- References to secrets are made in Home-Manager configuration files (`home/navi/*.nix`).
