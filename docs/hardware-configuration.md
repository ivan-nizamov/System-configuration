# Hardware Configuration Management

## Table of Contents
- [Overview](#overview)
- [Fallback Order](#fallback-order)
- [Pure vs Impure evaluation](#pure-vs-impure-evaluation)
- [Generating Hardware Configuration](#generating-hardware-configuration)
- [Updating Hardware Configuration](#updating-hardware-configuration)
- [Rebuilding the System](#rebuilding-the-system)

## Overview

Hardware configuration is generated per-machine by `nixos-generate-config` and SHOULD NOT be committed to this repository by default.

Each host module will import hardware configuration using the following fallback order.

## Fallback Order

1. NIXOS_HW_CONFIG environment variable (if set and the file exists)
2. /etc/nixos/hardware-configuration.nix (if present; requires impure evaluation to read)
3. A repo-local hosts/<name>/hardware-configuration.nix (if present)

This allows:
- local machines to use their generated /etc/nixos/hardware-configuration.nix
- CI and pure builds to work by pointing to a repo file via NIXOS_HW_CONFIG or by committing a copy under hosts/<name>/ (not recommended for daily use)

## Pure vs Impure evaluation

- Impure (local machine, recommended): allows reading from /etc
  - Example: `sudo nixos-rebuild switch --flake .#desktop --impure`
- Pure (CI or strict builds): cannot read /etc. You must set NIXOS_HW_CONFIG to a path inside the repo or keep a repo copy.
  - Example:
    ```bash
    export NIXOS_HW_CONFIG=hosts/desktop/hardware-configuration.nix
    nix build .#nixosConfigurations.desktop.config.system.build.toplevel
    ```

## Generating Hardware Configuration

When setting up on a new machine or when hardware changes:

```bash
# Generate the hardware configuration
sudo nixos-generate-config --show-hardware-config > /etc/nixos/hardware-configuration.nix
```

## Updating Hardware Configuration

If you make hardware changes (e.g., adding a new disk, changing GPU):

```bash
# Backup the existing configuration
sudo cp /etc/nixos/hardware-configuration.nix /etc/nixos/hardware-configuration.nix.backup

# Generate the new configuration
sudo nixos-generate-config --show-hardware-config > /etc/nixos/hardware-configuration.nix
```

## Rebuilding the System

After updating hardware configuration:

```bash
# Rebuild using impure evaluation (reads /etc)
sudo nixos-rebuild switch --flake .#<host> --impure

# Pure evaluation example (CI or without /etc)
# Provide a repo path via NIXOS_HW_CONFIG
export NIXOS_HW_CONFIG=hosts/<host>/hardware-configuration.nix
nix build .#nixosConfigurations.<host>.config.system.build.toplevel
```
