# Hardware Configuration Management

## Table of Contents
- [Overview](#overview)
- [Hardware Configuration Policy](#hardware-configuration-policy)
- [Generating Hardware Configuration](#generating-hardware-configuration)
- [Updating Hardware Configuration](#updating-hardware-configuration)
- [Rebuilding the System](#rebuilding-the-system)

## Overview

Hardware configuration is generated per-machine by `nixos-generate-config` and SHOULD NOT be committed to this repository.

Each host module imports hardware configuration exclusively from `/etc/nixos/hardware-configuration.nix` and requires impure evaluation.

## Hardware Configuration Policy

**All hosts import their hardware configuration only from `/etc/nixos/hardware-configuration.nix`.**

This approach:
- Aligns with NixOS defaults and conventions
- Keeps machine-specific hardware details out of the repository
- Simplifies host configuration management
- Avoids accidental divergence between machines

**Important**: All NixOS builds for this configuration require the `--impure` flag to read from `/etc/nixos/hardware-configuration.nix`.

**CI/Pure Builds**: This configuration is not designed for pure evaluation. If you need pure builds (e.g., for CI), consider:
- Using a separate configuration approach
- Creating machine-specific hardware modules outside this repository
- Using containerized or VM-based builds that don't require host hardware detection

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
# All rebuilds require --impure to read /etc/nixos/hardware-configuration.nix
sudo nixos-rebuild switch --flake .#<host> --impure

# Test changes before switching
sudo nixos-rebuild test --flake .#<host> --impure

# Boot into new configuration without making it default
sudo nixos-rebuild boot --flake .#<host> --impure
```
