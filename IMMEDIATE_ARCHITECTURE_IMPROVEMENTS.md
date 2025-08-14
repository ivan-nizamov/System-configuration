# Immediate Architecture Improvements

This document outlines immediate architectural improvements that can be made to enhance the NixOS configuration while maintaining all existing functionality.

## 1. Improve Module Organization

### Action:
Create subdirectories in the `modules/` directory for better categorization:

```
modules/
├── system/
│   ├── common-system.nix
│   ├── bootloader.nix
│   └── users.nix
├── desktop/
│   ├── wayland.nix
│   └── display-manager.nix
├── hardware/
│   └── graphics.nix
├── network/
│   ├── network-curfew.nix
│   └── network-manager.nix
└── home-manager-integration.nix
```

### Benefits:
- Easier to locate specific modules
- Better logical grouping of related configurations
- Scales better as more modules are added

## 2. Enhance Configuration Reusability

### Action:
Extract common configuration patterns into reusable functions:

```nix
# In modules/system/users.nix
{ lib, ... }:

{
  # Function to create a standard user with common settings
  mkStandardUser = name: {
    isNormalUser = true;
    shell = lib.mkDefault "/run/current-system/sw/bin/zsh";
    extraGroups = [ "wheel" "networkmanager" "input" "video" "bluetooth" ];
  };
}
```

### Benefits:
- Reduces duplication
- Ensures consistency across user configurations
- Makes it easier to modify common settings in one place

## 3. Improve Host Configuration Standardization

### Action:
Create a standardized template for host configurations:

```nix
# In hosts/common/host-template.nix
{ config, lib, pkgs, host, ... }:

{
  imports = [
    # Always include these
    ../../modules/system/common-system.nix
    ../../modules/home-manager-integration.nix
  ];

  # Standard networking
  networking.hostName = lib.mkDefault host.name;
  
  # Standard services that should be on all hosts
  services.openssh.enable = lib.mkDefault true;
  
  # Hardware configuration (to be overridden by specific hosts)
  # This file would be included in each host's host.nix
}
```

### Benefits:
- Ensures all hosts have consistent base configurations
- Makes it easier to add new hosts
- Reduces the chance of missing important configurations

## 4. Centralize Package Management

### Action:
Create a centralized package registry:

```nix
# In modules/system/packages.nix
{ pkgs }:

{
  # Categorized package sets
  development = with pkgs; [
    git vim bat gh
  ];
  
  productivity = with pkgs; [
    anki-bin rnote
  ];
  
  media = with pkgs; [
    mpv pavucontrol audacity obs-studio
  ];
  
  systemTools = with pkgs; [
    ripgrep fd jq thefuck
  ];
}
```

### Benefits:
- Easier to manage package dependencies
- Better understanding of what packages are for what purpose
- Simpler to add/remove packages in categories

## 5. Improve User Configuration Modularity

### Action:
Create role-based configuration modules:

```nix
# In home/navi/roles/
# developer.nix
{ pkgs, ... }:

{
  home.packages = with pkgs; [
    vscode sqlite graphviz
  ];
  
  programs.git = {
    enable = true;
    userName = "navi";
    userEmail = "ivan.nizamov@proton.com";
  };
}

# In home/navi/user-base.nix
{ ... }:

{
  imports = [
    ./roles/developer.nix
    ./wayland-desktop.nix
    ./programs/emacs.nix
    ./scripts.nix
  ];
}
```

### Benefits:
- Clear separation of concerns
- Easier to enable/disable roles
- Better organization of user-specific configurations

## 6. Enhance Documentation

### Action:
Add more detailed comments to configuration files and create a documentation index:

```nix
# Example of enhanced documentation in a config file
# ------------------------------------------------------------------------------
# Audio Configuration
# ------------------------------------------------------------------------------
# This section configures the PipeWire audio system, which replaces PulseAudio
# and provides better Wayland integration. It includes:
# - ALSA support for compatibility
# - 32-bit application support
# - PulseAudio compatibility layer
# - JACK compatibility for professional audio applications
# - Real-time audio support for low-latency applications
# ------------------------------------------------------------------------------
services.pipewire = {
  enable = true;
  alsa.enable = true;
  alsa.support32Bit = true;
  pulse.enable = true;
  wireplumber.enable = true;
  jack.enable = true;
  security.rtkit.enable = true;
};
```

### Benefits:
- Easier for others (or future you) to understand the configuration
- Better knowledge transfer
- Reduced time spent figuring out why certain configurations exist

## Implementation Priority

1. **Module Organization** - Low effort, high benefit for maintainability
2. **Documentation Enhancement** - Immediate benefit, helps with all other improvements
3. **Package Management** - Medium effort, improves package management clarity
4. **Configuration Reusability** - Medium effort, reduces duplication
5. **Host Standardization** - Medium effort, improves consistency
6. **User Configuration Modularity** - Higher effort, but provides the most flexibility

## Testing Strategy

For each improvement:
1. Make changes in a separate branch
2. Run `nixos-rebuild test --flake .#desktop` and `nixos-rebuild test --flake .#laptop`
3. Verify functionality hasn't been broken
4. Run `nixos-rebuild switch` to apply changes
5. Commit and push changes

This approach ensures that all existing functionality is preserved while making the architecture more maintainable and scalable.