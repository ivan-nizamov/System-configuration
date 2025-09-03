# Changes Made to Switch to nixpkgs-unstable

## Summary of Changes

1. **flake.nix**:
   - Changed `nixpkgs.url` from `nixos-25.05` to `nixos-unstable`
   - Added `nixpkgs-stable.url` pointing to `nixos-25.05`
   - Updated home-manager to use `master` branch instead of `release-25.05`
   - Updated description to reflect unstable

2. **modules/home-manager-integration.nix**:
   - Changed `pkgs-unstable` to import from `inputs.nixpkgs` (now unstable) instead of `inputs.nixpkgs-unstable`
   - Added `pkgs-stable` import from `inputs.nixpkgs-stable`
   - Updated `home-manager.extraSpecialArgs` to include `pkgs-stable`
   - Updated the user module definition to include `pkgs-stable`

3. **modules/common-system.nix**:
   - Added comment explaining how to use stable packages when needed
   - Added header comment indicating it's using unstable

4. **home/navi/user-base.nix**:
   - Added comment explaining how to use stable packages when needed
   - Added header comment indicating it's using unstable

5. **README.md**:
   - Updated title to reflect unstable
   - Updated home-manager references from `release-25.05` to `master`
   - Updated commit message example

6. **QWEN.md**:
   - Updated title to reflect unstable

## How to Use Stable Packages

To use a package from the stable branch when the unstable version is broken or unusable:

1. For system packages in `modules/common-system.nix`:
   ```nix
   environment.systemPackages = with pkgs; [
     # Use unstable package
     git
     # Use stable package when needed
     pkgs-stable.broken-package
   ];
   ```

2. For user packages in `home/navi/user-base.nix`:
   ```nix
   home.packages = with pkgs; [
     # Use unstable package
     firefox
     # Use stable package when needed
   ] ++ (with pkgs-stable; [
     broken-package
   ]);
   ```

## Verification

To verify the changes are working:
1. Run `nix flake update` to update the lock file
2. Run `sudo nixos-rebuild switch --flake .#<host>` to apply the changes
3. Check that packages are being pulled from the correct sources