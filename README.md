# NixOS Config Design & Operating Guide (Unstable)

This doc is your **map**. It tells you **where things live**, **who owns what**, and **how to change stuff safely**—even if you (or an LLM agent) aren't fully acquainted with the codebase yet. It stays useful even after files evolve because it documents **responsibilities and patterns**, not just today's content.

---

## Goals & Scope (MVP)

- **One repo / one lockfile** for everything.
- **Two modes**:
  - **Integrated**: NixOS + Home-Manager switch atomically on your machines.
  - **Standalone HM**: user-only profile for servers (no sudo).
- **Two hosts** today: `desktop` (GPU) and `laptop` (CPU). Pattern supports more.
- **Host differences** are isolated (GPU drivers, disks, login, display configuration).
- **Secrets** optional via `sops-nix`, per-host keys.
- **Clear naming**: File names clearly indicate their purpose and scope.

---

## Documentation

Additional documentation files can be found in the `docs/` directory:

- `docs/hardware-configuration.md` - Hardware configuration policy: hosts import only from `/etc/nixos/hardware-configuration.nix` and require `--impure` rebuilds


## Directory Structure (what each path is for)

```
.
├─ flake.nix                          # Orchestrator & pinning (versions, outputs, host facts)
├─ flake.lock                         # Locked dependency versions
├─ modules/
│  ├─ common-system.nix               # Shared OS config (users, boot, SSH, nix settings)
│  └─ home-manager-integration.nix    # Home-Manager ↔ NixOS integration layer
├─ hosts/
│  ├─ desktop/
│  │  ├─ host.nix                     # Desktop-only OS settings (GPU, disks). Imports HW from /etc only
│  │  └─ user-config.nix              # Desktop-specific user settings & overrides
│  └─ laptop/
│     ├─ host.nix                     # Laptop-only OS settings (power/touchpad, display). Imports HW from /etc only
│     └─ user-config.nix              # Laptop-specific user settings & overrides
├─ home/
│  └─ navi/
│     ├─ user-base.nix                # Base user config (shell, git, CLI apps)
│     ├─ wayland-desktop.nix          # Wayland/Hyprland desktop environment
│     ├─ scripts.nix                  # User scripts and utilities
│     └─ desktop/                     # Desktop environment components
│         ├─ waybar.nix               # Status bar configuration
│         ├─ rofi.nix                 # Application launcher
│         ├─ wallpaper.nix            # Wallpaper management
│         └─ gammastep.nix            # Blue light filter
└─ secrets/
   └─ server.yaml                     # (optional) Encrypted with sops; per-host keys

````

**Mental model**
- `modules/` = reusable building blocks.
- `hosts/<name>/` = **only** what’s truly per-host.
- `home/navi/` = user-space profiles; **never** system-wide settings here.
- `flake.nix` = **orchestrator & pinning** (versions, outputs, host facts).

---

## Ownership Map (who “owns” what)

- **System (NixOS)** → kernel, bootloader, services, filesystems, networking, system users, global packages.  
  Files: `modules/common-system.nix` + `hosts/<name>/host.nix` (+ hardware file)
- **User (Home-Manager)** → dotfiles, shells, editors, CLI apps, per-user env vars.  
  Files: `home/navi/*.nix` (+ `hosts/<name>/user-config.nix` if needed)
- **Flakes** → pins & exposes build targets.  
  File: `flake.nix`
- **Secrets** → encrypted in `secrets/`, decrypted by HM on authorized host(s).

---

## Fundamental Rules: DO NOT 🚫

- **DO NOT** commit host hardware files. Hosts read hardware exclusively from `/etc/nixos/hardware-configuration.nix` and require `--impure` during rebuilds.
- **DO NOT** edit `hosts/<name>/hardware-configuration.nix` by hand unless you know why. Regenerate with `nixos-generate-config --show-hardware-config`.
- **DO NOT** bump `system.stateVersion` after first install. Keep it fixed (e.g., "25.05").
- **DO NOT** mix **integrated HM** and **standalone HM** for the **same user on the same host**.
- **DO NOT** keep plaintext secrets in Git. Use `sops` + Age recipients.
- **DO NOT** hardcode device names (e.g., `enp3s0`) in shared modules. Use by-uuid/by-label, or gate in per-host files.
- **DO NOT** use `nix-env -i`. Keep installs declarative.
- **DO NOT** remove boot labeling or loader limits blindly (`system.nixos.label`, `configurationLimit`).

---

## Good Practices ✅

- **Host facts** via flake `specialArgs`: `host = { name; accel = "cpu"|"cuda"|"rocm"; }`.  
  Gate features with `lib.mkIf`.
- **Separation of concerns**: system toggles → `hosts/<name>/host.nix`; user tweaks → `hosts/<name>/user-config.nix`.
- **Small commits**: one logical change per commit; include command used (e.g., `nixos-rebuild switch --flake .#desktop`).
- **Test first**: `nixos-rebuild test` before `switch`.
- **Rollbacks**: keep boot generations limited; label with host.
- **Pin HM to nixpkgs**: `home-manager.inputs.nixpkgs.follows = "nixpkgs"` to avoid drift.

---

## How to Change Things (safest recipes)

**System-wide package (global)**
- Edit: `modules/common-system.nix` → `environment.systemPackages`
- Test: `sudo nixos-rebuild test --flake .#<host> --impure`
- Apply: `sudo nixos-rebuild switch --flake .#<host> --impure`

**User package / alias (only `navi`)**
- Edit: `home/navi/user-base.nix` → `home.packages` or `programs.zsh.initExtra`
- Integrated host: `sudo nixos-rebuild switch --flake .#<host>`
- Standalone server:  
  `nix run github:nix-community/home-manager/master -- switch --flake .#"navi@server"`

**Enable a service**
- All hosts → `modules/common-system.nix`
- One host → `hosts/<name>/host.nix`

**GPU-only feature**
- Put logic in a dedicated module under `modules/`
- Enable via `lib.mkIf (host.accel != "cpu")`

**Host-specific filesystem**
- Edit `hosts/<name>/host.nix` (or use generated hardware file)
- Prefer by-uuid/by-label mounts

**Per-user env vars**
- Edit `home/navi/user-base.nix` → `home.sessionVariables`
- For host-specific vars: `lib.mkIf (host.name == "desktop")`

**System-wide env vars**
- Edit `hosts/<name>/host.nix` → `environment.sessionVariables`

**Secrets (tokens, API keys)**
- Create/modify `secrets/server.yaml` (template)
- Encrypt with `sops` to allowed hosts’ Age public keys
- Reference in HM under `sops.secrets.<name>`
- Never put secrets in system modules

**Boot menu clarity**
- Edit `modules/common-system.nix` → `system.nixos.label = "nixos-${host.name}"`

---

## Pattern Library

**Conditional features with `mkIf`**
```nix
{ lib, host, ... }: {
  config = lib.mkIf (host.accel != "cpu") {
    services.ollama.enable = true;
  };
}
````

**Per-host toggles**

* Prefer `host.accel`, `host.name` via `specialArgs`
* Avoid repeating hostname string checks around the tree

**New host onboarding**

1. Create `hosts/<new>/`:

   * `host.nix` (start small)
   * `user-config.nix` (optional)
2. On the target machine, generate `/etc/nixos/hardware-configuration.nix`:

   ```bash
   sudo nixos-generate-config --show-hardware-config > /etc/nixos/hardware-configuration.nix
   ```
3. Add to `flake.nix`:

   ```nix
   nixosConfigurations.<new> = mkHost { name = "<new>"; accel = "cpu"; };
   ```
4. Switch (impure, reads `/etc`): `sudo nixos-rebuild switch --flake .#<new> --impure`

**Standalone HM for any box**

* Add target in `flake.nix`: `homeConfigurations."user@tag"`
* Activate:

  ```bash
  mkdir -p ~/.config/nix
  printf 'experimental-features = nix-command flakes\n' > ~/.config/nix/nix.conf
  nix run github:nix-community/home-manager/release-25.05 -- \
    switch --flake .#"user@tag"
  ```

---

## Guardrails for LLM Agents (edit policy)

**Allowed (safe)**

* `home/navi/*.nix`
* `modules/*.nix` (new module or adjust existing)
* `hosts/<name>/host.nix`
* `flake.nix` (add host/HM target, or update inputs)

**Restricted (human review)**

* `hosts/<name>/hardware-configuration.nix`
* `secrets/*.yaml`
* `system.stateVersion`

**Process**

* Always run `nixos-rebuild test` before `switch`
* Editing `flake.nix`: append new outputs; don’t delete existing ones casually
* Updating inputs: `nix flake update`; commit `flake.lock` + `flake.nix`
* Prefer `lib.mkIf` & `specialArgs` to hostname string checks

---

## Workflows

**Daily (laptop → desktop)**

1. Laptop: edit → `sudo nixos-rebuild test --flake .#$(hostname)` → `sudo nixos-rebuild switch --flake .#$(hostname)`
2. `git add -A && git commit -m "Describe change" && git push`
3. Desktop: `git pull && sudo nixos-rebuild switch --flake .#$(hostname)`

**Server without sudo (standalone HM)**

```bash
git clone <repo> ~/dotfiles && cd ~/dotfiles
mkdir -p ~/.config/nix
printf 'experimental-features = nix-command flakes\n' > ~/.config/nix/nix.conf
nix run github:nix-community/home-manager/master -- \
  switch --flake .#"navi@server"
```

**Pin updates**

```bash
nix flake update
git add flake.lock flake.nix
git commit -m "flake: update nixpkgs + HM (unstable)"
git push
```

---

## Recovery & Safety

* Try without setting default: `sudo nixos-rebuild test --flake .#<host>`
* Switch (sets default): `sudo nixos-rebuild switch --flake .#<host>`
* Roll back:

  * At boot: pick previous generation (label shows host + date)
  * From userspace: `sudo nixos-rebuild --rollback`
* If boot entries vanish: ensure `/boot` mounted and loader configured
* Network curfew: Centralized module system with configurable times - see Network Curfew section below

---

## When Adding New Features

* Put reusable logic under `modules/<feature>.nix`
* Gate with `lib.mkIf` using `host.*` facts (or a `features` attr)
* Keep machine-specific bits in `hosts/<name>/host.nix`
* Keep user preferences in HM files

---

## Quick Index (where to edit)

| Task                | Edit here                                                            |
| ------------------- | -------------------------------------------------------------------- |
| System-wide pkg     | `modules/common-system.nix` → `environment.systemPackages`           |
| User pkg / alias    | `home/navi/user-base.nix` (or `hosts/<name>/user-config.nix`)       |
| Service (all hosts) | `modules/common-system.nix`                                          |
| Service (one host)  | `hosts/<name>/host.nix`                                              |
| GPU-only feature    | Create module in `modules/` with `lib.mkIf (host.accel != "cpu")`   |
| Add new host        | `hosts/<name>/` + `flake.nix` (`nixosConfigurations.<name>`)         |
| Server HM target    | `flake.nix` → `homeConfigurations."user@tag"`                        |
| Boot label          | `modules/common-system.nix` → `system.nixos.label`                   |
| Secrets             | `home/navi/*.nix` + `secrets/*.yaml` (encrypted)                     |
| Wayland/Hypr env    | `hosts/<name>/host.nix` → `environment.sessionVariables`             |
| Hyprland auto-login | `hosts/<name>/host.nix` → `services.greetd.settings.default_session` |
| Global theming      | `home/navi/user-base.nix` → `gtk`, `qt`, `dconf.settings`            |
| Terminal config     | `home/navi/user-base.nix` → `programs.kitty`                         |
| File manager        | Nautilus bound to `Super+F`, configured in `wayland-desktop.nix`     |
| Network curfew      | `hosts/<name>/host.nix` → `services.networkCurfew`                   |

---

## Network Curfew System

The network curfew system automatically terminates user network applications at configurable times using systemd timers. This provides a user-based approach to restrict network app usage during specified hours (e.g., nighttime) without affecting system-wide networking.

### Configuration

**Enable in host configuration:**
```nix
# In hosts/laptop/host.nix
services.networkCurfew = {
  enable = true;
  startTime = "20:30:00";  # Terminate network apps at 8:30 PM
  endTime = "06:00:00";    # Allow network apps at 6:00 AM
  persistent = true;       # Catches up missed executions after reboots
  user = "navi";          # Username to apply curfew to (default: "navi")
};
```

### Features

- **📅 Configurable times**: Set custom start/end times per host
- **👤 User-based**: Only affects specific user's applications, not system networking
- **🛠️ App termination**: Terminates browsers, torrent clients, chat apps, etc.
- **⏰ Systemd timers**: Daily scheduling with `OnCalendar=*-*-* HH:MM:SS`
- **📝 Logging**: All actions logged to `/var/log/net-curfew.log`
- **🔄 Persistent**: Catches up missed executions after reboots
- **🔔 Notifications**: Desktop notifications when curfew starts/ends

### Manual Control

```bash
# Check timer status
sudo systemctl list-timers | grep net-curfew

# Manual toggle (immediate)
sudo systemctl start net-curfew-off.service  # Disable now
sudo systemctl start net-curfew-on.service   # Enable now

# Check logs
sudo tail -f /var/log/net-curfew.log

# Emergency re-enable
sudo nmcli networking on
```

---

## About this README

This file serves as the operating guide for your NixOS configuration. It's located at the repo root and documents the design patterns, ownership model, and safe practices for maintaining your system configuration.
