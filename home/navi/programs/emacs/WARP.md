# WARP.md

This file provides guidance to WARP (warp.dev) when working with code in this repository.

## Architecture Overview

This is a **Nix-first Emacs configuration** that integrates with Home Manager. The configuration is designed to be declarative, reproducible, and fast. Unlike traditional Emacs configurations that use package.el or straight.el, this setup manages all packages through Nix.

### Core Design Principles

- **Nix-first**: All packages are declared in `packages.nix` and managed by Nix, not by Emacs package managers
- **Modular configuration**: Configuration is split into focused modules in the `config/` directory
- **Performance-optimized**: Uses garbage collection management and lazy loading patterns
- **Modern completion stack**: Vertico + Orderless + Corfu + Embark for completion

### Directory Structure

```
.
├── default.nix           # Home Manager Emacs configuration entry point
├── packages.nix          # Nix package declarations for Emacs packages
├── instructions.txt      # Important reminder about Nix-first approach
└── config/              # Emacs Lisp configuration modules
    ├── init-core.el      # Basic Emacs settings, GC, backups
    ├── init-theme.el     # Theme loading with fallbacks
    ├── init-completion.el # Modern completion stack (Vertico/Corfu)
    ├── init-search.el    # Project search with Consult
    ├── init-ui.el        # UI enhancements and visual elements
    ├── init-org.el       # Comprehensive Org-mode setup with Roam
    ├── init-dired.el     # Enhanced file management
    ├── init-typst.el     # Typst language support with LSP
    ├── init-spell.el     # Spell checking with Jinx
    ├── init-markdown.el  # Markdown mode configuration
    └── init-misc.el      # Editing utilities (Avy, Multiple Cursors, etc.)
```

## Package Management

**CRITICAL**: This configuration uses Nix for package management, NOT Emacs package managers.

- **Adding packages**: Edit `packages.nix`, then run `home-manager switch`
- **Never use**: `M-x package-install` or any package.el commands
- **System packages**: Add to `home.packages` in `default.nix` for system tools (LSP servers, formatters)

## Development Commands

### Managing the Configuration

```bash
# Apply configuration changes
home-manager switch

# Test configuration without switching
home-manager build

# Check what would change
home-manager news
```

### Working with Emacs

```bash
# Start Emacs
emacs

# Start in daemon mode (faster subsequent launches)
emacs --daemon
emacsclient -c

# Debug init issues
emacs --debug-init
```

### Nix Development

```bash
# Check flake inputs (from parent directory)
nix flake show

# Update dependencies
nix flake update

# Build specific output
nix build .#homeConfigurations.navi.activationPackage
```

## Configuration Loading Order

The configuration follows a specific loading sequence defined in `default.nix`:

1. `init-theme.el` - Load theme first to prevent visual flashing
2. `init-core.el` - Basic Emacs settings and performance optimizations
3. `init-completion.el` - Completion framework setup
4. `init-search.el` - Project and search configuration
5. `init-ui.el` - UI enhancements and visual elements
6. `init-spell.el` - Spell checking setup
7. `init-org.el` - Org-mode and note-taking features
8. `init-markdown.el` - Markdown support
9. `init-dired.el` - File management enhancements
10. `init-typst.el` - Typst language support
11. `init-misc.el` - Editing utilities and conveniences

## Key Features and Bindings

### Completion System
- **Vertico**: Modern minibuffer completion with `C-j/C-k` for navigation
- **Corfu**: In-buffer completion with automatic popup
- **Consult**: Enhanced search and navigation commands
- **Embark**: Context-sensitive actions with `C-.`

### Project Management
- Uses built-in `project.el` for project detection
- `C-c s s` - Search project with ripgrep
- `C-c s l` - Search current buffer
- `C-x p` - Project dispatch commands

### Org-mode Features
- Org-roam for networked note-taking in `~/ORG/Roam/`
- Custom TODO keywords with color coding
- LaTeX preview scaling optimized for readability
- Comprehensive capture templates for tasks, notes, ideas
- Custom agenda views for dashboard and weekly review

### Development Tools
- **Typst**: Full language support with Tinymist LSP server
- **Magit**: Git interface with `C-x g`
- **Multiple Cursors**: `C-S-c C-S-c` for line editing, `C->` for next occurrence
- **Avy**: Jump to characters with `C-:` and `C-'`

## External Dependencies

The following system packages are automatically installed:
- `emacs-all-the-icons-fonts` - Icon fonts for UI
- `pandoc` - Document conversion
- `languagetool` - Grammar checking
- `typst` - Typst compiler
- `tinymist` - Typst LSP server
- `typstfmt` - Typst formatter

## Important File Locations

- **Org directory**: `~/ORG/` (main organization folder)
- **Org Roam**: `~/ORG/Roam/` (networked notes)
- **Snippets**: `~/ORG/snippets/` (YASnippet templates)
- **Emacs data**: `~/.emacs.d/var/` (custom.el, backups, auto-saves)

## Troubleshooting

### Configuration Issues
1. Check Home Manager build: `home-manager build`
2. Verify package availability in Nix: `nix-env -qaP | grep package-name`
3. Debug Emacs init: `emacs --debug-init`

### Performance Issues
- The configuration includes GC optimization and should start quickly
- If slow, check for missing fonts or external dependencies
- Use `M-x profiler-start` for detailed performance analysis

### Package Management
- Remember: NO `package-install` commands - edit `packages.nix` instead
- All packages must be available in nixpkgs or added as overlays
- Check `instructions.txt` for package management reminders

## LSP and Language Support

Currently configured for:
- **Typst**: Tinymist server with formatting and completion
- **Nix**: Basic syntax highlighting and editing support

To add new language support:
1. Add LSP server to `home.packages` in `default.nix`
2. Add Emacs language mode to `packages.nix`
3. Configure in appropriate `init-*.el` file with `eglot-server-programs`
