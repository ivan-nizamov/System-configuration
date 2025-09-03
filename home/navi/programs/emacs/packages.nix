# packages.nix - Emacs packages organized by category
{ pkgs }:

# This file contains all the Emacs packages to be installed,
# organized by functional categories.
epkgs: with epkgs; [
  # ---- Core & Theme Packages ----
  nix-mode
  magit
  ligature
  move-text
  
  # UI Themes
  kanagawa-themes
  catppuccin-theme
  gruvbox-theme
  
  # Core packages
  use-package
  diminish
  bind-key
  cl-lib             # Required for cl-pushnew function

  # ---- Completion System ----
  company
  
  # Modern completion framework
  vertico
  vertico-posframe    # Display vertico in a posframe
  orderless          # Flexible matching
  marginalia         # Rich annotations
  consult            # Search and navigation commands
  embark             # Context-aware actions
  embark-consult     # Integration between embark and consult
  corfu              # In-buffer completion

  # ---- File and Project Management ----
  projectile
  counsel-projectile
  dired-hide-dotfiles # Hide dotfiles in dired

  # ---- UI Enhancements ----
  doom-modeline
  which-key
  page-break-lines
  helpful
  all-the-icons
  all-the-icons-dired
  rainbow-delimiters
  dashboard
  centaur-tabs

  # ---- Org Mode and Document Editing ----
  org-superstar
  org-download
  org-roam
  org-roam-ui
  emacsql
  anki-editor
  latex-preview-pane
  org-modern
  org-appear          # Show markers when editing
  org-fragtog         # Auto-toggle LaTeX fragments
  org-noter           # Note-taking while reading PDFs
  org-pdftools        # PDF integration with org-mode
  org-cliplink        # Insert links from clipboard

  # ---- Markdown Support ----
  markdown-mode
  markdown-toc
  grip-mode

  # ---- Spellchecking and Language Tools ----
  flycheck
  flyspell-correct
  jinx                 # Alternative spellchecker

  # ---- PDF and Media Support ----
  pdf-tools
  yasnippet
  nov                  # EPUB reader

  # ---- AI and Advanced Tools ----
  ellama
  gptel                # ChatGPT interface

  # ---- Navigation and Editing ----
  avy                  # Quick navigation
  ace-window           # Window management
  smartparens          # Parentheses handling
  expand-region        # Expand selection
  multiple-cursors     # Edit multiple places at once
  undo-tree            # Better undo visualization
]

