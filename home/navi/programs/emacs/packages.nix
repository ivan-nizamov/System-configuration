# packages.nix — Slim, modern, non-redundant set
{ pkgs }:
epkgs: with epkgs; [
  # --- Core / quality ---
  use-package
  diminish
  bind-key
  cl-lib
  gcmh                      # GC Manager for better interactivity

  # --- Themes ---
  gruvbox-theme
  catppuccin-theme
  kanagawa-themes

  # --- Treesitter grammars (all, including Typst) ---
  (treesit-grammars.with-all-grammars)

  # --- Completion stack (minibuffer + in-buffer) ---
  vertico
  vertico-posframe
  orderless
  marginalia
  consult
  embark
  embark-consult
  corfu
  cape

  # --- UI/QoL ---
  doom-modeline
  which-key
  page-break-lines
  helpful
  all-the-icons
  all-the-icons-dired
  rainbow-delimiters
  dashboard
  avy
  expand-region
  multiple-cursors
  smartparens

  # --- VCS ---
  magit
  nix-mode
  ligature
  move-text

  # --- Search helpers ---
  dired-hide-dotfiles

  # --- Org & documents ---
  org-modern
  org-appear
  org-superstar
  org-download
  org-roam
  org-roam-ui
  emacsql
  anki-editor
  latex-preview-pane
  org-pdftools
  org-cliplink
  org-fragtog   # ← add this
  nov
  yasnippet

  # --- Markdown ---
  markdown-mode
  markdown-toc
  grip-mode

  # --- Typst ---
  typst-ts-mode

  # --- Spell / language ---
  jinx

  # --- PDF ---
  pdf-tools

  # --- (Optional AI clients were removed to keep core clean) ---
]