{ pkgs, ... }:

{
  home.packages = with pkgs; [
    hunspell
    hunspellDicts.en-us
    hunspellDicts.ro-ro
    hunspellDicts.ru-ru
    hunspellDicts.es-es
    ripgrep                 # для consult-ripgrep
    curl                    # Для AnkiConnect тестирования (опционально, но рекомендуется)
  ];

  programs.emacs = {
    enable = true;
    package = pkgs.emacs30; # Emacs 30

    extraPackages = epkgs: with epkgs; [
      which-key
      (treesit-grammars.with-grammars (p: with p; [
        tree-sitter-bash
        tree-sitter-c
        tree-sitter-cpp
        tree-sitter-css
        tree-sitter-javascript
        tree-sitter-json
        tree-sitter-python
        tree-sitter-rust
        tree-sitter-yaml
      ]))
      consult
      dashboard
      eglot
      flyspell-correct
      gruvbox-theme
      ligature
      marginalia
      orderless
      org-modern
      org-roam
      org-roam-ui
      posframe
      vertico
      vertico-posframe
      all-the-icons
      doom-modeline
      anki-editor             # Для создания Anki-карточек из Org mode
    ];

    extraConfig = ''
;; -*- lexical-binding: t; -*-

;;;; Early UI
(setq inhibit-startup-screen t
      inhibit-startup-message t
      initial-buffer-choice nil)

;;;; Visual bell (no annoying sound)
(setq ring-bell-function 'flash-ding)
(setq visible-bell t)

;;;; Spelling
(setq ispell-program-name "hunspell"
      ispell-extra-args '("-d" "en_US,ro_RO,ru_RU,es_ES"))
(add-hook 'text-mode-hook #'flyspell-mode)
(add-hook 'org-mode-hook  #'flyspell-mode)
(use-package flyspell-correct
  :after flyspell
  :bind (("C-;" . flyspell-correct-wrapper)
         :map flyspell-mode-map
         ("C-;" . flyspell-correct-wrapper)))

;;;; Use-package & package mgmt (пусть Nix ставит пакеты)
(setq use-package-always-ensure nil)

;;;; Vertico stack
(use-package vertico
  :init (vertico-mode)
  :custom
  (vertico-scroll-margin 0)
  (vertico-count 20)
  (vertico-resize t)
  (vertico-cycle t))

(use-package vertico-posframe
  :after vertico
  :config (vertico-posframe-mode 1))

(use-package marginalia
  :after vertico
  :init (marginalia-mode))

(use-package orderless
  :custom
  (completion-styles '(orderless basic))
  (completion-category-defaults nil)
  (completion-category-overrides '((file (styles partial-completion)))))

(use-package consult
  :bind (("C-x b"   . consult-buffer)
         ("C-x p b" . consult-bookmark)
         ("C-x p f" . consult-recent-file)
         ("M-g g"   . consult-goto-line)
         ("M-g i"   . consult-imenu)
         ("M-s g"   . consult-grep)
         ("M-s L"   . consult-line)
         ("M-s m"   . consult-multi-occur)
         ("M-s r"   . consult-ripgrep)
         ("M-s s"   . consult-search)
         ("M-y"     . consult-yank-pop)
         :map minibuffer-local-map
         ("C-r"     . consult-history))
  :config
  (setq consult-project-root-function #'consult--default-project-root-function))

;;;; Eglot (без :ensure)
(use-package eglot
  :hook (prog-mode . eglot-ensure)
  :config
  (setq eglot-autoshutdown t
        eglot-confirm-server-initiated-restart nil)
  :bind (:map eglot-mode-map
              ("C-c a" . eglot-code-actions)
              ("C-c r" . eglot-rename)
              ("C-c f" . eglot-format)))

;;;; Font & ligatures
(set-face-attribute 'default nil
                    :family "Maple Mono NF CN"
                    :height 150
                    :weight 'regular)

(use-package ligature
  :config
  ;; Достаточный набор, включая "->" и "=>"
  (ligature-set-ligatures t
    '("->" "=>" "<-" "<->" "<=>" ">>" ">>>" "<<" "<<<" "::" ":::" "==" "===" "!="
      "<=" ">=" "||" "&&" "++" "--" "/*" "*/" "//" "///" "|>" "<|" "</" "/>" "+>" "<+"
      "~>" "<~" "<~>" "~~>" "<~~" ":>" ":<" "::=" "=:="))
  (global-ligature-mode t))

;;;; Stock UI off
(menu-bar-mode -1)
(tool-bar-mode -1)
(scroll-bar-mode -1)
(blink-cursor-mode 0)
(global-display-line-numbers-mode -1)
(column-number-mode 1)

;;;; Theme
(use-package gruvbox-theme
  :init
  (mapc #'disable-theme custom-enabled-themes)
  (load-theme 'gruvbox-dark-hard t))

;;;; Modeline
(use-package doom-modeline
  :init (doom-modeline-mode 1))

;;;; Dashboard (при старте без файлов)
(when (< (length command-line-args) 2)
  (use-package dashboard
    :config
    (setq dashboard-center-content t
          dashboard-vertically-center-content t
          dashboard-items '((recents . 8) (projects . 5) (bookmarks . 5) (agenda . 5))
          dashboard-startup-banner 'official)
    (dashboard-setup-startup-hook)))

;;;; History
(use-package savehist :init (savehist-mode))

;;;; Minibuffer QoL
(setq enable-recursive-minibuffers t
      minibuffer-prompt-properties '(read-only t cursor-intangible t face minibuffer-prompt))
(setq-default cursor-type 'bar)

;;;; Org
(setq org-directory "~/ORG/"
      org-agenda-files (directory-files-recursively "~/ORG/Roam/" "\\.org$")
      org-id-locations-file (expand-file-name ".org-id-locations" user-emacs-directory)
      org-startup-with-inline-images t
      org-use-fast-todo-selection t
      org-todo-keywords '((sequence "TODO(t)" "CALL(l)" "MEETING(m)" "TEST(e)" "HOMEWORK(h)" "PROJECT(p)" "|" "DONE(d)" "CANCELLED(c)"))
      org-todo-keyword-faces
      '(("TODO" . (:background "#458588" :foreground "#fbf1c7" :weight bold))
        ("CALL" . (:background "#689d6a" :foreground "#fbf1c7" :weight bold))
        ("MEETING" . (:background "#d65d0e" :foreground "#fbf1c7" :weight bold))
        ("TEST" . (:background "#cc241d" :foreground "#fbf1c7" :weight bold))
        ("HOMEWORK" . (:background "#b16286" :foreground "#fbf1c7" :weight bold))
        ("PROJECT" . (:background "#d79921" :foreground "#fbf1c7" :weight bold))
        ("DONE" . (:background "#98971a" :foreground "#282828" :weight bold))
        ("CANCELLED" . (:background "#3c3836" :foreground "#928374" :weight bold :strike-through t))))

(org-babel-do-load-languages
 'org-babel-load-languages
 '((emacs-lisp . t)
   (python . t)))

(global-set-key (kbd "C-c l") 'org-store-link)
(global-set-key (kbd "C-c a") 'org-agenda)

;; Org Modern
(use-package org-modern)
(with-eval-after-load 'org (global-org-modern-mode))
(add-hook 'org-mode-hook #'org-indent-mode)
;; Вынес визуальные тюнинги из setq-формы и включил перенос строк глобально:
(global-visual-line-mode t)
(setq
  org-auto-align-tags nil
  org-tags-column 0
  org-catch-invisible-edits 'show-and-error
  org-special-ctrl-a/e t
  org-insert-heading-respect-content t
  org-hide-emphasis-markers t
  org-startup-indented t
  org-indent-mode-turns-on-hiding-stars t
  org-modern-fold-stars '(("󰜵" . "󱥧"))
  org-modern-star 'fold
  org-ellipsis "…")

;; Размеры заголовков Org
(set-face-attribute 'org-level-1 nil :height 1.5)
(set-face-attribute 'org-level-2 nil :height 1.35)
(set-face-attribute 'org-level-3 nil :height 1.2)
(set-face-attribute 'org-level-4 nil :height 1.1)
(set-face-attribute 'org-level-5 nil :height 1.0)
(set-face-attribute 'org-level-6 nil :height 0.9)
(set-face-attribute 'org-level-7 nil :height 0.8)
(set-face-attribute 'org-level-8 nil :height 0.7)

;;;; Org-roam (+UI)
(use-package org-roam
  :init (setq org-roam-v2-ack t)
  :custom
  (org-roam-directory "~/ORG/Roam/")
  (org-roam-dailies-directory "~/ORG/Roam/journal/")
  (org-roam-completion-everywhere t)
  :bind (("C-c n l" . org-roam-buffer-toggle)
         ("C-c n f" . org-roam-node-find)
         ("C-c n i" . org-roam-node-insert)
         :map org-mode-map
         ("C-M-i" . completion-at-point)
         :map org-roam-dailies-map
         ("Y" . org-roam-dailies-capture-yesterday)
         ("T" . org-roam-dailies-capture-tomorrow))
  :bind-keymap ("C-c n d" . org-roam-dailies-map)
  :config
  (setq org-roam-capture-templates
      '(("d" "default" plain "%?"
         :if-new (file+head "''${slug}.org" "#+title: ''${title}\n")
         :unnarrowed t)))
  (require 'org-roam-dailies)
  (setq org-roam-dailies-capture-templates
        '(("d" "default" entry
           "* %?\nTaken: %(format-time-string \"<%Y-%m-%d %H:%M>\")"
           :if-new (file+head "%<%Y-%m-%d>.org" "#+title: %<%Y-%m-%d>\n"))))
  (org-roam-db-autosync-mode))

(use-package org-roam-ui
  :after org-roam
  :config
  (setq org-roam-ui-sync-theme t
        org-roam-ui-follow t
        org-roam-ui-update-on-save t
        org-roam-ui-open-on-start t))

;;;; Anki integration (anki-editor)
(use-package anki-editor
  :after org
  :config
  (setq anki-editor-create-decks t          ;; Auto-create decks if they don't exist
        anki-editor-org-tags-as-anki-tags t ;; Sync Org tags to Anki tags
        anki-editor-anki-connect-listening-address "127.0.0.1"  ;; Default AnkiConnect address
        anki-editor-anki-connect-listening-port 8765)           ;; Default port
  ;; Optional: Keybindings for common actions
  :bind (:map org-mode-map
              ("C-c a p" . anki-editor-push-notes)     ;; Push notes to Anki
              ("C-c a i" . anki-editor-insert-note)    ;; Insert new note template
              ("C-c a c" . anki-editor-cloze-region))) ;; Cloze a selected region

;; Optional: Org Capture templates for Anki cards
(setq org-capture-templates
      '(("a" "Anki Basic" entry
         (file+headline "~/ORG/Roam/Inbox.org" "Anki Inbox")  ;; Adjust path as needed
         "* %t %^g\n:PROPERTIES:\n:ANKI_NOTE_TYPE: Basic\n:ANKI_DECK: Default\n:END:\n** Front\n%?\n** Back\n")
        ("A" "Anki Cloze" entry
         (file+headline "~/ORG/Roam/Inbox.org" "Anki Inbox")  ;; Adjust path as needed
         "* %t %^g\n:PROPERTIES:\n:ANKI_NOTE_TYPE: Cloze\n:ANKI_DECK: Default\n:END:\n** Text\n%x\n")))

;;;; LaTeX export (опционально, нужен xelatex в системе)
(setq org-latex-pdf-process '("xelatex -interaction nonstopmode -output-directory %o %f"))

;;;; Treesit
(when (and (fboundp 'treesit-available-p) (treesit-available-p))
  (setq major-mode-remap-alist
        '((bash-mode   . bash-ts-mode)
          (c-mode      . c-ts-mode)
          (c++-mode    . c++-ts-mode)
          (css-mode    . css-ts-mode)
          (js-mode     . js-ts-mode)
          (json-mode   . json-ts-mode)
          (rust-mode   . rust-ts-mode)
          (python-mode . python-ts-mode)
          (yaml-mode   . yaml-ts-mode)))
  (setq treesit-font-lock-level 4))

;;;; Helpers
(defun my/org-list-checkboxes-region (beg end &optional remove)
  "В регионе BEG..END добавить [ ] к пунктам списка без чекбоксов.
С префиксом REMOVE (C-u) — удалить чекбоксы."
  (interactive
   (list (if (use-region-p) (region-beginning) (point-min))
         (if (use-region-p) (region-end) (point-max))
         current-prefix-arg))
  (unless (use-region-p)
    (user-error "Нет активного региона — выделите нужный список"))
  (save-excursion
    (save-restriction
      (narrow-to-region beg end)
      (goto-char (point-min))
      (if remove
          (while (re-search-forward
                  "^\\(\\s-*\\(?:[-+*]\\|[0-9]+[.)]\\) \\)\\(\\[[ xX-]\\]\\s-*\\)"
                  nil t)
            (replace-match "\\1" nil nil))
        (while (re-search-forward
                "^\\(\\s-*\\(?:[-+*]\\|[0-9]+[.)]\\) \\)\\([^[]\\)"
                nil t)
          (replace-match "\\1[ ] \\2" nil nil))))))

(with-eval-after-load 'org
  (define-key org-mode-map (kbd "C-c x r") #'my/org-list-checkboxes-region))

;; === Leader на ОБРАТНУЮ КОСУЮ ЧЕРТУ (без Evil) ===
;; --- Leader on BACKSLASH (no Evil) ---
;; Define a real keymap var (clean, no quirks)
(defvar my/leader-map (make-sparse-keymap)
  "Leader keymap under BACKSLASH.")

;; Global leader: pass the *keymap value*, not a quoted symbol
(keymap-global-set "\\" my/leader-map)

;; "\\ \\" inserts a literal backslash in normal buffers
(keymap-set my/leader-map "\\" #'self-insert-command)

;; Keep backslash as backslash in minibuffer maps (don't wrap in with-eval-after-load)
(dolist (map (list minibuffer-local-map
                   minibuffer-local-ns-map
                   minibuffer-local-completion-map
                   minibuffer-local-must-match-map))
  (define-key map (kbd "\\") #'self-insert-command))

;; (optional) make \ work on the dashboard
(with-eval-after-load 'dashboard
  (keymap-set dashboard-mode-map "\\" my/leader-map))

;; 5) which-key, чтобы видеть меню после "\"
(use-package which-key
  :init (which-key-mode)
  :config
  (setq which-key-idle-delay 0.2
        which-key-separator " → "
        which-key-prefix-prefix "◂ "))

(with-eval-after-load 'which-key
  ;; Top-level prefix labels under \
  (which-key-add-keymap-based-replacements my/leader-map
    "f" "files"
    "b" "buffers"
    "w" "windows"
    "s" "search"
    "g" "goto"
    "o" "org"
    "n" "org-roam"
    "h" "help"
    "<escape>" "cancel")

  ;; Optional: label common sub-prefixes/leafs too
  (which-key-add-keymap-based-replacements my/leader-map
    "f f" "find file"
    "f r" "recent files"
    "f s" "save file"
    "b b" "switch buffer"
    "b k" "kill buffer"
    "w /" "split right"
    "w -" "split below"
    "w o" "maximize"
    "s s" "search line"
    "s r" "ripgrep"
    "g g" "goto line"
    "g i" "imenu"
    "o a" "agenda"
    "o l" "store link"
    "n f" "find node"
    "n i" "insert node"
    "n l" "toggle roam buffer"))

;; === Примеры биндов «как в Doom» ===
;; Files
(define-key my/leader-map (kbd "f f") #'find-file)
(define-key my/leader-map (kbd "f r") #'consult-recent-file)
(define-key my/leader-map (kbd "f s") #'save-buffer)

;; Buffers
(define-key my/leader-map (kbd "b b") #'consult-buffer)
(define-key my/leader-map (kbd "b k") #'kill-current-buffer)
(define-key my/leader-map (kbd "b n") #'next-buffer)
(define-key my/leader-map (kbd "b p") #'previous-buffer)

;; Windows
(define-key my/leader-map (kbd "w /") #'split-window-right)
(define-key my/leader-map (kbd "w -") #'split-window-below)
(define-key my/leader-map (kbd "w d") #'delete-window)
(define-key my/leader-map (kbd "w o") #'delete-other-windows)
(define-key my/leader-map (kbd "w h") #'windmove-left)
(define-key my/leader-map (kbd "w j") #'windmove-down)
(define-key my/leader-map (kbd "w k") #'windmove-up)
(define-key my/leader-map (kbd "w l") #'windmove-right)

;; Search
(define-key my/leader-map (kbd "s s") #'consult-line)
(define-key my/leader-map (kbd "s r") #'consult-ripgrep)

;; GoTo
(define-key my/leader-map (kbd "g g") #'goto-line)
(define-key my/leader-map (kbd "g i") #'consult-imenu)

;; Org / Org-roam (под твои пакеты)
(define-key my/leader-map (kbd "o a") #'org-agenda)
(define-key my/leader-map (kbd "o l") #'org-store-link)
(define-key my/leader-map (kbd "n f") #'org-roam-node-find)
(define-key my/leader-map (kbd "n i") #'org-roam-node-insert)
(define-key my/leader-map (kbd "n l") #'org-roam-buffer-toggle)

;; Help (как Doom: \ h …)
(define-key my/leader-map (kbd "h k") #'describe-key)
(define-key my/leader-map (kbd "h f") #'describe-function)
(define-key my/leader-map (kbd "h v") #'describe-variable)

;; Удобно уметь отменять лидер — ESC
(define-key my/leader-map (kbd "<escape>") #'keyboard-quit)

'';
  };
}