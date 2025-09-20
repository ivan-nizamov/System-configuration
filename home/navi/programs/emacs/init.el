;; Local Variables:
;; byte-compile-warnings: (not undefined-functions)
;; End:

(require 'package)
(setq package-archives '(("gnu" . "https://elpa.gnu.org/packages/")
                         ("melpa" . "https://melpa.org/packages/")
                         ("nongnu" . "https://elpa.nongnu.org/nongnu/")))
(package-initialize)

;; Bootstrap use-package
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))
(require 'use-package)
(setq use-package-always-ensure t)

(setq custom-file (expand-file-name "custom.el" user-emacs-directory))
(load custom-file 'noerror)

;; Set keybinding for reloading config
 (defun my/reload-config ()
   "Tangle the literate config to a temp file and load it for testing.
 This does NOT replace the Nix-managed init.el."
   (interactive)
   (let* ((org-config-file (expand-file-name "~/System-configuration/home/navi/programs/emacs/config.org"))
          (temp-load-file (expand-file-name  "~/System-configuration/home/navi/programs/emacs/init.el")))
     ;; 1. Tangle the org file to our temporary elisp file
     (message "Tangling %s..." org-config-file)
     (require 'org)
     (org-babel-tangle-file org-config-file temp-load-file)

     ;; 2. Load the newly created temporary file
     (message "Loading %s..." temp-load-file)
     (load-file temp-load-file)
     (message "Configuration reloaded from %s. (This is a temporary load)" temp-load-file)))

(global-set-key (kbd "C-c r") #'my/reload-config)

;; Set default font
(set-face-attribute 'default nil
  :family "Maple Mono NF CN"
  :height 180
  :weight 'regular)

(use-package ligature
:config
;; Enable ligatures in all modes
(ligature-set-ligatures t '(">>" ">>>" "<<" "<<" "{{" "}}" "{{--" "}}" "/*" "*/" "||" "|||" "&&" "&&&" "::" ":::" "??" "???" "++" "+++" "##" "###" "!!" "!!!" "//" "///" "==" "===" "!=" "!==" "<=" ">=" "=<<" "=>>" "->" "<-" "<->" "=>" "<=>" "<!--" "-->" "<#--" "<!---->" "~~" "~>" "<~" "<~>" "~~>" "<~~" "::=" "=:=" ":>" ":<" "<:" ">:" "<*" "<*>" "*>" "<|" "<|>" "|>" "<+" "<+>" "+>" "</" "</>" "/>" "###" "####" "...." "::" ":::" "++" "+++" "??" "???" "!!" "!!!" "||" "|||" "&&" "&&&" "--" "---" "==" "===" "!=" "!==" "<=" ">=" "=<<" "=>>" "->" "<-" "<->" "=>" "<=>" "<!--" "-->" "<#--" "<!---->" "~~" "~>" "<~" "<~>" "~~>" "<~~" "::=" "=:=" ":>" ":<" "<:" ">:" "<*" "<*>" "*>" "<|" "<|>" "|>" "<+" "<+>" "+>" "</" "</>" "/>"))
(global-ligature-mode t))

;; Disable startup message
(setq inhibit-startup-message t)

;; Enable line numbers
(global-display-line-numbers-mode -1)

;; Highlight current line
(global-hl-line-mode -1)

;; Show column number
(setq column-number-mode t)

;; Disable toolbar and scrollbar
(tool-bar-mode -1)
(scroll-bar-mode -1)
(menu-bar-mode -1)

;; Disable cursor blinlink
(setq blink-cursor-mode nil)

(use-package gruvbox-theme
:init
;; avoid mixed faces if another theme was active
(mapc #'disable-theme custom-enabled-themes)
;; load without confirmation
(load-theme 'gruvbox-dark-hard t))

(use-package dashboard
  :config
  (setq dashboard-center-content t
        dashboard-vertically-center-content t)
  (dashboard-setup-startup-hook))

;; Enable Vertico.
(use-package vertico
  ;; :custom
  ;; (vertico-scroll-margin 0) ;; Different scroll margin
  ;; (vertico-count 20) ;; Show more candidates
  ;; (vertico-resize t) ;; Grow and shrink the Vertico minibuffer
  ;; (vertico-cycle t) ;; Enable cycling for `vertico-next/previous'
  :init
  (vertico-mode))

;; Persist history over Emacs restarts. Vertico sorts by history position.
(use-package savehist
  :init
  (savehist-mode))

;; Emacs minibuffer configurations.
(use-package emacs
  :custom
  ;; Enable context menu. `vertico-multiform-mode' adds a menu in the minibuffer
  ;; to switch display modes.
  (context-menu-mode t)
  ;; Support opening new minibuffers from inside existing minibuffers.
  (enable-recursive-minibuffers t)
  ;; Hide commands in M-x which do not work in the current mode.  Vertico
  ;; commands are hidden in normal buffers. This setting is useful beyond
  ;; Vertico.
  (read-extended-command-predicate #'command-completion-default-include-p)
  ;; Do not allow the cursor in the minibuffer prompt
  (minibuffer-prompt-properties
   '(read-only t cursor-intangible t face minibuffer-prompt)))

(use-package vertico-posframe)
(vertico-posframe-mode 1)

(setq-default cursor-type 'bar)

;; Set default directory for org files
(setq org-directory "~/ORG/")
(setq org-agenda-files (directory-files-recursively "~/ORG/Roam/" "\.org$"))

;; Enable org-babel languages
(org-babel-do-load-languages
 'org-babel-load-languages
 '((emacs-lisp . t)
   (python . t)))

;; Set keybinding for org-capture
(global-set-key (kbd "C-c l") 'org-store-link)
(global-set-key (kbd "C-c a") 'org-agenda)

(use-package org-modern)
(with-eval-after-load 'org (global-org-modern-mode))
(add-hook 'org-mode-hook #'org-indent-mode)
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
 org-ellipsis "…"
 )

;; Set variable font sizes for Org headings
(set-face-attribute 'org-level-1 nil :height 1.5)
(set-face-attribute 'org-level-2 nil :height 1.35)
(set-face-attribute 'org-level-3 nil :height 1.2)
(set-face-attribute 'org-level-4 nil :height 1.1)
(set-face-attribute 'org-level-5 nil :height 1.0)
(set-face-attribute 'org-level-6 nil :height 0.9)
(set-face-attribute 'org-level-7 nil :height 0.8)
(set-face-attribute 'org-level-8 nil :height 0.7)

(setq
 org-startup-with-inline-images t
 org-use-fast-todo-selection t
 org-todo-keywords
 '((sequence "TODO( t )" "CALL(l)" "MEETING(m)" "TEST(e)" "HOMEWORK(h)" "PROJECT(p)" "|" "DONE(d)" "CANCELLED(c)"))
 org-todo-keyword-faces
 '(("TODO" . (:background "#458588" :foreground "#fbf1c7" :weight bold))
   ("CALL" . (:background "#689d6a" :foreground "#fbf1c7" :weight bold))
   ("MEETING" . (:background "#d65d0e" :foreground "#fbf1c7" :weight bold))
   ("TEST" . (:background "#cc241d" :foreground "#fbf1c7" :weight bold))
   ("HOMEWORK" . (:background "#b16286" :foreground "#fbf1c7" :weight bold))
   ("PROJECT" . (:background "#d79921" :foreground "#fbf1c7" :weight bold))
   ("DONE" . (:background "#98971a" :foreground "#282828" :weight bold))
   ("CANCELLED" . (:background "#3c3836" :foreground "#928374" :weight bold :strike-through t))))

(use-package org-roam
:init
(setq org-roam-directory (file-truename "~/ORG/Roam/")
      org-roam-dailies-directory "journal/"
      org-roam-completion-everywhere t
)

:bind (("C-c n l" . org-roam-buffer-toggle)
       ("C-c n f" . org-roam-node-find)
       ("C-c n i" . org-roam-node-insert)
       :map org-roam-dailies-map
       )
:bind-keymap ("C-c n d" . org-roam-dailies-map)
:config
(require 'org-roam-dailies)
(setq org-roam-dailies-capture-templates
      '(("d" "default" entry
         "* %?\nTaken: %(format-time-string \"<%Y-%m-%d %H:%M>\")"
         :if-new (file+head "%<%Y-%m-%d>.org"
                            "#+title: %<%Y-%m-%d>\n"))))
(org-roam-db-autosync-mode))

(use-package org-roam-ui
:if (locate-library "org-roam-ui")   ;; guard if not in packages.nix
:after org-roam
:custom
(org-roam-ui-sync-theme t)
(org-roam-ui-follow t)
(org-roam-ui-update-on-save t)
(org-roam-ui-open-on-start t))

(use-package tree-sitter
  ;; The :config block ensures this code runs AFTER the package is loaded
  :config
  (global-tree-sitter-mode)
  ;; Optional: Add a hook to turn on the minor mode for specific major modes
  (add-hook 'prog-mode-hook #'tree-sitter-hl-mode))

;; The tree-sitter-langs package provides the language grammars
(use-package tree-sitter-langs)
