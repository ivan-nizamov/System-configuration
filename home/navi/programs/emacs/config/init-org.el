;;; init-org.el --- Org essentials  -*- lexical-binding: t; -*-

(require 'cl-lib) ;; for cl-remove-if

;; --- Agenda files (recursive, with one exclusion) --------------------------
(defun my/org-agenda-files ()
  "All .org files under ~/ORG/Roam (incl. journal), excluding orar.org."
  (let* ((root (expand-file-name "~/ORG/Roam"))
         (files (when (file-directory-p root)
                  (directory-files-recursively root "\\.org\\'"))))
    (cl-remove-if (lambda (f) (string-match-p "/orar\\.org\\'" f)) files)))

;; Apply our computed list now AND after Org loads (so defcustom can't clobber it)
(defun my/org-agenda-apply ()
  (setq org-agenda-files (my/org-agenda-files)))

;; Set once after Emacs starts (even if Org hasn't loaded yet)
(add-hook 'after-init-hook #'my/org-agenda-apply)

;; Set again the moment Org is loaded (overrides Org's default)
(with-eval-after-load 'org
  (my/org-agenda-apply))

;; (Optional) Also re-apply when org-agenda loads, if you want to be extra sure
(with-eval-after-load 'org-agenda
  (my/org-agenda-apply))

;; Optional: refresh agenda list after saves in Roam tree
(defun my/org-agenda-refresh-maybe ()
  (when (and buffer-file-name
             (string-prefix-p (expand-file-name "~/ORG/Roam")
                              (file-truename buffer-file-name)))
    (setq org-agenda-files (my/org-agenda-files))))
(add-hook 'after-save-hook #'my/org-agenda-refresh-maybe)

;; --- Org core --------------------------------------------------------------
(use-package org
  :mode ("\\.org\\'" . org-mode)
  :bind (("C-c a" . org-agenda)
         ("C-c c" . org-capture)
         ("C-c l" . org-store-link))
  :config
  ;; Org variables AFTER org is loaded
  (setq org-hide-emphasis-markers t
        org-startup-with-inline-images t
        org-image-actual-width 500
        org-use-fast-todo-selection t
        org-startup-indented t
        org-startup-folded 'content
        ;; Use custom fold indicator (ellipsis) for folded headings
        org-ellipsis " 󰁖"
        org-todo-keywords
        '((sequence "TODO(t)" "CALL(l)" "MEETING(m)" "TEST(e)" "HOMEWORK(h)" "PROJECT(p)" "|" "DONE(d)" "CANCELLED(c)"))
        org-todo-keyword-faces
        '(("TODO" . (:background "#458588" :foreground "#fbf1c7" :weight bold))
          ("CALL" . (:background "#689d6a" :foreground "#fbf1c7" :weight bold))
          ("MEETING" . (:background "#d65d0e" :foreground "#fbf1c7" :weight bold))
          ("TEST" . (:background "#cc241d" :foreground "#fbf1c7" :weight bold))
          ("HOMEWORK" . (:background "#b16286" :foreground "#fbf1c7" :weight bold))
          ("PROJECT" . (:background "#d79921" :foreground "#fbf1c7" :weight bold))
          ("DONE" . (:background "#98971a" :foreground "#282828" :weight bold))
          ("CANCELLED" . (:background "#3c3836" :foreground "#928374" :weight bold :strike-through t))))
  ;; Structure templates
  (require 'org-tempo)
  (dolist (tpl '(("sh" . "src shell") ("el" . "src emacs-lisp") ("py" . "src python")
                 ("ja" . "src java")  ("js" . "src javascript") ("rs" . "src rust")))
    (add-to-list 'org-structure-template-alist tpl))
  ;; Open PDFs in Emacs (FIXED parens)
  (setq org-file-apps '(("\\.pdf\\'" . emacs)))
  ;; LaTeX preview scale
  (setq org-format-latex-options (plist-put org-format-latex-options :scale 2.5))
  ;; Scale Org headings by hierarchy (largest at level 1)
  (let ((org-heading-sizes '(1.35 1.25 1.15 1.1 1.05 1.0 0.95 0.9)))
    (cl-loop for i from 1 to 8
             for h in org-heading-sizes
             do (set-face-attribute (intern (format "org-level-%d" i)) nil :height h)))
  ;; Useful minor modes in Org
  (add-hook 'org-mode-hook #'auto-fill-mode))

;; --- Visual add-ons (guard optional deps) ----------------------------------
(use-package org-modern
  :after org
  :hook (org-mode . org-modern-mode)
  :custom
  (org-modern-hide-stars nil)
  (org-modern-table t)
  ;; Use requested icons for folded and open states
  (org-modern-ellipsis " 󰁖")
  ;; Replace leading stars with your open/unfolded icon across levels
  (org-modern-star '("󱖕" "󱖕" "󱖕" "󱖕" "󱖕" "󱖕" "󱖕" "󱖕"))
  (org-modern-list '((43 . "➤") (45 . "–") (42 . "•"))))


(use-package org-superstar
  :after org
  :hook (org-mode . org-superstar-mode)
  :custom (org-superstar-leading-bullet " "))

(use-package org-appear
  :after org
  :hook (org-mode . org-appear-mode)
  :custom
  (org-appear-autoemphasis t)
  (org-appear-autolinks t)
  (org-appear-autosubmarkers t))

(use-package org-fragtog
  :if (locate-library "org-fragtog")   ;; avoid hard error if not installed
  :after org
  :hook (org-mode . org-fragtog-mode))

(use-package org-download
  :after org
  :custom
  (org-download-method 'directory)
  (org-download-image-dir "images")
  (org-download-heading-lvl nil)
  :config
  (org-download-enable))


;; --- Org-roam --------------------------------------------------------------
(use-package org-roam
  :init
  (setq org-roam-directory (file-truename "~/ORG/Roam/")
        org-roam-dailies-directory "journal/"
        org-roam-completion-everywhere t)
  ;; Keep the top-level roam key bindings and expose the dailies keymap
  :bind (("C-c n l" . org-roam-buffer-toggle)
         ("C-c n f" . org-roam-node-find)
         ("C-c n i" . org-roam-node-insert)
         ;; expose the dailies keymap on prefix C-c n d
         :map org-roam-dailies-map
         ;; make sure the prefix is available as a keymap (see :bind-keymap fallback below)
         ;; (bindings for dailies are defined in :config)
         )
  :bind-keymap ("C-c n d" . org-roam-dailies-map)
  :config
  (require 'org-roam-dailies)
  (org-roam-db-autosync-mode)
  ;; Restore the convenient dailies subkeys so you can use:
  ;;   C-c n d n -> capture for today
  ;;   C-c n d y -> capture for yesterday
  ;;   C-c n d t -> capture for tomorrow
  (when (boundp 'org-roam-dailies-map)
    (define-key org-roam-dailies-map (kbd "n") #'org-roam-dailies-capture-today)
    (define-key org-roam-dailies-map (kbd "y") #'org-roam-dailies-capture-yesterday)
    (define-key org-roam-dailies-map (kbd "t") #'org-roam-dailies-capture-tomorrow))
  
  (setq org-roam-capture-templates
        '(("d" "default" plain "%?"
           :target (file+head "${slug}.org" "#+title: ${title}")
           :unnarrowed t)))
  (setq org-roam-dailies-capture-templates
        '(("d" "default" entry "* %<%H:%M>: %?"
           :if-new (file+head "%<%Y-%m-%d>.org" "#+title: %<%Y-%m-%d>")))))

(use-package org-roam-ui
  :if (locate-library "org-roam-ui")   ;; guard if not in packages.nix
  :after org-roam
  :custom
  (org-roam-ui-sync-theme t)
  (org-roam-ui-follow t)
  (org-roam-ui-update-on-save t)
  (org-roam-ui-open-on-start t))

;; --- Capture & Agenda custom views -----------------------------------------
(with-eval-after-load 'org-capture
  (setq org-capture-templates
        '(("t" "Todo"    entry (file+headline "~/ORG/tasks.org"  "Tasks")
           "* TODO %?\n  %i\n  %a")
          ("j" "Journal" entry (file+olp+datetree "~/ORG/journal.org")
           "* %?\nEntered on %U\n  %i\n  %a")
          ("n" "Note"    entry (file+headline "~/ORG/notes.org"  "Notes")
           "* %?\n  %i\n  %a")
          ("i" "Idea"    entry (file+headline "~/ORG/ideas.org"  "Ideas")
           "* %?\n  %i\n  %U"))))

(with-eval-after-load 'org-agenda
  (setq org-agenda-custom-commands
        '(("d" "Dashboard"
           ((agenda "" ((org-agenda-span 'day)
                        (org-deadline-warning-days 7)))
            (tags-todo "+PRIORITY=\"A\"" ((org-agenda-overriding-header "High Priority")))
            (tags-todo "+homework"       ((org-agenda-overriding-header "Homework")))
            (todo "TEST"                 ((org-agenda-overriding-header "Testing")))
            (tags "project"              ((org-agenda-overriding-header "Projects")))))
          ("w" "Weekly Review"
           ((agenda "" ((org-agenda-span 'week)
                        (org-agenda-start-on-weekday 1)))
            (stuck "")
            (todo "DONE|CANCELLED" ((org-agenda-overriding-header "Completed/Cancelled"))))))))

;; --- PDF / LaTeX / Anki ----------------------------------------------------
(use-package latex-preview-pane
  :commands (latex-preview-pane-mode)
  :config (latex-preview-pane-enable))

(use-package anki-editor :after org)

(provide 'init-org)