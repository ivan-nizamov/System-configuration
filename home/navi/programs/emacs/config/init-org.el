;;; init-org.el --- Enhanced Org mode configuration

;; ---------------------------------------------------------------------------
;; Org Mode - Basic configuration
;; ---------------------------------------------------------------------------
(use-package org
  :ensure t
  :defer t
  :mode ("\\.org\\'" . org-mode)
  :bind (("C-c a" . org-agenda)
         ("C-c c" . org-capture)
         ("C-c l" . org-store-link))
  :init
  ;; Defer agenda file discovery until org-agenda is called
  (defun my-org-agenda-files ()
    "Dynamically get org agenda files, excluding orar.org"
    (when (file-directory-p "~/ORG/Roam")
      (remove "~/ORG/Roam/orar.org"
              (directory-files-recursively "~/ORG/Roam" "\\.org$"))))
  
  ;; Set agenda files function instead of immediate evaluation
  (setq org-agenda-files-function #'my-org-agenda-files)
  
  ;; Hide emphasis markers like *bold* and /italic/
  (setq org-hide-emphasis-markers t)
  
  ;; Enable automatic image display
  (setq org-startup-with-inline-images t)
  
  ;; Set default image width
  (setq org-image-actual-width 500)
  
  ;; Enable fast TODO selection
  (setq org-use-fast-todo-selection t)
  
  ;; Setup TODO keywords
  (setq org-todo-keywords
        '((sequence "TODO(t)" "CALL(l)" "MEETING(m)" "TEST(e)" "HOMEWORK(h)" "PROJECT(p)" "|" "DONE(d)" "CANCELLED(c)")))
      
  ;; Define TODO keyword faces for the quick selection menu
  (setq org-todo-keyword-faces
        '(("TODO" . (:background "#458588" :foreground "#fbf1c7" :weight bold))
          ("CALL" . (:background "#689d6a" :foreground "#fbf1c7" :weight bold))
          ("MEETING" . (:background "#d65d0e" :foreground "#fbf1c7" :weight bold))
          ("TEST" . (:background "#cc241d" :foreground "#fbf1c7" :weight bold))
          ("HOMEWORK" . (:background "#b16286" :foreground "#fbf1c7" :weight bold))
          ("PROJECT" . (:background "#d79921" :foreground "#fbf1c7" :weight bold))
          ("DONE" . (:background "#98971a" :foreground "#282828" :weight bold))
          ("CANCELLED" . (:background "#3c3836" :foreground "#928374" :weight bold :strike-through t))))
  
  ;; Make the indentation look nicer
  (setq org-startup-indented t)
  
  ;; Don't fold everything on startup
  (setq org-startup-folded 'content)
  
  ;; Add some structure templates for common blocks
  (with-eval-after-load 'org
    (require 'org-tempo)
    (add-to-list 'org-structure-template-alist '("sh" . "src shell"))
    (add-to-list 'org-structure-template-alist '("el" . "src emacs-lisp"))
    (add-to-list 'org-structure-template-alist '("py" . "src python"))
    (add-to-list 'org-structure-template-alist '("ja" . "src java"))
    (add-to-list 'org-structure-template-alist '("js" . "src javascript"))
    (add-to-list 'org-structure-template-alist '("rs" . "src rust")))
  
  :config
  ;; Set LaTeX preview scale
  (setq org-format-latex-options (plist-put org-format-latex-options :scale 2.75))
  
  ;; Set file applications - use Emacs for PDFs
  (setq org-file-apps '((("\\.pdf\\'" . emacs))))
  
  ;; Integrate with PDF tools
  (use-package org-pdftools
    :ensure t
    :hook (org-mode . org-pdftools-setup-link))
  
  ;; Enable auto-fill-mode in Org files
  (add-hook 'org-mode-hook 'auto-fill-mode))

;; ---------------------------------------------------------------------------
;; Org Modern - Modern-looking Org mode
;; ---------------------------------------------------------------------------
(use-package org-modern
  :ensure t
  :defer t
  :after org
  :hook (org-mode . org-modern-mode)
  :custom
  (org-modern-hide-stars nil) ; We'll use superstar for this
  (org-modern-table t)
  (org-modern-list '((43 . "➤") (45 . "–") (42 . "•")))
  
  ;; Custom TODO faces with modern colors matching org-todo-keyword-faces
  (setq org-modern-todo-faces
    '(("TODO" . (:inherit org-modern-label :background "#458588" :foreground "#fbf1c7" :weight bold))
      ("CALL" . (:inherit org-modern-label :background "#689d6a" :foreground "#fbf1c7" :weight bold))
      ("MEETING" . (:inherit org-modern-label :background "#d65d0e" :foreground "#fbf1c7" :weight bold))
      ("TEST" . (:inherit org-modern-label :background "#cc241d" :foreground "#fbf1c7" :weight bold))
      ("HOMEWORK" . (:inherit org-modern-label :background "#b16286" :foreground "#fbf1c7" :weight bold))
      ("PROJECT" . (:inherit org-modern-label :background "#d79921" :foreground "#fbf1c7" :weight bold))
      ("DONE" . (:inherit org-modern-label :background "#98971a" :foreground "#282828" :weight bold))
      ("CANCELLED" . (:inherit org-modern-label :background "#3c3836" :foreground "#928374" :weight bold :strike-through t)))))

;; ---------------------------------------------------------------------------
;; Org Superstar - Better bullets and heading stars
;; ---------------------------------------------------------------------------
(use-package org-superstar
  :ensure t
  :defer t
  :after org
  :hook (org-mode . org-superstar-mode)
  :custom
  (org-superstar-headline-bullets-list '(" %G󰓎 %@" " %G󰫢 %@" " %G󰫤 %@" " %G %@" " %G󰫣 %@" " %G󰫥 %@"))
  (org-superstar-leading-bullet " ")
  (org-superstar-special-todo-items t))

;; ---------------------------------------------------------------------------
;; Org Appear - Show markup when editing
;; ---------------------------------------------------------------------------
(use-package org-appear
  :ensure t
  :defer t
  :after org
  :hook (org-mode . org-appear-mode)
  :custom
  (org-appear-autoemphasis t)
  (org-appear-autolinks t)
  (org-appear-autosubmarkers t))

;; ---------------------------------------------------------------------------
;; Org Fragtog - Toggle LaTeX preview automatically
;; ---------------------------------------------------------------------------
(use-package org-fragtog
  :ensure t
  :defer t
  :after org
  :hook (org-mode . org-fragtog-mode))

;; ---------------------------------------------------------------------------
;; Org Download - Drag and drop images
;; ---------------------------------------------------------------------------
(use-package org-download
  :ensure t
  :defer t
  :after org
  :custom
  (org-download-method 'directory)
  (org-download-image-dir "images")
  (org-download-heading-lvl nil)
  :config
  (org-download-enable))

;; ---------------------------------------------------------------------------
;; Org Roam - Personal knowledge management
;; ---------------------------------------------------------------------------
(use-package org-roam
  :ensure t
  :defer t
  :init
  ;; Only initialize org-roam when needed
  (defun my-org-roam-init ()
    "Initialize org-roam when first needed"
    (require 'org-roam-dailies)
    (org-roam-db-autosync-mode))
  :custom
  (org-roam-directory (file-truename "~/ORG/Roam/"))
  (org-roam-dailies-directory "journal/")
  (org-roam-completion-everywhere t)
  :bind (("C-c n l" . org-roam-buffer-toggle)
         ("C-c n f" . org-roam-node-find)
         ("C-c n i" . org-roam-node-insert)
         ("C-c n d" . org-roam-dailies-map)
         :map org-mode-map
         ("C-M-i"    . completion-at-point)
         :map org-roam-dailies-map
         ("Y" . org-roam-dailies-capture-yesterday)
         ("T" . org-roam-dailies-capture-tomorrow))
  :bind-keymap
  ("C-c n d" . org-roam-dailies-map)
  :config
  ;; Initialize org-roam
  (my-org-roam-init)
  
  ;; Configure capture templates
  (setq org-roam-capture-templates 
        `(("d" "default" plain "%?" 
           :target (file+head "${slug}.org" "#+title: ${title}")
           :unnarrowed t)))
  
  ;; Configure daily capture templates
  (setq org-roam-dailies-capture-templates
        '(("d" "default" entry "* %<%I:%M %p>:  %?"
           :if-new (file+head "%<%Y-%m-%d>.org" "#+title: %<%Y-%m-%d>")))))

;; ---------------------------------------------------------------------------
;; Org Roam UI - Visualize your notes
;; ---------------------------------------------------------------------------
(use-package org-roam-ui
  :ensure t
  :after org-roam
  :config
  (setq org-roam-ui-sync-theme t
        org-roam-ui-follow t
        org-roam-ui-update-on-save t
        org-roam-ui-open-on-start t))

;; ---------------------------------------------------------------------------
;; Capture Templates and Agenda
;; ---------------------------------------------------------------------------
;; Set up more capture templates
(with-eval-after-load 'org-capture
  (setq org-capture-templates
        '(("t" "Todo" entry (file+headline "~/ORG/tasks.org" "Tasks")
           "* TODO %?\n  %i\n  %a")
          ("j" "Journal" entry (file+olp+datetree "~/ORG/journal.org")
           "* %?\nEntered on %U\n  %i\n  %a")
          ("n" "Note" entry (file+headline "~/ORG/notes.org" "Notes")
           "* %?\n  %i\n  %a")
          ("i" "Idea" entry (file+headline "~/ORG/ideas.org" "Ideas")
           "* %?\n  %i\n  %U"))))

;; Set up agenda views
(with-eval-after-load 'org-agenda
  (setq org-agenda-custom-commands
        '(("d" "Dashboard"
           ((agenda "" ((org-agenda-span 'day)
                        (org-deadline-warning-days 7)))
            (tags-todo "+PRIORITY=\"A\""
                  ((org-agenda-overriding-header "High Priority")))
            (tags-todo "+homework" 
                  ((org-agenda-overriding-header "Homework")))
            (todo "TEST"
                  ((org-agenda-overriding-header "Testing")))
            (tags "project"
                  ((org-agenda-overriding-header "Projects")))))
          ("w" "Weekly Review"
           ((agenda "" ((org-agenda-span 'week)
                        (org-agenda-start-on-weekday 1)))
            (stuck "")
            (todo "DONE|CANCELLED"
                  ((org-agenda-overriding-header "Completed/Cancelled tasks"))))))))

;; ---------------------------------------------------------------------------
;; LaTeX Preview
;; ---------------------------------------------------------------------------
(use-package latex-preview-pane
  :ensure t
  :commands (latex-preview-pane-mode)
  :config
  (latex-preview-pane-enable))

;; ---------------------------------------------------------------------------
;; Anki integration
;; ---------------------------------------------------------------------------
(use-package anki-editor
  :ensure t
  :after org
  :bind (:map org-mode-map
              (("C-c a" . anki-editor-push-notes))))

(provide 'init-org)
;;; init-org.el ends here

