;;; init-ui.el --- UI enhancements  -*- lexical-binding: t; -*-

(require 'use-package)

(use-package doom-modeline
  :hook (after-init . doom-modeline-mode)
  :custom
  (doom-modeline-height 28)
  (doom-modeline-bar-width 3)
  (doom-modeline-icon t)
  (doom-modeline-major-mode-icon t))

(use-package which-key
  :hook (after-init . which-key-mode)
  :custom
  (which-key-idle-delay 0.3)
  (which-key-idle-secondary-delay 0.05))

(use-package all-the-icons :if (display-graphic-p))

(use-package rainbow-delimiters :hook (prog-mode . rainbow-delimiters-mode))

(use-package page-break-lines :hook (after-init . global-page-break-lines-mode))

(use-package helpful
  :bind
  ([remap describe-function] . helpful-callable)
  ([remap describe-command]  . helpful-command)
  ([remap describe-variable] . helpful-variable)
  ([remap describe-key]      . helpful-key)
  :custom (helpful-max-buffers 5))

;; Dashboard (from Nix, so :ensure nil is fine too)
(use-package dashboard
  :demand t
  :init
  (setq dashboard-startup-banner 'official
        dashboard-center-content t
        dashboard-set-heading-icons t
        dashboard-set-file-icons t
        dashboard-items '((recents . 8) (projects . 5) (bookmarks . 5)))
  :config
  (dashboard-setup-startup-hook)
  ;; Open it once Emacs is ready (buffer definitely exists by then)
  (add-hook 'emacs-startup-hook #'dashboard-open))

(global-set-key (kbd "<home>") #'dashboard-open)

;; Slight frame transparency (optional)
(when (display-graphic-p)
  (set-frame-parameter (selected-frame) 'alpha '(95 . 95))
  (add-to-list 'default-frame-alist '(alpha . (95 . 95))))

(provide 'init-ui)