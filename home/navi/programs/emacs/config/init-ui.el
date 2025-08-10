;; init-ui.el --- Modern UI enhancements for Emacs

;; ---------------------------------------------------------------------------
;; UI Configuration
;; ---------------------------------------------------------------------------
(require 'use-package)

;; ---------------------------------------------------------------------------
;; Doom-modeline - A modern and cool status bar
;; ---------------------------------------------------------------------------
(use-package doom-modeline
  :ensure t
  :defer t
  :hook (after-init . doom-modeline-mode)
  :custom
  (doom-modeline-height 35)
  (doom-modeline-bar-width 3)
  (doom-modeline-icon t)
  (doom-modeline-major-mode-icon t))

;; ---------------------------------------------------------------------------
;; Which-key - Display available keybindings in popup
;; ---------------------------------------------------------------------------
(use-package which-key
  :ensure t
  :defer t
  :hook (after-init . which-key-mode)
  :init
  (setq which-key-idle-delay 0.3)
  (setq which-key-idle-secondary-delay 0.05))

;; ---------------------------------------------------------------------------
;; All-the-icons - A library for inserting developer friendly icons
;; ---------------------------------------------------------------------------
(use-package all-the-icons
  :ensure t
  :defer t
  :if (display-graphic-p))

;; ---------------------------------------------------------------------------
;; Rainbow-delimiters - Highlight nested delimiters with different colors
;; ---------------------------------------------------------------------------
(use-package rainbow-delimiters
  :ensure t
  :defer t
  :hook (prog-mode . rainbow-delimiters-mode))

;; ---------------------------------------------------------------------------
;; Page-break-lines - Display ^L as tidy horizontal lines
;; ---------------------------------------------------------------------------
(use-package page-break-lines
  :ensure t
  :defer t
  :hook (after-init . global-page-break-lines-mode))

;; ---------------------------------------------------------------------------
;; Helpful - a better Emacs help buffer
;; ---------------------------------------------------------------------------
(use-package helpful
  :ensure t
  :defer t
  :custom
  (helpful-max-buffers 5)
  :bind
  ([remap describe-function] . helpful-callable)
  ([remap describe-command] . helpful-command)
  ([remap describe-variable] . helpful-variable)
  ([remap describe-key] . helpful-key))

;; ---------------------------------------------------------------------------
;; Dashboard - An extensible emacs startup screen
;; ---------------------------------------------------------------------------
(use-package dashboard
  :ensure t
  :defer t
  :init
  ;; Always show dashboard on startup
  (add-hook 'emacs-startup-hook 'dashboard-setup-startup-hook)
  :config
  (setq dashboard-startup-banner 'official)
  (setq dashboard-items '((recents  . 5)
                          (projects . 5)))
  
  ;; Simplified banner title
  (setq dashboard-banner-logo-title "Welcome back, mighty Emacs wizard!")
  
  ;; Disable footer for faster loading
  (setq dashboard-set-footer nil))

;; ---------------------------------------------------------------------------
;; Additional UI improvements
;; ---------------------------------------------------------------------------
;; Prevents the scratch message from appearing in the buffer
(setq initial-scratch-message nil)

;; Disable the blinking cursor
(blink-cursor-mode 0)

;; Smooth scrolling
(setq scroll-conservatively 101)
(setq mouse-wheel-scroll-amount '(1 ((shift) . 1))) ;; one line at a time
(setq mouse-wheel-progressive-speed nil)           ;; don't accelerate scrolling
(setq mouse-wheel-follow-mouse 't)                 ;; scroll window under mouse
(setq scroll-step 1)                               ;; keyboard scroll one line at a time

;; Fringe settings
(set-fringe-mode '(8 . 8)) ;; set left and right fringe width

;; Set frame transparency (95% for both active and inactive states)
(set-frame-parameter (selected-frame) 'alpha '(95 . 95))
(add-to-list 'default-frame-alist '(alpha . (95 . 95)))

(provide 'init-ui)
;;; init-ui.el ends here

