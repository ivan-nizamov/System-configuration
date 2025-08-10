;; init-core.el - Core configuration for Emacs
;; Package management is now handled in default.nix

;; Essential UI settings - applied immediately for better startup experience
(setq inhibit-startup-screen t)
(setq inhibit-startup-message t)
(setq inhibit-startup-echo-area-message t)

;; Disable UI elements immediately
(when (fboundp 'tool-bar-mode) (tool-bar-mode -1))
(when (fboundp 'scroll-bar-mode) (scroll-bar-mode -1))
(when (fboundp 'menu-bar-mode) (menu-bar-mode -1))

;; Defer font and line number setup until after startup
(add-hook 'emacs-startup-hook
  (lambda ()
    (set-face-attribute 'default nil :font "Maple Mono NF CN-16")
    (setq display-line-numbers-type 'relative)
    (global-display-line-numbers-mode 1)))

;; FIXME: Commented out to fix "Symbol's function definition is void: my-org-heading-customizations" error
;; Either define this function or remove this hook if no longer needed
;; (add-hook 'after-init-hook (lambda () (my-org-heading-customizations)))

;; Load additional configurations after package system is ready

;; Additional global configurations can be added here
