;;; init-theme.el --- Centralized theme configuration

;; ---------------------------------------------------------------------------
;; Theme Configuration - Gruvbox Dark
;; ---------------------------------------------------------------------------

;; Disable any enabled themes to prevent theme mixing
(mapc #'disable-theme custom-enabled-themes)

;; Install and load Gruvbox Dark theme with deferred loading
(use-package gruvbox-theme
  :ensure t
  :defer t
  :init
  ;; Load theme after startup for better performance
  (add-hook 'emacs-startup-hook
    (lambda ()
      (load-theme 'gruvbox-dark-medium t)))
  :config
  ;; Theme-specific configurations can go here
  nil)

(provide 'init-theme)
;;; init-theme.el ends here
