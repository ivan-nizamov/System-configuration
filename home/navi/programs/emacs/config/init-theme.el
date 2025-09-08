;;; init-theme.el --- Theme bootstrap  -*- lexical-binding: t; -*-

;; Disable any leftover themes to avoid mixing
(mapc #'disable-theme custom-enabled-themes)

(defun my/theme-ensure-on-path (feature)
  "Ensure FEATURE's directory is on `custom-theme-load-path`."
  (when-let* ((lib (locate-library feature))
              (dir (file-name-directory lib)))
    (add-to-list 'custom-theme-load-path dir)))

;; Themes provided by Nix
(dolist (th '("gruvbox-theme" "catppuccin-theme" "kanagawa-themes"))
  (my/theme-ensure-on-path th))

;; Load preferred theme with graceful fallbacks
(or (ignore-errors (load-theme 'gruvbox-dark-medium t))
    (ignore-errors (load-theme 'catppuccin-mocha t))
    (ignore-errors (load-theme 'kanagawa-wave t))
    (load-theme 'tsdh-dark t))

(provide 'init-theme)