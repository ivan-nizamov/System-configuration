;;; init-misc.el --- Editing QoL  -*- lexical-binding: t; -*-

;; Avy
(use-package avy
  :bind (("C-:" . avy-goto-char)
         ("C-'" . avy-goto-char-2)
         ("M-g f" . avy-goto-line)
         ("M-g w" . avy-goto-word-1))
  :custom (avy-background t) (avy-style 'at-full))

;; Smartparens (you can switch to `electric-pair-mode` if you prefer)
(use-package smartparens
  :hook ((prog-mode . smartparens-mode)
         (org-mode  . smartparens-mode))
  :config (require 'smartparens-config))

;; Multiple cursors
(use-package multiple-cursors
  :bind (("C-S-c C-S-c" . mc/edit-lines)
         ("C->"         . mc/mark-next-like-this)
         ("C-<"         . mc/mark-previous-like-this)
         ("C-c C-<"     . mc/mark-all-like-this)))

;; Expand region
(use-package expand-region
  :bind (("C-=" . er/expand-region)))

;; Programming defaults
(defun my/prog-common ()
  (setq-local show-trailing-whitespace t
              indicate-empty-lines t))
(add-hook 'prog-mode-hook #'my/prog-common)

;; Magit convenience
(use-package magit
  :commands (magit-status)
  :bind (("C-x g" . magit-status)))

;; Rename current buffer + file
(defun my/rename-current-buffer-file ()
  "Rename current buffer and visited file."
  (interactive)
  (let ((filename (buffer-file-name)))
    (unless (and filename (file-exists-p filename))
      (user-error "Buffer is not visiting a file"))
    (let ((new-name (read-file-name "New name: " filename)))
      (cond
       ((vc-backend filename) (vc-rename-file filename new-name))
       (t (rename-file filename new-name t)
          (set-visited-file-name new-name t t)
          (message "Renamed to %s" new-name))))))

(provide 'init-misc)