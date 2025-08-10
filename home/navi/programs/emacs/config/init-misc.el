;;; init-misc.el --- Miscellaneous enhancements and utilities

;; ---------------------------------------------------------------------------
;; Avy - Jump to things in Emacs tree-style
;; ---------------------------------------------------------------------------
;;; Code:

(use-package avy
  :ensure t
  :defer t
  :bind (("C-:" . avy-goto-char)
         ("C-'" . avy-goto-char-2)
         ("M-g f" . avy-goto-line)
         ("M-g w" . avy-goto-word-1))
  :custom
  (avy-background t)
  (avy-style 'at-full))

;; ---------------------------------------------------------------------------
;; Smartparens - Automatic parenthesis pairing
;; ---------------------------------------------------------------------------
(use-package smartparens
  :ensure t
  :defer t
  :hook ((prog-mode . smartparens-mode)
         (org-mode . smartparens-mode))
  :config
  (when (featurep 'smartparens)
    (require 'smartparens-config)
    ;; Turn off some annoyances
    (sp-use-smartparens-bindings)))

;; ---------------------------------------------------------------------------
;; Multiple Cursors - Use multiple cursors at once
;; ---------------------------------------------------------------------------
(use-package multiple-cursors
  :ensure t
  :defer t
  :bind (("C-S-c C-S-c" . mc/edit-lines)
         ("C->" . mc/mark-next-like-this)
         ("C-<" . mc/mark-previous-like-this)
         ("C-c C-<" . mc/mark-all-like-this)))

;; ---------------------------------------------------------------------------
;; Backup and Auto-Save Settings
;; ---------------------------------------------------------------------------
;; Create backup and auto-save directories if they don't exist
(let ((backup-dir (concat user-emacs-directory "backups"))
      (auto-save-dir (concat user-emacs-directory "auto-saves")))
  (unless (file-directory-p backup-dir)
    (make-directory backup-dir t))
  (unless (file-directory-p auto-save-dir)
    (make-directory auto-save-dir t))
  
  (setq backup-directory-alist `(("." . ,backup-dir)))
  (setq auto-save-file-name-transforms `((".*" ,auto-save-dir t))))

;; Optimize auto-save for better performance
(setq auto-save-timeout 30)     ; Increased from 20 to reduce I/O frequency
(setq auto-save-interval 300)   ; Increased from 200 to reduce I/O frequency
(setq auto-save-no-message t)   ; Suppress messages
(setq make-backup-files t)      ; Enable backups
(setq backup-by-copying t)      ; Copy files instead of renaming (safer)
(setq delete-old-versions t)    ; Delete excess backup versions
(setq kept-new-versions 6)      ; Keep 6 new versions
(setq kept-old-versions 2)      ; Keep 2 old versions
(setq version-control t)        ; Use version control for backups

;; ---------------------------------------------------------------------------
;; General Utilities
;; ---------------------------------------------------------------------------
;; Function to rename the current buffer and its file
(defun rename-current-buffer-file ()
  "Renames current buffer and file it is visiting."
  (interactive)
  (let ((name (buffer-name))
        (filename (buffer-file-name)))
    (if (not (and filename (file-exists-p filename)))
        (message "Buffer '%s' is not visiting a file!" name)
      (let ((new-name (read-file-name "New name: " filename)))
        (cond
         ((vc-backend filename) (vc-rename-file filename new-name))
         (t
          (rename-file filename new-name t)
          (set-visited-file-name new-name t t)))))))

;; ---------------------------------------------------------------------------
;; Programming Mode Enhancements
;; ---------------------------------------------------------------------------
;; Flycheck - moved outside of hook for better performance
(use-package flycheck
  :ensure t
  :defer t
  :hook (prog-mode . flycheck-mode)
  :config
  ;; Only enable for specific modes to reduce overhead
  (setq flycheck-check-syntax-automatically '(save mode-enabled))
  (setq flycheck-display-errors-delay 0.3))

(defun setup-prog-mode ()
  "Common settings for all programming modes."
  ;; Show line numbers (only when needed)
  (when (display-graphic-p)
    (display-line-numbers-mode 1))
  
  ;; Ensure tab-aligned columns
  (setq-local tab-width 4)
  
  ;; Performance optimizations
  (setq-local show-trailing-whitespace t)
  (setq-local indicate-empty-lines t))

(add-hook 'prog-mode-hook 'setup-prog-mode)

;; ---------------------------------------------------------------------------
;; Dired Improvements
;; ---------------------------------------------------------------------------
;; All dired configurations have been moved to init-dired.el
;; This includes:
;;  - Single buffer behavior (replacement for dired-single)
;;  - Dired-hide-dotfiles for toggling hidden files
;;  - Other dired customizations and enhancements

