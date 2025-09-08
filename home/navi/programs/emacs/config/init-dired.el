;;; init-dired.el --- Single-buffer Dired, icons, QoL  -*- lexical-binding: t; -*-

(use-package dired
  :ensure nil
  :commands (dired dired-jump)
  :bind (("C-x C-j" . dired-jump)
         :map dired-mode-map
         ("RET" . my/dired-find-file)
         ("^"   . my/dired-up-directory)
         ("C"   . my/dired-create-file))
  :init
  (setq dired-listing-switches "-alh"
        dired-recursive-copies 'always
        dired-recursive-deletes 'always
        dired-auto-revert-buffer t)
  :config
  (add-hook 'dired-mode-hook #'dired-hide-details-mode)

  (defun my/dired-find-file ()
    "Open thing at point; reuse buffer for directories."
    (interactive)
    (let ((file (dired-get-file-for-visit)))
      (if (file-directory-p file)
          (progn (find-alternate-file file) (dired-hide-details-mode 1))
        (find-file file))))

  (defun my/dired-up-directory ()
    "Go up a dir, reusing buffer."
    (interactive)
    (let ((dir (dired-current-directory)))
      (unless (string= dir "/") (find-alternate-file ".."))))

  (defun my/dired-create-file (file)
    "Create empty FILE, creating parent dirs if necessary."
    (interactive (list (read-file-name "Create file: " (dired-current-directory))))
    (let* ((expanded (expand-file-name file))
           (dir (file-name-directory expanded)))
      (when (file-exists-p expanded) (user-error "File exists: %s" expanded))
      (unless (file-exists-p dir)
        (when (yes-or-no-p (format "Create directory %s?" dir))
          (make-directory dir t)))
      (write-region "" nil expanded t)
      (find-file expanded))))

(use-package dired-hide-dotfiles
  :hook (dired-mode . dired-hide-dotfiles-mode)
  :bind (:map dired-mode-map
         ("." . dired-hide-dotfiles-mode)))

(use-package all-the-icons-dired
  :if (display-graphic-p)
  :hook (dired-mode . all-the-icons-dired-mode))

(setq global-auto-revert-non-file-buffers t
      auto-revert-verbose nil)

(provide 'init-dired)