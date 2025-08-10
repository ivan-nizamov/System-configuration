;;; init-dired.el --- Enhanced dired configuration for single-buffer usage

;; ---------------------------------------------------------------------------
;; Core Dired Configuration
;; ---------------------------------------------------------------------------
(use-package dired
  :ensure nil ; built-in
  :commands (dired dired-jump)
  :bind (("C-x C-j" . dired-jump)
         :map dired-mode-map
         ;; Use same buffer for directory navigation
         ("RET" . my-dired-find-file)
         ("^" . my-dired-up-directory))
  :config
  ;; Kill the current buffer when selecting a new directory
  (defun my-dired-find-file ()
    "Like `dired-find-file', but kills the current buffer if new file is a directory."
    (interactive)
    (let ((file (dired-get-file-for-visit)))
      (if (file-directory-p file)
          (progn
            (find-alternate-file file)
            (dired-hide-details-mode 1))
        (find-file file))))
  
  (defun my-dired-up-directory ()
    "Like `dired-up-directory', but kills the current buffer."
    (interactive)
    (let ((dir (dired-current-directory)))
      (when (not (string= dir "/"))
        (find-alternate-file ".."))))
  
  ;; Display file details by default
  (add-hook 'dired-mode-hook 'dired-hide-details-mode)
  
  ;; Use human-readable file sizes
  (setq dired-listing-switches "-alh")
  
  ;; Always copy/delete recursively
  (setq dired-recursive-copies 'always)
  (setq dired-recursive-deletes 'always)
  
  ;; Auto refresh dired when files change
  (setq dired-auto-revert-buffer t))

;; ---------------------------------------------------------------------------
;; Dired Hide Dotfiles - Hide or show dotfiles with a keystroke
;; ---------------------------------------------------------------------------
(use-package dired-hide-dotfiles
  :ensure t
  :defer t
  :after dired
  :hook (dired-mode . dired-hide-dotfiles-mode)
  :bind (:map dired-mode-map
         ("." . dired-hide-dotfiles-mode)))

;; ---------------------------------------------------------------------------
;; Additional Dired Enhancements
;; ---------------------------------------------------------------------------
;; Use all-the-icons in dired if available
(use-package all-the-icons-dired
  :ensure t
  :defer t
  :if (display-graphic-p)
  :hook (dired-mode . all-the-icons-dired-mode))

;; Auto-revert dired buffers silently
(setq global-auto-revert-non-file-buffers t)
(setq auto-revert-verbose nil)

;; Function to create a new file
(defun my-dired-create-file (file)
  "Create a new empty FILE. If FILE already exists, signal an error."
  (interactive
   (list (read-file-name "Create file: " (dired-current-directory))))
  (let* ((expanded (expand-file-name file))
         (try expanded)
         (dir (directory-file-name (file-name-directory expanded)))
         new)
    (if (file-exists-p expanded)
        (error "Cannot create file %s: file exists" expanded))
    ;; Create directory if necessary
    (when (and (not (file-exists-p dir))
               (yes-or-no-p (format "Create directory %s?" dir)))
      (make-directory dir t))
    ;; Create the file
    (write-region "" nil expanded t)
    (find-file expanded)))

;; Bind it to "C" in dired-mode
(with-eval-after-load 'dired
  (define-key dired-mode-map (kbd "C") 'my-dired-create-file))

(provide 'init-dired)
;;; init-dired.el ends here

