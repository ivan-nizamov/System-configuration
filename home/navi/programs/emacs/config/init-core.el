;;; init-core.el --- Core settings  -*- lexical-binding: t; -*-

;; Quiet startup
(setq inhibit-startup-screen t
      inhibit-startup-message t
      inhibit-startup-echo-area-message t
      initial-scratch-message nil)

;; Disable chrome early
(when (fboundp 'tool-bar-mode)   (tool-bar-mode -1))
(when (fboundp 'scroll-bar-mode) (scroll-bar-mode -1))
(when (fboundp 'menu-bar-mode)   (menu-bar-mode -1))

;; Sensible defaults
(setq-default indent-tabs-mode nil
              tab-width 4
              fill-column 100)
(setq sentence-end-double-space nil)

;; Smooth-ish scrolling
(setq scroll-conservatively 101
      mouse-wheel-scroll-amount '(1 ((shift) . 1))
      mouse-wheel-progressive-speed nil
      mouse-wheel-follow-mouse t
      scroll-step 1)

;; Fringe + cursor
(set-fringe-mode '(8 . 8))
(blink-cursor-mode 0)

;; Font: only set if present
(add-hook 'emacs-startup-hook
  (lambda ()
    (when (member "Maple Mono NF CN" (font-family-list))
      (set-face-attribute 'default nil :family "Maple Mono NF CN" :height 160))))

;; Backups & autosaves -> ~/.emacs.d/{backups,auto-saves}
(let ((backup-dir (expand-file-name "backups" user-emacs-directory))
      (autosv-dir (expand-file-name "auto-saves" user-emacs-directory)))
  (setq backup-directory-alist `(("." . ,backup-dir))
        auto-save-file-name-transforms `((".*" ,autosv-dir t))
        auto-save-timeout 30
        auto-save-interval 300
        auto-save-no-message t
        make-backup-files t
        backup-by-copying t
        delete-old-versions t
        kept-new-versions 6
        kept-old-versions 2
        version-control t))

;; GCMH (smoother GC) — installed via Nix
(use-package gcmh
  :hook (after-init . gcmh-mode)
  :custom (gcmh-idle-delay 0.5)
  :config (setq gcmh-high-cons-threshold (* 64 1024 1024)))

(provide 'init-core)