(setq gc-cons-threshold 100000000
      read-process-output-max (* 3 1024 1024))

(require 'package)
(setq package-archives
      '(("gnu"   . "https://elpa.gnu.org/packages/")
        ("melpa" . "https://melpa.org/packages/")))
(package-initialize)
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))
(eval-when-compile (require 'use-package))
(setq use-package-always-ensure t)

;; Основные настройки шрифта
(defconst my/font-family "Maple Mono NF CN")
(defconst my/font-size   15)  ;; pt

(defun my/apply-font (&optional frame)
  "Применить шрифт к FRAME (или к текущему фрейму)."
  (let* ((f (or frame (selected-frame)))
         (font-str (format "%s-%d" my/font-family my/font-size)))
    (when (member my/font-family (font-family-list))
      ;; set-face-attribute для текущего/указанного фрейма
      (with-selected-frame f
        (set-face-attribute 'default f :font font-str)
        (set-face-attribute 'fixed-pitch f :font font-str))
      ;; чтобы новые фреймы сразу были с этим шрифтом
      (add-to-list 'default-frame-alist `(font . ,font-str)))))

;; Применяем сейчас…
(my/apply-font)

;; …и для фреймов, созданных демоном/позже:
(add-hook 'after-make-frame-functions #'my/apply-font)

(use-package gruvbox-theme
  :init
  ;; avoid mixed faces if another theme was active
  (mapc #'disable-theme custom-enabled-themes)
  ;; load without confirmation
  (load-theme 'gruvbox-dark-hard t))

(menu-bar-mode -1)
(tool-bar-mode -1)
(scroll-bar-mode -1)
(setq inhibit-startup-screen t
    inhibit-startup-message t
    inhibit-startup-echo-area-message t
    initial-scratch-message nil
    global-display-line-numbers-mode nil)
(column-number-mode 1)

(use-package vertico :init (vertico-mode 1))
(use-package orderless
  :custom (completion-styles '(orderless basic))
          (completion-category-defaults nil)
          (completion-category-overrides '((file (styles partial-completion)))))
(use-package marginalia :init (marginalia-mode 1))

(use-package org
  :config
  (setq org-hide-leading-stars t
        org-ellipsis " ▾"))

;; Hot-reload literate config (absolute paths, no surprises)
(defvar my/lit-org-file
  (expand-file-name "~/System-configuration/home/navi/programs/emacs/config/emacsV2/config.org"))

(defvar my/tangled-el (expand-file-name "~/.emacs.d/config.el"))

(defun my/reload-config ()
  "Tangle `my/lit-org-file` into ~/.emacs.d/config.el and load it."
  (interactive)
  (require 'org) (require 'ob-tangle)
  (let ((org-confirm-babel-evaluate nil))
    ;; ensure dir exists
    (unless (file-directory-p (file-name-directory my/tangled-el))
      (make-directory (file-name-directory my/tangled-el) t))
    ;; tangle only emacs-lisp blocks into the exact file we want
    (org-babel-tangle-file my/lit-org-file my/tangled-el "emacs-lisp"))
  (if (file-exists-p my/tangled-el)
      (progn (load my/tangled-el nil 'nomessage)
             (message "Reloaded %s" my/tangled-el))
    (user-error "Tangle failed; %s not found" my/tangled-el)))

(global-set-key (kbd "C-c r") #'my/reload-config)
