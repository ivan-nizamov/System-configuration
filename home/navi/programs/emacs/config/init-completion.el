;;; init-completion.el --- Minibuffer + in-buffer completion  -*- lexical-binding: t; -*-

;; Vertico
(use-package vertico
  :hook (after-init . vertico-mode)
  :custom
  (vertico-scroll-margin 0)
  (vertico-count 12)
  (vertico-resize t)
  (vertico-cycle t)
  :bind (:map vertico-map
         ("C-j" . vertico-next)
         ("C-k" . vertico-previous)
         ("C-l" . vertico-exit)
         ("C-f" . vertico-exit-input)))

;; Vertico posframe (GUI only)
(use-package vertico-posframe
  :after vertico
  :if (display-graphic-p)
  :hook (vertico-mode . vertico-posframe-mode)
  :custom
  (vertico-posframe-poshandler #'posframe-poshandler-frame-center)
  (vertico-posframe-parameters '((left-fringe . 4) (right-fringe . 4) (internal-border-width . 1))))

;; Marginalia
(use-package marginalia
  :hook (after-init . marginalia-mode)
  :bind (:map minibuffer-local-map
         ("M-A" . marginalia-cycle)))

;; Orderless
(use-package orderless
  :demand t
  :config
  (setq completion-styles '(orderless basic)
        ;; Don't inherit defaults; be explicit
        completion-category-defaults nil
        completion-category-overrides '((file (styles basic partial-completion))))
  (setq orderless-component-separator #'orderless-escapable-split-on-space))

;; Corfu (CAPF UI)
(use-package corfu
  :hook (after-init . global-corfu-mode)
  :custom
  (corfu-auto t)
  (corfu-cycle t)
  (corfu-count 12)
  (corfu-auto-delay 0.2)
  (corfu-min-width 30)
  (corfu-max-width 80)
  :bind (:map corfu-map
         ("C-j" . corfu-next)
         ("C-k" . corfu-previous)
         ("TAB" . corfu-complete)
         ("<tab>" . corfu-complete)))

(use-package corfu-popupinfo
  :after corfu
  :ensure nil
  :hook (corfu-mode . corfu-popupinfo-mode)
  :custom
  (corfu-popupinfo-delay 0.25))

;; CAPE sources (small, useful set)
(use-package cape
  :init
  (dolist (fn '(cape-file cape-dabbrev cape-keyword))
    (add-to-list 'completion-at-point-functions fn)))

;; Use TAB to indent or complete
(setq tab-always-indent 'complete)

;; Corfu in minibuffer when CAPF exists (compatible with Vertico)
(defun my/corfu-enable-in-minibuffer ()
  (when (where-is-internal #'completion-at-point (current-local-map))
    (corfu-mode 1)))
(add-hook 'minibuffer-setup-hook #'my/corfu-enable-in-minibuffer)

;; Embark
(use-package embark
  :bind (("C-." . embark-act)
         ("C-;" . embark-dwim)
         ("C-h B" . embark-bindings))
  :config
  (add-to-list 'display-buffer-alist
               '("\\`\\*Embark Collect \\(Live\\|Completions\\)\\*"
                 nil (window-parameters (mode-line-format . none)))))

(use-package embark-consult
  :after (embark consult)
  :hook (embark-collect-mode . consult-preview-at-point-mode))

(provide 'init-completion)