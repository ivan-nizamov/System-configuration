;;; init-completion.el --- Modern completion system for Emacs

;; ---------------------------------------------------------------------------
;; Vertico - Vertical completion UI in minibuffer
;; ---------------------------------------------------------------------------
(use-package vertico
  :ensure t
  :defer t
  :hook (after-init . vertico-mode)
  :init
  ;; Different scroll margin
  (setq vertico-scroll-margin 0)
  ;; Show more candidates
  (setq vertico-count 12)  ; Reduced from 15
  ;; Grow and shrink the Vertico minibuffer
  (setq vertico-resize t)
  ;; Optionally enable cycling for `vertico-next' and `vertico-previous'.
  (setq vertico-cycle t)
  :bind (:map vertico-map
         ("C-j" . vertico-next)
         ("C-k" . vertico-previous)
         ("C-l" . vertico-exit)
         ("C-f" . vertico-exit-input)))

;; ---------------------------------------------------------------------------
;; Vertico-posframe - Display vertico completions in a posframe
;; ---------------------------------------------------------------------------
(use-package vertico-posframe
  :ensure t
  :defer t
  :after vertico
  :if (display-graphic-p)
  :hook (vertico-mode . vertico-posframe-mode)
  :config
  ;; Center the posframe in the middle of the screen
  (setq vertico-posframe-poshandler #'posframe-poshandler-frame-center)
  
  ;; Simplified appearance for better performance
  (setq vertico-posframe-parameters
        '((left-fringe . 4)
          (right-fringe . 4)
          (internal-border-width . 1))))

;; ---------------------------------------------------------------------------
;; Marginalia - Rich annotations in the minibuffer
;; ---------------------------------------------------------------------------
(use-package marginalia
  :ensure t
  :defer t
  :hook (after-init . marginalia-mode)
  :bind (:map minibuffer-local-map
         ("M-A" . marginalia-cycle)))

;; ---------------------------------------------------------------------------
;; Orderless - Flexible completion style
;; ---------------------------------------------------------------------------
(use-package orderless
  :ensure t
  :defer t
  :hook (after-init . (lambda () 
                       (setq completion-styles '(orderless basic))
                       (setq completion-category-overrides '((file (styles basic partial-completion))))
                       (setq orderless-component-separator #'orderless-escapable-split-on-space))))

;; ---------------------------------------------------------------------------
;; Corfu - In-buffer completion UI
;; ---------------------------------------------------------------------------
(use-package corfu
  :ensure t
  :defer t
  :hook (after-init . global-corfu-mode)
  :custom
  ;; Enable auto completion
  (corfu-auto t)
  ;; Allow cycling through candidates
  (corfu-cycle t)
  ;; Number of candidates to show (reduced)
  (corfu-count 8)
  ;; Popup delay (increased for better performance)
  (corfu-auto-delay 0.2)
  ;; Popup size (reduced)
  (corfu-min-width 40)
  (corfu-max-width 60)
  :bind (:map corfu-map
         ("C-j" . corfu-next)
         ("C-k" . corfu-previous)
         ("C-l" . corfu-complete)
         ("TAB" . corfu-complete)
         ("<tab>" . corfu-complete)))

;; Enable Corfu completion in the minibuffer
(defun corfu-enable-in-minibuffer ()
  "Enable Corfu in the minibuffer if `completion-at-point' is bound."
  (when (where-is-internal #'completion-at-point (current-local-map))
    ;; (setq-local corfu-auto nil) ;; Enable/disable auto completion
    (corfu-mode 1)))
(add-hook 'minibuffer-setup-hook #'corfu-enable-in-minibuffer)

;; ---------------------------------------------------------------------------


;; ---------------------------------------------------------------------------
;; Embark - Context-aware actions
;; ---------------------------------------------------------------------------
(use-package embark
  :ensure t
  :defer t
  :bind
  (("C-." . embark-act)         ;; pick some comfortable binding
   ("C-;" . embark-dwim)        ;; good alternative: M-.
   ("C-h B" . embark-bindings)) ;; alternative for `describe-bindings'
  :config
  ;; Hide the mode line of the Embark live/completions buffers
  (add-to-list 'display-buffer-alist
               '("\\`\\*Embark Collect \\(Live\\|Completions\\)\\*"
                 nil
                 (window-parameters (mode-line-format . none)))))

;; Embark and Consult integration
(use-package embark-consult
  :ensure t
  :defer t
  :after (embark consult)
  :hook
  (embark-collect-mode . consult-preview-at-point-mode))

;; Disable company-mode as we're using Corfu instead
(use-package company
  :ensure t
  :config
  (global-company-mode -1))

(provide 'init-completion)
;;; init-completion.el ends here

