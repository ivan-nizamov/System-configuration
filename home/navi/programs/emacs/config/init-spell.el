;;; init-spell.el --- Simple spell checking with Jinx

;; Use Jinx for modern spell checking (no external dependencies)
(use-package jinx
  :ensure t
  :defer t
  :hook ((text-mode . jinx-mode)
         (prog-mode . jinx-mode))
  :bind ([remap ispell-word] . jinx-correct)
  :config
  ;; Use native spell checker
  (setq jinx-languages "en_US"))

;; Optional: Basic flyspell for compatibility
(use-package flyspell
  :ensure t
  :defer t
  :commands (flyspell-mode flyspell-prog-mode)
  :config
  ;; Disable by default, enable manually if needed
  (setq flyspell-issue-message-flag nil))

(provide 'init-spell)
;;; init-spell.el ends here
