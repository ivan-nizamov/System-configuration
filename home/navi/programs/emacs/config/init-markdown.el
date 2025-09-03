;;; init-markdown.el --- Markdown support configuration

;; --------------------------------------------------------------------------- ;;
;; Markdown Mode
;; --------------------------------------------------------------------------- ;;
(use-package markdown-mode
  :ensure t
  :mode (("README\\.md\\'" . gfm-mode)
         ("\\.md\\'" . markdown-mode)
         ("\\.markdown\\'" . markdown-mode))
  :init
  (setq markdown-command "pandoc")
  :config
  ;; Enable syntax highlighting in code blocks
  (setq markdown-fontify-code-blocks-natively t)
  
  ;; Enable line wrapping
  (add-hook 'markdown-mode-hook 'visual-line-mode)
  
  ;; Enable auto-fill mode for text wrapping
  (add-hook 'markdown-mode-hook 'auto-fill-mode)
  
  ;; Key bindings for markdown-mode
  (define-key markdown-mode-map (kbd "C-c C-e") 'markdown-preview)
  (define-key markdown-mode-map (kbd "C-c C-k") 'markdown-kill-ring-save)
  (define-key markdown-mode-map (kbd "C-c C-l") 'markdown-insert-link)
  (define-key markdown-mode-map (kbd "C-c C-i") 'markdown-insert-image))

;; --------------------------------------------------------------------------- ;;
;; Markdown TOC
;; --------------------------------------------------------------------------- ;;
(use-package markdown-toc
  :ensure t
  :after markdown-mode
  :commands (markdown-toc-generate-toc))

;; --------------------------------------------------------------------------- ;;
;; Grip Mode (GitHub Readme Instant Preview)
;; --------------------------------------------------------------------------- ;;
(use-package grip-mode
  :ensure t
  :after markdown-mode
  :commands (grip-mode grip-preview)
  :init
  (setq grip-update-after-change nil)
  :config
  ;; Set preview browser
  (setq browse-url-browser-function 'browse-url-default-browser))

(provide 'init-markdown)
;;; init-markdown.el ends here