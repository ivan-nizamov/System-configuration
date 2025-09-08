;;; init-markdown.el --- Markdown support  -*- lexical-binding: t; -*-

(use-package markdown-mode
  :mode (("README\\.md\\'" . gfm-mode)
         ("\\.md\\'" . markdown-mode)
         ("\\.markdown\\'" . markdown-mode))
  :init
  (setq markdown-command "pandoc"
        markdown-fontify-code-blocks-natively t)
  :hook ((markdown-mode . visual-line-mode)
         (markdown-mode . auto-fill-mode))
  :bind (:map markdown-mode-map
         ("C-c C-e" . markdown-preview)
         ("C-c C-k" . markdown-kill-ring-save)
         ("C-c C-l" . markdown-insert-link)
         ("C-c C-i" . markdown-insert-image)))

(use-package markdown-toc :after markdown-mode :commands markdown-toc-generate-toc)

(use-package grip-mode
  :after markdown-mode
  :commands (grip-mode grip-preview)
  :custom (grip-update-after-change nil))

(provide 'init-markdown)