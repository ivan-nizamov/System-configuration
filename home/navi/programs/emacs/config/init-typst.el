;;; init-typst.el --- Typst ts-mode + Eglot + format  -*- lexical-binding: t; -*-

;; Major mode (tree-sitter)
(use-package typst-ts-mode
  :mode "\\.typ\\'"
  :init
  (setq typst-ts-mode-watch-options "--open") ;; used by CLI preview tools
  :hook
  (typst-ts-mode . eglot-ensure)
  (typst-ts-mode . (lambda ()
                     (when (and (executable-find "typstfmt")
                                (fboundp 'typstfmt-buffer))
                       (add-hook 'before-save-hook #'typstfmt-buffer nil t)))))

;; Eglot: Tinymist server for Typst
(with-eval-after-load 'eglot
  (add-to-list 'eglot-server-programs
               '(typst-ts-mode . ("tinymist" "lsp"))))

;; Minimal formatter wrapper (only if not already defined)
(unless (fboundp 'typstfmt-buffer)
  (defun typstfmt-buffer ()
    "Format current buffer with typstfmt."
    (interactive)
    (when (executable-find "typstfmt")
      (let ((pt (point)))
        (call-process-region (point-min) (point-max) "typstfmt" t t nil)
        (goto-char pt)))))

(provide 'init-typst)