;;; init-spell.el --- Jinx spell checking  -*- lexical-binding: t; -*-

(use-package jinx
  :hook ((text-mode . jinx-mode)
         (prog-mode . jinx-mode))
  :bind ([remap ispell-word] . jinx-correct)
  :custom (jinx-languages "en_US ro_RO es_ES"))

;; Keep Flyspell available but off by default (compat)
(use-package flyspell
  :commands (flyspell-mode flyspell-prog-mode)
  :custom (flyspell-issue-message-flag nil))

(provide 'init-spell)