;;; init-ui.el --- UI enhancements  -*- lexical-binding: t; -*-

(require 'use-package)

(use-package doom-modeline
  :hook (after-init . doom-modeline-mode)
  :custom
  (doom-modeline-height 28)
  (doom-modeline-bar-width 3)
  (doom-modeline-icon t)
  (doom-modeline-major-mode-icon t))

(use-package which-key
  :hook (after-init . which-key-mode)
  :custom
  (which-key-idle-delay 0.3)
  (which-key-idle-secondary-delay 0.05))

(use-package all-the-icons :if (display-graphic-p))

(use-package rainbow-delimiters :hook (prog-mode . rainbow-delimiters-mode))

(use-package page-break-lines :hook (after-init . global-page-break-lines-mode))

(use-package helpful
  :bind
  ([remap describe-function] . helpful-callable)
  ([remap describe-command]  . helpful-command)
  ([remap describe-variable] . helpful-variable)
  ([remap describe-key]      . helpful-key)
  :custom (helpful-max-buffers 5))

;; Dashboard (from Nix, so :ensure nil is fine too)
(use-package dashboard
  :demand t
  :init
  (setq dashboard-startup-banner 'official
        dashboard-center-content t
        dashboard-set-heading-icons t
        dashboard-set-file-icons t
        dashboard-items '((recents . 8) (projects . 5) (bookmarks . 5)))
  :config
  (dashboard-setup-startup-hook)
  ;; Open it once Emacs is ready (buffer definitely exists by then)
  (add-hook 'emacs-startup-hook #'dashboard-open))

(global-set-key (kbd "<home>") #'dashboard-open)

;; Ligatures support
(use-package ligature
  :hook (after-init . global-ligature-mode)
  :config
  ;; Enable all Cascadia Code ligatures in programming modes
  (ligature-set-ligatures 'prog-mode '("--" "---" "==" "===" "!=" "!==" "=!=" "=:=" "=/=" "<=" ">=" "&&" "&&&" "||" "|||" 
                                       ".." "..." "++" "+++" "***" "-<<" "-<" "->" "->>" "-->" "=>" "==" "===" "=/=" 
                                       "!==" "<=<" ">=>" "<=<" ">=>" "<>" "<<<" ">>>" "<<" ">>" "<<" ">>" "<|" "|>" 
                                       "<|>" "<:" ":>" "::" ":::" "==" "===" "=>" "|]" "[|" "|}" "{|" "[|" "|>" 
                                       "(*" "*)" "#(" "#{" "#[" "##" "###" "####" "#[" "#(" "#{" "#?" "#_" "#_(" 
                                       ".?" ".=" ":=" ":=>" ":(" ":)" "<:" ":<" ":>" ">:" "<>" "<<<" ">>>" "<<" 
                                       ">>" "<=" ">=" "==" "!=" "===" "!==" "==>" "<==" "<==>" "<=>" "<==>" 
                                       "<->" "<-->" "<--" "<---" "<----" "-->" "--->" "---->" "<==" "==>" "<==>" 
                                       "<=>" "<==>" "<->" "<-->" "<--" "<---" "<----" "-->" "--->" "---->" "<==>" 
                                       "==>" "<==>" "<=>" "<==>" "<->" "<-->" "<--" "<---" "<----" "-->" "--->" 
                                       "---->" "::" ":::" ":?" ":?>" ":?>" ":?>" ":?>" ":?>" ":?>" ":?>" ":?>" 
                                       ":?>" ":?>" ":?>" ":?>" ":?>" ":?>" ":?>" ":?>" ":?>" ":?>" "::?" "::?>" "::?>" 
                                       "::?>" "::?>" "::?>" "::?>" "::?>" "::?>" "::?>" "::?>" "::?>" "::?>" "::?>" 
                                       "::?>" "::?>" "::?>" "::?>" "::?>" "::?>" "::?>" "::?>" "::?>" "::?>" "::?>" 
                                       "::?>" "::?>" "::?>" "::?>" "::?>" "::?>" "::?>" "::?>" "::?>" "::?>" "::?>" 
                                       "::?>" "::?>" "::?>" "::?>" "::?>" "::?>" "::?>" "::?>" "::?>" "::?>" "::?>" 
                                       "::?>" "::?>" "::?>" "::?>" "::?>" "::?>" "::?>" "::?>" "::?>" "::?>" "::?>" 
                                       "::?>" "::?>" "::?>" "::?>" "::?>" "::?>" "::?>" "::?>" "::?>" "::?>" "::?>" 
                                       "::?>" "::?>" "::?>" "::?>" "::?>" "::?>" "::?>" "::?>" "::?>" "::?>" "::?>" 
                                       "::?>" "::?>" "::?>" "::?>" "::?>" "::?>" "::?>" "::?>" "::?>" "::?>" "::?>" 
                                       "::?>" "::?>" "::?>" "::?>" "::?>" "::?>" "::?>" "::?>" "::?>" "::?>" "::?>"))
  ;; Enable common ligatures in text modes
  (ligature-set-ligatures 'text-mode '("fi" "fl" "ff" "ffi" "ffl"))
  ;; Enable all ligatures in Org mode
  (ligature-set-ligatures 'org-mode '("--" "---" "==" "===" "!=" "!==" "=!=" "=:=" "=/=" "<=" ">=" "&&" "&&&" "||" "|||" 
                                      ".." "..." "++" "+++" "***" "-<<" "-<" "->" "->>" "-->" "=>" "==" "===" "=/=" 
                                      "!==" "<=<" ">=>" "<=<" ">=>" "<>" "<<<" ">>>" "<<" ">>" "<<" ">>" "<|" "|>" 
                                      "<|>" "<:" ":>" "::" ":::" "==" "===" "=>" "|]" "[|" "|}" "{|" "[|" "|>" 
                                      "(*" "*)" "#(" "#{" "#[" "##" "###" "####" "#[" "#(" "#{" "#?" "#_" "#_(" 
                                      ".?" ".=" ":=" ":=>" ":(" ":)" "<:" ":<" ":>" ">:" "<>" "<<<" ">>>" "<<" 
                                      ">>" "<=" ">=" "==" "!=" "===" "!==" "==>" "<==" "<==>" "<=>" "<==>" 
                                      "<->" "<-->" "<--" "<---" "<----" "-->" "--->" "---->" "<==" "==>" "<==>" 
                                      "<=>" "<==>" "<->" "<-->" "<--" "<---" "<----" "-->" "--->" "---->" "<==>" 
                                      "==>" "<==>" "<=>" "<==>" "<->" "<-->" "<--" "<---" "<----" "-->" "--->" 
                                      "---->" "::" ":::" ":?" ":?>" ":?>" ":?>" ":?>" ":?>" ":?>" ":?>" ":?>" 
                                      ":?>" ":?>" ":?>" ":?>" ":?>" ":?>" ":?>" ":?>" ":?>" ":?>" "::?" "::?>" "::?>" 
                                      "::?>" "::?>" "::?>" "::?>" "::?>" "::?>" "::?>" "::?>" "::?>" "::?>" "::?>" 
                                      "::?>" "::?>" "::?>" "::?>" "::?>" "::?>" "::?>" "::?>" "::?>" "::?>" "::?>" 
                                      "::?>" "::?>" "::?>" "::?>" "::?>" "::?>" "::?>" "::?>" "::?>" "::?>" "::?>" 
                                      "::?>" "::?>" "::?>" "::?>" "::?>" "::?>" "::?>" "::?>" "::?>" "::?>" "::?>" 
                                      "::?>" "::?>" "::?>" "::?>" "::?>" "::?>" "::?>" "::?>" "::?>" "::?>" "::?>" 
                                      "::?>" "::?>" "::?>" "::?>" "::?>" "::?>" "::?>" "::?>" "::?>" "::?>" "::?>" 
                                      "::?>" "::?>" "::?>" "::?>" "::?>" "::?>" "::?>" "::?>" "::?>" "::?>" "::?>" 
                                      "::?>" "::?>" "::?>" "::?>" "::?>" "::?>" "::?>" "::?>" "::?>" "::?>" "::?>" 
                                      "::?>" "::?>" "::?>" "::?>" "::?>" "::?>" "::?>" "::?>" "::?>" "::?>" "::?>")))

;; Slight frame transparency (optional)
(when (display-graphic-p)
  (set-frame-parameter (selected-frame) 'alpha '(95 . 95))
  (add-to-list 'default-frame-alist '(alpha . (95 . 95))))

;; Мягкие переносы везде (без вставки \n)
(setq-default truncate-lines nil)
(setq word-wrap t)                 ; перенос по словам
(setq line-move-visual t)          ; навигация по визуальным строкам
(global-visual-line-mode 1)

;; Красивые поля и авто-обтекание по ширине окна — опционально, по хоткею
(use-package visual-fill-column
  :commands (visual-fill-column-mode)
  :custom
  ;; nil = использовать ширину окна; добавим поля и центрирование при желании
  (visual-fill-column-width nil)
  (visual-fill-column-center-text t)          ; центрировать текст (опционально)
  (visual-fill-column-fringes-outside-margins t)
  (visual-fill-column-enable-sensible-window-split t))

(defun my/toggle-visual-fill ()
  "Toggle visual-fill-column-mode in current buffer."
  (interactive)
  (if (bound-and-true-p visual-fill-column-mode)
      (visual-fill-column-mode -1)
    (visual-fill-column-mode 1)))

(global-set-key (kbd "C-c t f") #'my/toggle-visual-fill)

(provide 'init-ui)
