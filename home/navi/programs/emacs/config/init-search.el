;;; init-search.el --- Project & search  -*- lexical-binding: t; -*-

;; Consult core bindings
(use-package consult
  :bind (("C-x b"   . consult-buffer)
         ("C-x C-r" . consult-recent-file)
         ("M-g g"   . consult-goto-line)
         ("M-g M-g" . consult-goto-line)
         ("M-g o"   . consult-outline)
         ("M-g f"   . consult-flymake)
         ("C-c s l" . consult-line)
         ("C-c s i" . consult-imenu)
         ("C-c s L" . consult-line-multi)
         ("C-c s r" . consult-ripgrep)
         ("C-c s f" . consult-find)))

;; xref with Consult
(setq xref-show-xrefs-function #'consult-xref
      xref-show-definitions-function #'consult-xref)

;; ripgrep args
(when (executable-find "rg")
  (setq consult-ripgrep-args
        "rg --null --line-buffered --color=never --max-columns=1000 --path-separator / --smart-case --no-heading --line-number --with-filename"))

;; Project integration: prefer built-in project.el
(use-package project
  :ensure nil
  :config
  (setq project-switch-commands
        '((project-find-file "Find file")
          (consult-ripgrep "Ripgrep")
          (project-find-dir "Find dir")
          (project-dired "Dired")
          (project-switch-to-buffer "Buffers")
          (project-kill-buffers "Kill buffers"))))

;; Helpers
(defun my/search-project ()
  "Search current project (or CWD) via ripgrep."
  (interactive)
  (let ((default-directory (or (when-let ((pr (project-current))) (project-root pr))
                               default-directory)))
    (call-interactively 'consult-ripgrep)))

(global-set-key (kbd "C-c s s") #'my/search-project)

(provide 'init-search)