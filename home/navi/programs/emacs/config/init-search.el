;;; init-search.el --- Enhanced file and project search

;; ---------------------------------------------------------------------------
;; Projectile - Project Management
;; ---------------------------------------------------------------------------
(use-package projectile
  :ensure t
  :defer t
  :diminish projectile-mode
  :bind-keymap
  ("C-c p" . projectile-command-map)
  :init
  (setq projectile-completion-system 'default) ; Use the default completion system
  (setq projectile-enable-caching t)           ; Enable caching for better performance
  (setq projectile-indexing-method 'alien)     ; Use external indexing for large projects
  (setq projectile-sort-order 'recently-active)
  (setq projectile-cache-file (expand-file-name "projectile.cache" user-emacs-directory))
  (setq projectile-known-projects-file (expand-file-name "projectile-bookmarks.eld" user-emacs-directory))
  ;; Defer projectile initialization until first use
  (add-hook 'find-file-hook 
            (lambda () 
              (when (and (not (bound-and-true-p projectile-mode))
                         (buffer-file-name))
                (projectile-mode +1))))
  :config
  ;; Performance optimizations
  (setq projectile-file-exists-remote-cache-expire nil)
  (setq projectile-require-project-root nil)
  
  ;; Create a custom keymap prefix for all search-related commands
  (define-prefix-command 'search-command-map)
  (global-set-key (kbd "C-c s") 'search-command-map)
  
  ;; Set the default search directory to the project root if in a project
  (setq consult-project-function
        (lambda (_) (when (projectile-project-root) (projectile-project-root)))))

;; ---------------------------------------------------------------------------
;; Consult - Enhanced search commands
;; ---------------------------------------------------------------------------
(use-package consult
  :ensure t
  :defer t
  :bind (;; Search and navigation
         ;; Project-specific search bindings
         ("C-c s f" . consult-find)        ; Find file by name using fd
         ("C-c s g" . consult-grep)        ; Search for pattern with grep
         ("C-c s r" . consult-ripgrep)     ; Search for pattern with ripgrep
         ("C-c s l" . consult-line)        ; Search for line in current buffer
         ("C-c s i" . consult-imenu)       ; Jump to symbol in buffer
         ("C-c s m" . consult-multi-occur) ; Search in multiple buffers
         
         ;; Buffer and file navigation
         ("C-x b" . consult-buffer)        ; Enhanced buffer switching
         ("C-x C-r" . consult-recent-file) ; Find recent files
         ("C-x 4 b" . consult-buffer-other-window) ; Buffer in other window
         
         ;; Register access
         ("M-g r" . consult-register)
         ("M-g m" . consult-bookmark)
         
         ;; More convenient M-g bindings
         ("M-g g" . consult-goto-line)     ; Go to line
         ("M-g M-g" . consult-goto-line)   ; Go to line
         ("M-g o" . consult-outline)       ; Navigate outline
         
         ;; Isearch integration
         :map isearch-mode-map
         ("M-e" . consult-isearch-history) ; Isearch history
         ("M-s e" . consult-isearch-history)
         ("M-s l" . consult-line)          ; Jump to line from isearch
         ("M-s L" . consult-line-multi))   ; Line in multiple buffers
  
  :init
  ;; Use Consult to select xref locations with preview
  (setq xref-show-xrefs-function #'consult-xref
        xref-show-definitions-function #'consult-xref)
  :config
  ;; Configure preview functionality with better performance
  (consult-customize
   consult-theme
   :preview-key '(:debounce 0.3 any)
   consult-ripgrep consult-git-grep consult-grep
   consult-bookmark consult-recent-file consult-xref
   consult--source-bookmark consult--source-recent-file
   consult--source-project-recent-file
   :preview-key '(:debounce 0.5 any))
  
  ;; Optimize preview settings for better performance
  (setq consult-preview-key '(:debounce 0.3 any))
  (setq consult-preview-max-size 5242880) ; Reduced to 5MB from 10MB
  (setq consult-narrow-key "<")           ; Narrowing key
  (setq consult-line-start-from-top nil)
  (setq consult-async-min-input 2)        ; Require at least 2 chars for async commands
  (setq consult-async-refresh-delay 0.2)  ; Delay before refreshing async results
  )

;; ---------------------------------------------------------------------------
;; Ripgrep integration
;; ---------------------------------------------------------------------------
;; Ensure ripgrep is available and configured
(when (executable-find "rg")
  (setq consult-ripgrep-args "rg --null --line-buffered --color=never --max-columns=1000 --path-separator / --smart-case --no-heading --line-number --with-filename")
  
  ;; Custom function to search in project with ripgrep
  (defun search-project-with-ripgrep ()
    "Search in project using ripgrep."
    (interactive)
    (let ((default-directory (or (projectile-project-root) default-directory)))
      (call-interactively 'consult-ripgrep)))
  
  ;; Bind to key
  (global-set-key (kbd "C-c s s") 'search-project-with-ripgrep))

;; ---------------------------------------------------------------------------
;; Projectile and Consult integration
;; ---------------------------------------------------------------------------
;; Override projectile find file with consult-find
(with-eval-after-load 'projectile
  (define-key projectile-command-map (kbd "f") 'consult-find)
  
  ;; Custom function to find file in project
  (defun find-file-in-project ()
    "Find file in current project."
    (interactive)
    (if (projectile-project-p)
        (consult-find (projectile-project-root))
      (consult-find)))
  
  ;; Bind to key
  (global-set-key (kbd "C-c s p") 'find-file-in-project))

;; ---------------------------------------------------------------------------
;; Project.el integration
;; ---------------------------------------------------------------------------
;; Make project.el use projectile for project detection
(with-eval-after-load 'project
  (defun project-try-projectile (dir)
    "Try to detect project root in DIR using projectile."
    (when-let ((root (projectile-project-root dir)))
      (cons 'projectile root)))
  
  ;; Add to project root functions
  (add-to-list 'project-find-functions #'project-try-projectile))

;; ---------------------------------------------------------------------------
;; Additional search utilities
;; ---------------------------------------------------------------------------
;; Function to search for a file by name or content
(defun search-file-by-name-or-content ()
  "Search for a file by name or content."
  (interactive)
  (let ((action (completing-read "Search by: " '("Name" "Content") nil t)))
    (cond
     ((string= action "Name") (call-interactively 'consult-find))
     ((string= action "Content") (call-interactively 'consult-ripgrep)))))

;; Bind to a convenient key
(global-set-key (kbd "C-c s a") 'search-file-by-name-or-content)

;; Function to quickly find a file within a project
(defun quick-find-file ()
  "Quickly find a file within the current project or directory."
  (interactive)
  (if (projectile-project-p)
      (call-interactively 'projectile-find-file)
    (call-interactively 'find-file)))

;; Bind to a convenient key
(global-set-key (kbd "C-c f") 'quick-find-file)

(provide 'init-search)
;;; init-search.el ends here

