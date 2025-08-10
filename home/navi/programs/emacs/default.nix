# default.nix - Main entry for Emacs configuration
{ config, pkgs, ... }:

{
  programs.emacs = {
    enable = true;

    # Import packages from packages.nix
    extraPackages = import ./packages.nix { inherit pkgs; };

    # Setting the main init file path for custom configuration
    extraConfig = ''
      ;;; init.el - Main configuration
      
      ;; Performance optimizations for startup
      (setq gc-cons-threshold (* 50 1000 1000)) ; Increase GC threshold during startup
      (setq gc-cons-percentage 0.6)
      (setq read-process-output-max (* 1024 1024)) ; 1MB for better subprocess performance
      
      ;; Defer font computation until needed
      (setq inhibit-compacting-font-caches t)
      
      ;; Initialize package.el with optimizations
      (require 'package)
      (setq package-enable-at-startup nil) ; Disable automatic package loading
      (package-initialize)

      ;; Load core and modular configurations
      (add-to-list 'load-path (expand-file-name "config" user-emacs-directory))
      (add-to-list 'load-path (expand-file-name "lisp" user-emacs-directory))
      
      ;; Ensure use-package is available
      (unless (package-installed-p 'use-package)
        (package-refresh-contents)
        (package-install 'use-package))
      
      (eval-when-compile
        (require 'use-package))
      (require 'use-package)
      (setq use-package-always-ensure t)
      (setq use-package-always-defer t) ; Defer package loading by default
      (setq use-package-expand-minimally t) ; Reduce use-package expansion overhead
      
      ;; Load configurations by section - theme first
      (load "init-theme.el")
      (load "init-core.el")
      (load "init-completion.el")
      (load "init-search.el")
      (load "init-ui.el")
      (load "init-spell.el")
      (load "init-org.el")
      (load "init-dired.el")
      (load "init-misc.el")
      
      ;; Reset GC threshold after initialization
      (add-hook 'emacs-startup-hook
                (lambda ()
                  (setq gc-cons-threshold (* 2 1000 1000)) ; 2MB
                  (setq gc-cons-percentage 0.1)
                  (message "Emacs ready in %s with %d garbage collections."
                          (format "%.2f seconds"
                                  (float-time
                                   (time-subtract after-init-time before-init-time)))
                          gcs-done)))
    '';
  };
}
