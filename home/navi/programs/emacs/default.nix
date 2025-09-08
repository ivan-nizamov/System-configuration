{ config, pkgs, ... }:

{
  programs.emacs = {
    enable = true;
    extraPackages = import ./packages.nix { inherit pkgs; };

    extraConfig = ''
      ;;; init.el — Nix-first bootstrap

      ;; Perf: larger GC during startup; faster LSP IO
      (setq gc-cons-threshold (* 64 1000 1000)
            gc-cons-percentage 0.6
            read-process-output-max (* 4 1024 1024)
            inhibit-compacting-font-caches t)

      ;; package.el inert, Nix drives packages
      (setq package-enable-at-startup nil
            package-quickstart       nil)
      (require 'use-package)
      (setq use-package-always-ensure    nil
            use-package-always-defer     t
            use-package-expand-minimally t)

      ;; Keep Custom outside store
      (setq custom-file (expand-file-name "var/custom.el" user-emacs-directory))
      (when (file-exists-p custom-file) (load custom-file))

      ;; Paths for our modules
      (add-to-list 'load-path (expand-file-name "config" user-emacs-directory))

      ;; Load order: theme first (prevents flash), then core, then features
      ;; Using `require` with `'noerror` to prevent hard crashes if a file is missing
      (require 'init-theme nil 'noerror)
      (require 'init-core nil 'noerror)
      (require 'init-completion nil 'noerror)
      (require 'init-search nil 'noerror)
      (require 'init-ui nil 'noerror)
      (require 'init-spell nil 'noerror)
      (require 'init-org nil 'noerror)
      (require 'init-markdown nil 'noerror)
      (require 'init-dired nil 'noerror)
      (require 'init-typst nil 'noerror)
      (require 'init-misc nil 'noerror)

      ;; Normalize GC after startup
      (add-hook 'emacs-startup-hook
        (lambda ()
          (setq gc-cons-threshold (* 2 1000 1000)
                gc-cons-percentage 0.1)
          (message "Emacs ready in %s with %d GCs."
                   (format "%.2f s"
                     (float-time (time-subtract after-init-time before-init-time)))
                   gcs-done)))

      ;; Project dirs you referenced elsewhere
      (setq org-directory "~/ORG"
            org-roam-directory "~/ORG/Roam")

      ;; YAS
      (with-eval-after-load 'yasnippet
        (setq yas-snippet-dirs '("~/ORG/snippets"))
        (yas-global-mode 1))

      ;; ispell defaults (Jinx is the main checker)
      (setq ispell-program-name "hunspell"
            ispell-dictionary   "en_US")
    '';
  };

  home.file = {
    ".emacs.d/config" = { source = ./config; recursive = true; };
    ".emacs.d/early-init.el".text = ''
      (setq package-enable-at-startup nil
            package-quickstart       nil)
    '';
  };

  home.activation.createEmacsDirectories = ''
    $DRY_RUN_CMD mkdir -p $VERBOSE_ARG "$HOME/.emacs.d/backups"
    $DRY_RUN_CMD mkdir -p $VERBOSE_ARG "$HOME/.emacs.d/auto-saves"
    $DRY_RUN_CMD mkdir -p $VERBOSE_ARG "$HOME/.emacs.d/var"
  '';

  home.packages = with pkgs; [
    emacs-all-the-icons-fonts
    pandoc
    languagetool

    # Typst toolchain
    typst
    tinymist
    typstfmt
  ];
}