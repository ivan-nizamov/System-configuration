# /home/navi/System-configuration/home/navi/home.nix

{ config, pkgs, ... }:

let
  # The small init.el that bootstraps your config.org
  emacs-init-el = pkgs.writeText "init.el" ''
    ;; -*- lexical-binding: t; -*-

    ;; Set up package archives
    (require 'package)
    (setq package-archives '(("gnu" . "https://elpa.gnu.org/packages/")
                             ("melpa" . "https://melpa.org/packages/")
                             ("nongnu" . "https://elpa.nongnu.org/nongnu/")))
    (package-initialize)

    ;; Bootstrap use-package
    (unless (package-installed-p 'use-package)
      (package-refresh-contents)
      (package-install 'use-package))

    ;; The magic line: Tangle and load your org-mode config
    (org-babel-load-file (expand-file-name "config.org" user-emacs-directory))
  '';
in
{
  # Install Emacs itself and essential dependencies
  home.packages = with pkgs; [
    emacs-pgtk # Or whatever emacs variant you prefer
    # Add dependencies for Emacs packages if needed (e.g., for tree-sitter)
    fd
    ripgrep
  ];

  # This is the core of the solution:
  # Declaratively place the necessary files in .emacs.d
  home.file = {
    # Place the bootstrapper init.el
    ".emacs.d/init.el" = {
      source = emacs-init-el;
    };

    # Symlink your config.org from your flake repo
    ".emacs.d/config.org" = {
      # This path is relative to home.nix. Adjust if you store it elsewhere.
      source = ./config.org;
    };
  };
}