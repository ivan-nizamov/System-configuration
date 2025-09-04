# default.nix - Emacs configuration for System-configuration
{ config, pkgs, ... }:

{
  # Configure the path for our modular configuration
  home.file = {
    # Create the directory for configuration modules
    ".emacs.d/config" = {
      source = ./config;
      recursive = true;
    };
  };
  
  # Create necessary directories
  home.activation = {
    createEmacsDirectories = ''
      $DRY_RUN_CMD mkdir -p $VERBOSE_ARG $HOME/.emacs.d/backups
      $DRY_RUN_CMD mkdir -p $VERBOSE_ARG $HOME/.emacs.d/auto-saves
    '';
  };
  
  # Configure external dependencies
  programs.emacs.extraConfig = ''
    ;; Set paths for org-related directories
    (setq org-directory "~/ORG")
    (setq org-roam-directory "~/ORG/Roam")
    
    ;; Configure YASnippet snippet directory
    (with-eval-after-load 'yasnippet
      (setq yas-snippet-dirs '("~/ORG/snippets")))
    
    ;; Configure ispell for compatibility
    (setq ispell-program-name "hunspell")
    (setq ispell-dictionary "en_US")
  '';
  
  # Install external tools needed by our configuration
  home.packages = with pkgs; [
    # For language tool
    languagetool
    
    # Icons
    emacs-all-the-icons-fonts
    
    # For markdown rendering
    pandoc
  ];
}