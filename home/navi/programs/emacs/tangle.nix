# ./tangle.nix
{ pkgs , ...}:

let
  # Choose a high‑quality, upstream Emacs for Wayland: PGTK variant.
  emacsPkg = pkgs.emacs-pgtk;

  # Read the content of config.org to make the derivation dependent on its content
  configOrgContent = builtins.readFile ./config.org;

  # Build init.el from config.org content (content-addressed)
  tangled-init-el = pkgs.runCommand "tangled-init.el" {
    # Only Emacs is needed to tangle. Keep TeX out of the build closure.
    buildInputs = [ emacsPkg ];
  } ''
    # Write the config.org content to a file
    cat > config.org <<'EOF_CONFIG_ORG'
${configOrgContent}
EOF_CONFIG_ORG

    # Tangle with explicit Org and ob-tangle loading and error handling
    ${emacsPkg}/bin/emacs --batch \
      --eval "(require 'org)" \
      --eval "(require 'ob-tangle)" \
      --eval "(condition-case err
                    (progn
                      (org-babel-tangle-file \"config.org\" \"init.el\")
                      (kill-emacs 0))
                  (error
                   (princ (format \"Tangling error: %s\" (error-message-string err)))
                   (kill-emacs 1)))"

    # Publish the result as the derivation output
    mv init.el "$out"
  '';

in
{
  # Keep TeX-related packages in the user environment only
  home.packages = with pkgs; [
    texliveMedium
    emacsPackages.gcmh
  ];

  # Ensure the interactive Emacs matches the batch Emacs used for tangling
  programs.emacs = {
    enable = true;
    package = emacsPkg;
  };

  # Link the tangled init.el into XDG config
  xdg.configFile."emacs/init.el".source = tangled-init-el;
}
