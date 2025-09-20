# ./tangle.nix
{ pkgs , ...}:

let
  # Choose a high‑quality, upstream Emacs for Wayland: PGTK variant.
  emacsPkg = pkgs.emacs-pgtk;

  # Build init.el from config.org inside an isolated Nix build.
  tangled-init-el = pkgs.runCommand "tangled-init.el" {
    # Only Emacs is needed to tangle. Keep TeX out of the build closure.
    buildInputs = [ emacsPkg ];
    src = ./config.org;
  } ''
    # Make the source available under a predictable name
    cp "$src" ./config.org

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

    # Publish the result as the derivation output (a single file path)
    mv init.el "$out"
  '';

in
{
  # Keep TeX-related packages in the user environment only (not in the tangle build)
  home.packages = with pkgs; [
    texliveMedium
    emacsPackages.gcmh
  ];

  # Ensure the interactive Emacs matches the batch Emacs used for tangling
  programs.emacs = {
    enable = true;
    package = emacsPkg;
  };

  # Link the tangled init.el into XDG config so every rebuild includes the tangle step
  xdg.configFile."emacs/init.el".source = tangled-init-el;
}
