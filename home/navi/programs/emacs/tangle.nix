# ./tangle.nix
{ config, pkgs, ... }:

let
  # This is a Nix "derivation" that builds your init.el from config.org.
  # It runs inside the isolated Nix build environment.
  tangled-init-el = pkgs.runCommand "tangled-emacs-init.el" {
    # 1. List the tools needed for the build command. We need Emacs.
    buildInputs = [ pkgs.emacs ];

    # 2. Give it the source file. Nix will copy this file into the build environment.
    src = ./config.org;
  } ''
    # 3. This is the script that runs.
    # The source file is available at the path stored in the `$src` variable.
    # We copy it to the current directory with a predictable name.
    cp $src ./config.org

    # Run the emacs command to tangle the org file.
    # --batch runs Emacs without the UI.
    # The output will be 'init.el' in the current directory, because your
    # config.org specifies '#+property: header-args:emacs-lisp :tangle ./init.el'
    emacs --batch --load org --eval '(org-babel-tangle-file "config.org")'

    # The final output of a `runCommand` derivation must be moved to the `$out` path.
    mv init.el $out
  '';

in
{
  # Now, instead of sourcing a local file, we source the *result*
  # of the build command we defined above.
  xdg.configFile."emacs/init.el".source = tangled-init-el;
}