{ lib, host, ... }:

# Conditionally enable the Ollama LLM server when a GPU is present.
# You can set host.accel to "cuda" (for NVIDIA) or "rocm" (for AMD) in
# flake.nix to activate GPU acceleration.  When host.accel is "cpu"
# this module does nothing.
let gpu = host.accel;
in {
  config = lib.mkIf (gpu != "cpu") {
    services.ollama = {
      enable = true;
      # "cuda" or "rocm".  If you have a CPU‑only machine, this
      # attribute is ignored because the service is disabled via mkIf.
      acceleration = gpu;
    };
  };
}
