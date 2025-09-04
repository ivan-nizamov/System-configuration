{ config, pkgs, ... }:

{
  services.mpd = {
    enable = true;
    musicDirectory = "~/Music";
    playlistDirectory = "~/.local/share/mpd/playlists";
    extraConfig = ''
      audio_output {
        type  "pulse"
        name  "PipeWire Output"
      }
    '';
  };

  programs.ncmpcpp.enable = true;
}