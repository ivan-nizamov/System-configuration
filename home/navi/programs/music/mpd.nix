{ config, pkgs, ... }:

{
  services.mpd = {
    enable = true;
    musicDirectory = "/home/navi/Music";
    playlistDirectory = "/home/navi/.local/share/mpd/playlists";
    extraConfig = ''
      audio_output {
        type  "pulse"
        name  "PipeWire Output"
      }
    '';
  };

  programs.ncmpcpp.enable = true;
}