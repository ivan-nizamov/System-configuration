# configuration.nix (or a NixOS module)
{ config, pkgs, ... }:
{
  services.syncthing = {
    enable = true;
    # run as this user; data lives in their home by default
    user = "navi";
    group = "users";

    # Open 22000/TCP+UDP (sync) and 21027/UDP (discovery) for you
    openDefaultPorts = true;

    # Where Syncthing stores its config/state
    configDir = "/home/navi/.config/syncthing";

    # Declarative config: Nix → syncthing.xml on activation
    settings = {
      gui = {
        # change to 127.0.0.1:8384 on a server and tunnel with SSH
        address = "0.0.0.0:8384";
        # Set user/pass once in the GUI; the hash will be written back into state.
        # If you want it fully declarative, you can copy that hashed password here later.
      };

      options = {
        urAccepted = -1;              # hide usage-report nag
        relaysEnabled = true;
        natEnabled = true;            # NAT-PMP/UPnP if your router allows it
        autoUpgradeIntervalH = 0;     # NixOS manages versions; keep Syncthing updater off
      };

      # Your devices (IDs are NOT secret—paste from each peer’s GUI)
      devices = {
        "phone" = { id = "2WGUA6E-OJLUMYK-7NJLC4C-42JON6A-FOLZZQX-VT4JEGC-AA3CGJ7-BPNFZQX"; addresses = [ "dynamic" ]; };
        "desktop-windows" = {
          id = "M6Z45DS-BSYJV34-Y4RVGL5-7LTLBIU-KL35BCR-E5YTV6C-2KXIHEE-IL2I4AH";
          addresses = [ "dynamic" ];
        };
      };

      # Folders: names are arbitrary keys; path is local
      folders = {
        "Common\ ground" = {
          path = "/home/navi/Common\ ground/";
          devices = [ "phone" "desktop-windows" ];
          type = "sendreceive";      # or "receiveonly"
          fsWatcherEnabled = true;   # inotify; instantish sync
          ignorePerms = true;        # cross-OS friendliness
          versioning = {
            type = "staggered";      # QoL: accidental deletes/edits recovery
            params = { cleanInterval = "3600"; maxAge = "2592000"; }; # 30 days
          };
        };
        "org" = {
          path = "/home/navi/ORG/";
          devices = [ "phone" ];
          type = "sendreceive";      # or "receiveonly"
          fsWatcherEnabled = true;   # inotify; instantish sync
          ignorePerms = true;        # cross-OS friendliness
          versioning = {
            type = "staggered";      # QoL: accidental deletes/edits recovery
            params = { cleanInterval = "3600"; maxAge = "2592000"; }; # 30 days
          };
        };

      };
    };

    # If true, Nix will OVERWRITE devices/folders each switch.
    # Great for fleets; if you like clicking in the GUI, leave them false.
    overrideDevices = true;
    overrideFolders = true;
  };
}
