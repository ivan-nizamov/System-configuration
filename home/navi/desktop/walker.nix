{ config, pkgs, ... }:

{
  # Walker via its Home Manager module (module imported at integration level)
  programs.walker = {
    enable = true;
    # Run as a socket service for sub-100ms startup times
    runAsService = true;
    
    # Basic settings mirroring config.toml
    settings = {
      # Customize placeholder text
      placeholders.default.input = "Launch or search...";
      
      # Provider prefixes for quick access (e.g., + for web search, _ for provider list)
      providers.prefixes = [
        { provider = "applications"; prefix = ">"; }
        { provider = "websearch"; prefix = "+"; }
        { provider = "calculator"; prefix = "="; }
        { provider = "providerlist"; prefix = "_"; }
      ];
      
      # Quick keybinds (e.g., F1/F2/F3 to activate without Super+R)
      keybinds.quick_activate = [ "F1" "F2" "F3" ];
      
      # Simple theme override (Nord-inspired; full CSS in ~/.config/walker/theme.css)
      theme.style = ''
        * {
          color: #D8DEE9;
          background-color: #2E3440;
          font-family: "JetBrains Mono", monospace;
          font-size: 14px;
        }
        entry {
          border: 1px solid #4C566A;
          border-radius: 4px;
          padding: 8px;
        }
      '';
    };
  };

  # Optional: Add wl-clipboard for clipboard integration
  home.packages = with pkgs; [ wl-clipboard ];
}
