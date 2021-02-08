{
  # launch point: https://github.com/sulami/dotfiles/blob/master/nix/.nixpkgs/darwin.nix

  # system.keyboard = {
  #   enableKeyMapping = true;
  #   userKeyMapping = [
  #     {
  #       # Capslock -> Ctrl
  #       HIDKeyboardModifierMappingSrc = 30064771129;
  #       HIDKeyboardModifierMappingDst = 30064771296;
  #     }
  #     {
  #       # Ctrl -> Escape
  #       HIDKeyboardModifierMappingDst = 30064771113;
  #       HIDKeyboardModifierMappingSrc = 30064771129;
  #     }
  #   ];
  # };

  system.defaults.NSGlobalDomain = {
    # currently fairly sure I've set these elsewhere
    # AppleMeasurementUnits                = "Centimeters";
    # AppleMetricUnits                     = 1;
    # AppleTemperatureUnit                 = "Celsius";
    # AppleShowScrollBars                  = "Automatic";
    # InitialKeyRepeat                     = 12;
    # KeyRepeat                            = 2;
    # NSAutomaticCapitalizationEnabled     = false;
    # NSAutomaticPeriodSubstitutionEnabled = false;
  };

  system.defaults.dock = {
    autohide            = true;
    expose-group-by-app = false;
    mru-spaces          = false;
    tilesize            = 48;
  };

  # system.defaults.spaces.spans-displays = false;

  # system.defaults.trackpad = {
  #   Clicking                = false;
  #   TrackpadRightClick      = true;
  # };
}
