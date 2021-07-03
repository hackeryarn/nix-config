{ pkgs, ... }:

let
  disableWebRtcIndicator = ''
    #webrtcIndicator {
      display: none;
    }
  '';

  shared-settings = {
    "browser.contantblocking.category" = "standard";

    "browser.search.region" = "US";
    "browser.search.widget.inNavBar" = true;

    "browser.shell.checkDefaultBrowser" = true;
    "browser.tabs.loadInBackground" = true;
    "browser.urlbar.placeholderName" = "DuckDuckGo";

    "distribution.searchplugins.defaultLocale" = "en-US";

    "dom.forms.autocomplete.formautofill" = false;

    "general.autoScroll" = true;
    "general.useragent.locale" = "en-US";

    "extensions.activeThemeID" = "{71864fba-a0ac-47f5-a514-e5f3378b9c12}";
    "extensions.webcompat.enable_picture_in_picture_overrides" = true;
    "extensions.webcompat.enable_shims" = true;
    "extensions.webcompat.perform_injections" = true;
    "extensions.webcompat.perform_ua_overrides" = true;

    "print.print_footerleft" = "";
    "print.print_footerright" = "";
    "print.print_headerleft" = "";
    "print.print_headerright" = "";

    "privacy.donottrackheader.enabled" = true;
  };
in {
  programs.firefox = {
    enable = true;

    extensions = with pkgs.nur.repos.rycee.firefox-addons; [
      bitwarden
      languagetool
      link-cleaner
      privacy-badger
      ublock-origin
      unpaywall
    ];
  };

}
