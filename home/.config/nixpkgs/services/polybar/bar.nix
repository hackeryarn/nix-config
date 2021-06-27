{ font0 ? 16, font1 ? 18, font2 ? 40, font3 ? 28, font4 ? 7 }:

let
  bar = ''
[bar/main]
monitor = ''${env:MONITOR:eDP}
width = 100%
height = 48
radius = 6.0
fixed-center = true

background = ''${color.bg}
foregroud = ''${color.fg}

padding-left = 0
padding-right = 0
tray-padding = 3
tray-background = ''${color.bg}
cursor-click = pointer
cursor-scroll = ns-resize
overline-size = 2
overline-color = ''${color.ac}
border-bottom-size = 0
border-color = ''${color.ac}
  '';
  top = ''
[bar/top]
inherit = bar/main
tray-position = center
modules-left = right-end-top left-end-bottom right-end-top left-end-top
modules-right = left-end-top keyboard temperature clickable-date battery
enable-ipc = true
  '';

  bottom = ''
[bar/bottom]
inherit = bar/main
bottom = true
tray-position = none
modules-left = right-end-bottom mpris left-end-top cpu memory filesystem
modules-right = left-end-bottom wired-network wireless-network left-end-bottom powermenu
enable-ipc = true
  '';
in
bar + top + bottom
