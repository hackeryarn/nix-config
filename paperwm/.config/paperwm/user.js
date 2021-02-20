// -*- mode: gnome-shell -*-

var Meta = imports.gi.Meta;
var Clutter = imports.gi.Clutter;
var St = imports.gi.St;
var Main = imports.ui.main;
var Shell = imports.gi.Shell;

// Extension local imports
var Extension, Me, Tiling, Utils, App, Keybindings, Examples;

function init() {
    // Runs _only_ once on startup

    // Initialize extension imports here to make gnome-shell-reload work
    Extension = imports.misc.extensionUtils.getCurrentExtension();
    Me = Extension.imports.user;
    Tiling = Extension.imports.tiling;
    Utils = Extension.imports.utils;
    Keybindings = Extension.imports.keybindings;
    Examples = Extension.imports.examples;
    App = Extension.imports.app;

    Keybindings.bindkey(
        "<Super>d",
        "my-favorite-width",
        (mw) => {
            let f = mw.get_frame_rect();
            let space = Tiling.spaces.spaceOfWindow(mw);
            let workarea = Main.layoutManager.getWorkAreaForMonitor(space.monitor.index);
            let margin = Tiling.prefs.horizontal_margin;
            let width = workarea.width / 3 - margin
            mw.move_resize_frame(true, f.x, f.y, width, f.h);
        },
        { activeInNavigator: true });
}

function enable() {
    // Runs on extension reloads, eg. when unlocking the session
}

function disable() {
    // Runs on extension reloads eg. when locking the session (`<super>L).
}
