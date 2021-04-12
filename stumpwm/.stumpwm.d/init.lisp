(in-package :stumpwm)

;; Setup sly
(ql:quickload :slynk)
(slynk:create-server :dont-close t)

;;; Modules
(init-load-path #p"/home/artem/.stumpwm.d/modules/")

(ql:quickload "clx-truetype")
(load-module "ttf-fonts")
(set-font (make-instance 'xft:font
                         :family "Charter"
                         :subfamily "Regular"
                         :size 11))

(ql:quickload "xembed")
(load-module "stumptray")
(stumptray::stumptray)

(ql:quickload "py-configparser")
(ql:quickload "FiveAM")
(load-module "desktop-entry")
(desktop-entry:init-entry-list)
(define-key *root-map* (kbd "m") "show-desktop-menu")

(load-module "end-session")

(load-module "net")
(load-module "cpu")
(load-module "mem")
(load-module "battery-portable")

;;; Configs
(setf *window-format* "%m%n%s%c")

(setf *mode-line-timeout* 0.7)

(setf *screen-mode-line-format*
      (list "^2%d^]    ^7[|>^] %g    ^7[|>^] %W    "
            "^7[|>^] ^2%C    %l    %M    BAT: %B^]")) 

(setf *mode-line-position* :bottom)

(set-prefix-key (kbd "Menu"))

(setf *mouse-focus-policy* :sloppy)

;;; Startup commands
(mode-line)

(run-shell-command "redshift")
(run-shell-command "syncthing -no-browser")
