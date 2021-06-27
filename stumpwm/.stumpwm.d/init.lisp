(in-package :stumpwm)

;; Setup sly
(ql:quickload :slynk)
(slynk:create-server :dont-close t)

;;; Setup path
(init-load-path #p"/home/artem/.stumpwm.d/modules/")

;;; Fonts
(ql:quickload "clx-truetype")
(load-module "ttf-fonts")
(xft:cache-fonts)
(set-font (list (make-instance 'xft:font
                               :family "Charter"
                               :subfamily "Regular"
                               :size 11)
                (make-instance 'xft:font
                               :family "Triplicate T4c"
                               :subfamily "Regular"
                               :size 14)
                (make-instance 'xft:font
                               :family "FontAwesome"
                               :subfamily "Regular"
                               :size 14)
                (make-instance 'xft:font
                               :family "Triplicate T4c"
                               :subfamily "Bold"
                               :size 14)))

;;; Modeline
(load-module "net")
(load-module "cpu")
(load-module "mem")
(load-module "battery-portable")

(setf *window-format* "%m%n%s%c")
(setf *colors* (append *colors*
                       (list "#d7d7d7")))
(setf (nth 1 *colors*) "#b60000")
(setf (nth 2 *colors*) "#006800")
(setf (nth 3 *colors*) "#605b00")
(setf (nth 4 *colors*) "#1f1fce")
(setf (nth 5 *colors*) "#a8007f")
(setf (nth 6 *colors*) "#005f88")
(update-color-map (current-screen))
(setf *mode-line-position* :bottom)
(setf *mode-line-timeout* 0.7)
(setf *mode-line-background-color* "#efefef")
(setf *mode-line-foreground-color* "#000000")
(setf *mode-line-highlight-template* "^8^f3^R~A^r^0^f1")
*mode-line-highlight-template*

(setf *screen-mode-line-format*
      (list "^f1%g   ^5^f2^]^f1   %W   ^5^f2^]^f1   %d   "
            (format nil "^4^f2~a^0^f1%C  ^4^f2~a^0^f1%M  ^4^f2~a^0^f1 %B   ^4^f2~a^0^f1 %l^]"
                    #\Uf0a0
                    #\Uf1c0
                    #\Uf241
                    #\Uf233)))

(mode-line)

(set-bg-color "#efefef")
(set-fg-color "#000000")

;;; Modules
(ql:quickload "xembed")
(load-module "stumptray")
(stumptray::stumptray)

(defcommand rofi () ()
  (run-shell-command "rofi -modi drun,ssh,window -show drun -show-icons"))

(define-key *root-map* (kbd "m") "rofi")
 
(load-module "end-session")

;;; Configs
(set-prefix-key (kbd "Menu"))
(setf *mouse-focus-policy* :sloppy)

;;; Startup commands

(run-shell-command "redshift")
(run-shell-command "syncthing -no-browser")
(run-shell-command "nextcloud")

;;; Custom functions
(defcommand fresh () ()
  (refresh-heads)
  (mode-line)
  (stumptray::stumptray))
  
(defcommand setup-main () ()
  (clear-window-placement-rules)
  (grename "main")
  (vsplit-equally 3)
  (define-frame-preference "main"
    (0 t nil :instance "brave")
    (1 t nil :instance "emacs")
    (1 t nil :instance "alacritty"))
  (run-shell-command "konsole")
  (run-shell-command "emacs")
  (run-shell-command "brave --new-window 'http://localhost:3449/#/'")
  (run-shell-command "brave --new-window 'code.hzi.io/horizonweb'"))
  
(defcommand setup-comm () ()
  (gnew "comm")
  (vsplit-equally 3)
  (define-frame-preference "comm"
    (3 t nil :instance "emacs")
    (1 t nil :instance "slack")
    (0 t nil :instance "brave"))
  (run-shell-command "emacs")
  (run-shell-command "slack")
  (run-shell-command "brave --new-window 'mail.google.com'"))

(defcommand setup-learn () ()
  (gnew "learn")
  (vsplit-equally 3)
  (define-frame-preference "learn"
    (3 t nil :instance "okular")
    (0 t nil :instance "brave")
    (1 t nil :instance "emacs"))
  (run-shell-command "okular ~/Sync/books/haskell/book-screen.pdf")
  (run-shell-command "emacs")
  (run-shell-command "brave --new-window 'github.com'"))
