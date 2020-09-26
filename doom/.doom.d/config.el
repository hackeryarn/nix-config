;;; $DOOMDIR/config.el -*- lexical-binding: t; -*-

;; Place your private configuration here! Remember, you do not need to run 'doom
;; refresh' after modifying this file!


;; These are used for a number of things, particularly for GPG configuration,
;; some email clients, file templates and snippets.
(setq user-full-name "Artem Chernyak"
      user-mail-address "artemchernyak@gmail.com")

;; Doom exposes five (optional) variables for controlling fonts in Doom. Here
;; are the three important ones:
;;
;; + `doom-font'
;; + `doom-variable-pitch-font'
;; + `doom-big-font' -- used for `doom-big-font-mode'
;;
;; They all accept either a font-spec, font string ("Input Mono-12"), or xlfd
;; font string. You generally only need these two:
(setq doom-font (font-spec :family "Triplicate T4c" :size 18))
; (setq doom-variable-pitch-font (font-spec :family "triplicate t4c"))
; (setq doom-font (font-spec :family "triplicate t4c" :size 20))

;; There are two ways to load a theme. Both assume the theme is installed and
;; available. You can either set `doom-theme' or manually load a theme with the
;; `load-theme' function. These are the defaults.
(if (string-equal "hackeryarn" (user-login-name))
    (setq doom-theme 'doom-solarized-dark)
  (setq doom-theme 'doom-solarized-light))

;; If you intend to use org, it is recommended you change this!
(setq org-directory "~/Sync/org")

;; If you want to change the style of line numbers, change this to `relative' or
;; `nil' to disable it:
(setq display-line-numbers-type t)


;; Here are some additional functions/macros that could help you configure Doom:
;;
;; - `load!' for loading external *.el files relative to this one
;; - `use-package' for configuring packages
;; - `after!' for running code after a package has loaded
;; - `add-load-path!' for adding directories to the `load-path', where Emacs
;;   looks when you load packages with `require' or `use-package'.
;; - `map!' for binding new keys
;;
;; To get information about any of these functions/macros, move the cursor over
;; the highlighted symbol at press 'K' (non-evil users must press 'C-c g k').
;; This will open documentation for it, including demos of how they are used.
;;
;; You can also try 'gd' (or 'C-c g d') to jump to their definition and see how
;; they are implemented.

(setq doom-localleader-key ",")

(after! evil
  (setq evil-escape-key-sequence "fd")
  (define-key! evil-normal-state-map "/" #'swiper))

(after! cider
  (cider-register-cljs-repl-type
   'bro
   "(do (user/run)
        (user/browser-repl))"))

(after! parinfer
  (add-hook! (racket-mode hy-mode) #'parinfer-mode))

(setq flycheck-python-flake8-executable "flake8")

(add-hook! text-mode #'auto-fill-mode)
