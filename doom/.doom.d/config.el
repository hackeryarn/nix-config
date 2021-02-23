;;; $DOOMDIR/config.el -*- lexical-binding: t; -*-

;; Place your private configuration here! Remember, you do not need to run 'doom
;; sync' after modifying this file!


;; Some functionality uses this to identify you, e.g. GPG configuration, email
;; clients, file templates and snippets.
(setq user-full-name "Artem Chernyak"
      user-mail-address "artemchernyak@gmail.com")

;; Doom exposes five (optional) variables for controlling fonts in Doom. Here
;; are the three important ones:
;;
;; + `doom-font'
;; + `doom-variable-pitch-font'
;; + `doom-big-font' -- used for `doom-big-font-mode'; use this for
;;   presentations or streaming.
;;
;; They all accept either a font-spec, font string ("Input Mono-12"), or xlfd
;; font string. You generally only need these two:
(setq doom-font (font-spec :family "Triplicate T4c" :size 18)
      doom-variable-pitch-font (font-spec :family "Charter" :size 16))

;; There are two ways to load a theme. Both assume the theme is installed and
;; available. You can either set `doom-theme' or manually load a theme with the
;; `load-theme' function. This is the default:
(setq doom-theme 'modus-operandi)

;; If you use `org' and don't want your org files in the default location below,
;; change `org-directory'. It must be set before org loads!
(setq org-directory "~/org/")

;; This determines the style of line numbers in effect. If set to `nil', line
;; numbers are disabled. For relative line numbers, set this to `relative'.
(setq display-line-numbers-type t)

;; Theme config
(setq modus-operandi-theme-bold-constructs t)
(setq modus-operandi-theme--constructs t)

;; Required for pytest
(setq comint-prompt-read-only nil)

(setq ispell-dictionary "en")

;; Here are some additional functions/macros that could help you configure Doom:
;;
;; - `load!' for loading external *.el files relative to this one
;; - `use-package!' for configuring packages
;; - `after!' for running code after a package has loaded
;; - `add-load-path!' for adding directories to the `load-path', relative to
;;   this file. Emacs searches the `load-path' when you load packages with
;;   `require' or `use-package'.
;; - `map!' for binding new keys
;;
;; To get information about any of these functions/macros, move the cursor over
;; the highlighted symbol at press 'K' (non-evil users must press 'C-c c k').
;; This will open documentation for it, including demos of how they are used.
;;
;; You can also try 'gd' (or 'C-c c d') to jump to their definition and see how
;; they are implemented.

(after! modus-operandi-theme
  (setq modus-operandi-theme-bold-constructs t))

(after! evil
  (setq evil-escape-key-sequence "fd")
  (setq evil-snipe-scope 'buffer)
  (evil-global-set-key 'insert (kbd "C-S-v") 'evil-paste-before)
  (evil-global-set-key 'normal (kbd "<SPC>)") 'sp-forward-slurp-sexp))

(after! cider
  (cider-register-cljs-repl-type
   'bro
   "(do (user/run)
        (user/browser-repl))"))

(after! flycheck
  (flycheck-remove-next-checker 'python-flake8 'python-pylint)
  (flycheck-remove-next-checker 'python-flake8 'python-mypy))

(after! python
  (setq-hook! 'python-mode-hook +format-with :none)
  (setq-hook! 'python-mode-hook +format-with-lsp t)
  (add-hook! 'python-mode-hook
    (add-hook 'before-save-hook '+format/buffer 0 t)
    (add-hook 'before-save-hook 'py-isort-before-save 0 t)))

(after! esh-mode
  (map! :map eshell-mode-map
        :i "C-c C-c" #'eshell-interrupt-process
        :i "<up>" #'eshell-previous-matching-input-from-input
        :i "<down>" #'eshell-next-matching-input-from-input))

(after! projectile
  (setq projectile-indexing-method 'hybrid))

(after! racket-mode
  (add-hook 'racket-mode-hook #'racket-xp-mode))

(after! deft
  (setq deft-directory "~/org")
  (setq deft-recursive t))

(setq inferior-lisp-program "ros -Q run")

(after! pollen-mode
  (add-hook 'pollen-mode-hook #'spell-fu-mode))

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes
   (quote
    ("0cb1b0ea66b145ad9b9e34c850ea8e842c4c4c83abe04e37455a1ef4cc5b8791" default))))


;; Functions for drn

(setq +drn/bin-path "~/common-lisp/daily-reading-notes/daily-reading-notes")

(defun +drn/new-post ()
  (interactive)
  (let ((dir (projectile-project-root)))
    (find-file (shell-command-to-string
                (format "%s new-post %s" +drn/bin-path dir)))
    (goto-char (point-min))
    (forward-line 5)))

(defun +drn/new-book ()
  (interactive)
  (let ((dir (projectile-project-root))
        (title (read-string "Enter book title: ")))
    (find-file (shell-command-to-string
                (format "%s new-book %s '%s'" +drn/bin-path dir title)))
    (goto-char (point-min))
    (forward-line 5)))
