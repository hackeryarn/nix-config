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
(setq doom-font (font-spec :family "FiraCode Nerd Font" :size 18))

;; There are two ways to load a theme. Both assume the theme is installed and
;; available. You can either set `doom-theme' or manually load a theme with the
;; `load-theme' function. This is the default:
(setq doom-theme 'doom-solarized-light)

;; If you use `org' and don't want your org files in the default location below,
;; change `org-directory'. It must be set before org loads!
(setq org-directory "~/org/")

(setq ispell-dictionary "en")

(after! org
  (setq org-agenda-todo-ignore-scheduled 'future)
  (setq org-agenda-span 3)
  (setq org-agenda-custom-commands
        '(("n" "Daily tasks"
           ((agenda "" nil)
            (todo "STRT" nil)
            (todo "LOOP" nil)
            (todo "TODO" nil)
            (todo "WAIT" nil)
            (todo "HOLD" nil))
           nil)
          ("i" "IDEA"
           ((agenda "" nil)
            (todo "IDEA" nil)))))
  (push '("i" "Idea" entry
          (file+headline +org-capture-todo-file "Inbox")
          "* IDEA %?\n%i\n%a" :prepend t)
        org-capture-templates))


;; This determines the style of line numbers in effect. If set to `nil', line
;; numbers are disabled. For relative line numbers, set this to `relative'.
(setq display-line-numbers-type t)

;; Theme config
(after! modus-operandi-theme
  (setq modus-operandi-theme-override-colors-alist '(()))
  (setq modus-themes-slanted-constructs t)
  (setq modus-themes-bold-constructs nil)
  (setq modus-themes-paren-match 'intense))

;; Required for pytest
(setq comint-prompt-read-only nil)

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

(after! lsp-mode
  (setq +format-with-lsp nil))

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

(after! format-all
  (set-formatter! 'autopep8 "autopep8 -" :modes '(python-mode))
  (advice-add 'format-all-buffer :around #'envrc-propagate-environment))

(after! haskell
  (setq lsp-haskell-formatting-provider "brittany"))

(after! python
  (setq py-isort-options '("--profile=django"))
  (setq +python-ipython-repl-args '("-i" "--simple-prompt" "--no-color-info"))
  (setq +python-jupyter-repl-args '("--simple-prompt"))
  (setq-hook! 'python-mode-hook +format-with 'autopep8)
  (add-hook! 'python-mode-hook
    (add-hook 'before-save-hook 'py-isort-before-save)
    (add-hook 'before-save-hook '+format/buffer)))

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

(after! org
  (add-hook! 'org-mode-hook 'org-fragtog-mode))

(after! circe
  (set-irc-server! "irc.libera.chat"
    `(:tls t
      :port 6697
      :nick "hackeryarn"
      :sasl-username "hackeryarn"
      :sasl-password (lambda (&rest _) (+pass-get-secret "web.libera.chat"))
      :channels ("#emacs" "#scheme" "#guix"))))


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

(defun setup-bro ()
  (interactive)
  (+workspace-switch "bro" t)
  (find-file "~/horizon/bro/manage.py")
  (+eshell/toggle nil))

(defun setup-bro-clj ()
  (interactive)
  (+workspace-switch "clj" t)
  (find-file "~/horizon/bro/clj/project.clj")
  (cider-jack-in-cljs '(:cljs-repl-type bro)))
