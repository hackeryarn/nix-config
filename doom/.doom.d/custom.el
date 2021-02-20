(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes
   '("30095d77cf3b19b736e472f56899df6e072c9c3c404ea1e16dbcffb1544eaf08" "0cb1b0ea66b145ad9b9e34c850ea8e842c4c4c83abe04e37455a1ef4cc5b8791" default))
 '(elfeed-feeds '("https://dailystoic.com/category/email-archives/rss"))
 '(neuron-default-tags '("stub") t)
 '(neuron-default-zettelkasten-directory "/home/artem/zettelkasten" t)
 '(neuron-tag-specific-title-faces '(("stub" neuron-stub-face)) t)
 '(safe-local-variable-values
   '((eval setq ansible-vault-password-file "~/.picard-vault-pass")
     (ansible-vault-password-file "~/.picard-vault-pass")
     (cider-shadow-default-options . "app")
     (cider-default-cljs-repl . shadow)
     (cider-shadow-cljs-default-options . "app")
     (projectile-globally-ignored-files "clj")
     (projectile-project-name . "bro/clj")
     (eval progn
           (defun shell-command-in-root-dir
               (command)
             (projectile-with-default-dir
                 (projectile-ensure-project
                  (projectile-project-root))
               (shell-command
                (concat command " & disown"))))
           (defun runserver nil
             (interactive)
             (shell-command-in-root-dir "./manage.py runserver"))
           (defun rumble-horizonone nil
             (interactive)
             (shell-command-in-root-dir "make rumble env=horizonone"))
           (defun rumble-wealthport nil
             (interactive)
             (shell-command-in-root-dir "make rumble env=wealthport"))
           (defun rumble-all nil
             (interactive)
             (shell-command-in-root-dir "make rumble env=horizonone")
             (shell-command-in-root-dir "./manage.py build_mock_data"))
           (defun rumble-uat nil
             (interactive)
             (shell-command-in-root-dir "make rumble-uat user=postgres db=bro-dev")))
     (eval progn
           (defalias 'runserver "./manage.py runserver")
           (defalias 'rumble-hone "make rumble env=horizonone")
           (defalias 'rumble-wp nil "make rumble env=wealthport")
           (defalias 'rumble-all nil "make rumble env=horizonone && ./manage.py build_mock_data")
           (defalias 'rumble-uat nil "make rumble-uat user=postgres db=bro-dev"))
     (eval progn
           (defun shell-command-in-root
               (command)
             (projectile-with-default-dir
                 (projectile-ensure-project
                  (projectile-project-root))
               (eshell-com hf command)))
           (defun runserver nil
             (eshell-command "./manage.py runserver"))
           (defun rumble-horizonone nil
             (interactive)
             (shell-command-in-root "make rumble env=horizonone"))
           (defun rumble-wealthport nil
             (interactive)
             (shell-command-in-root "make rumble env=wealthport"))
           (defun rumble-all nil
             (interactive)
             (shell-command-in-root "make rumble env=horizonone")
             (shell-command-in-root "./manage.py build_mock_data"))
           (defun rumble-uat nil
             (interactive)
             (shell-command-in-root "make rumble-uat user=postgres db=bro-dev")))
     (eval progn
           (defun shell-command-in-root
               (command)
             (projectile-with-default-dir
                 (projectile-ensure-project
                  (projectile-project-root))
               (eshell-command
                (concat command " & disown"))))
           (defun runserver nil
             (interactive)
             (shell-command-in-root))
           "./manage.py runserver"
           (defun rumble-horizonone nil
             (interactive)
             (shell-command-in-root "make rumble env=horizonone"))
           (defun rumble-wealthport nil
             (interactive)
             (shell-command-in-root "make rumble env=wealthport"))
           (defun rumble-all nil
             (interactive)
             (shell-command-in-root "make rumble env=horizonone")
             (shell-command-in-root "./manage.py build_mock_data"))
           (defun rumble-uat nil
             (interactive)
             (shell-command-in-root "make rumble-uat user=postgres db=bro-dev")))
     (eval progn
           (defun shell-command-in-root-dir
               (command)
             (projectile-with-default-dir
                 (projectile-ensure-project
                  (projectile-project-root))
               (eshell-command
                (concat command " & disown"))))
           (defun rumble-horizonone nil
             (interactive)
             (shell-command-in-root-dir "make rumble env=horizonone"))
           (defun rumble-wealthport nil
             (interactive)
             (shell-command-in-root-dir "make rumble env=wealthport"))
           (defun rumble-all nil
             (interactive)
             (shell-command-in-root-dir "make rumble env=horizonone")
             (shell-command-in-root-dir "./manage.py build_mock_data"))
           (defun rumble-uat nil
             (interactive)
             (shell-command-in-root-dir "make rumble-uat user=postgres db=bro-dev")))
     (rumble-hone . "make rumble env=horizonone")
     (projectile-ignored-directories "clj")
     (projectile-project-name . "bro-py")
     (projectile-project-name . "bro-clj")
     (eval progn
           (defun shell-command-in-root-dir
               (command)
             (projectile-with-default-dir
                 (projectile-ensure-project
                  (projectile-project-root))
               (shell-command command)))
           (defun rumble-horizonone nil
             (interactive)
             (shell-command-in-root-dir "make rumble env=horizonone"))
           (defun rumble-wealthport nil
             (interactive)
             (shell-command-in-root-dir "make rumble env=wealthport"))
           (defun rumble-all nil
             (interactive)
             (shell-command-in-root-dir "make rumble env=horizonone")
             (shell-command-in-root-dir "./manage.py build_mock_data"))
           (defun rumble-uat nil
             (interactive)
             (shell-command-in-root-dir "make rumble-uat user=postgres db=bro-dev")))
     (eval progn
           (pipenv-activate-projectile))
     (eval progn
           (defun rumble-horizonone nil
             (interactive)
             (shell-command "make rumble env=horizonone"))
           (defun rumble-wealthport nil
             (interactive)
             (shell-command "make rumble env=wealthport"))
           (defun rumble-all nil
             (interactive)
             (shell-command "make rumble env=horizonone")
             (shell-command "./manage.py build_mock_data"))
           (defun rumble-uat nil
             (interactive)
             (shell-command "make rumble-uat user=postgres db=bro-dev"))))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
