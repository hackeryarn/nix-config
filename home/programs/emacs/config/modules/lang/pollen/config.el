;;; lang/pollen/config.el -*- lexical-binding: t; -*-

(use-package! pollen-mode
  :hook (pollen-mode . visual-line-mode)
  :config
  (map! :localleader
        :map pollen-mode-map
        "s" #'pollen-server-start
        "q" #'pollen-server-stop))
