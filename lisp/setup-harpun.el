;;; -*- lexical-binding: t; -*-

(use-package harpun
  :load-path "lisp/harpun"
  :bind ("C-'" . harpun-set-register-or-swap)
  :config
  (harpun-setup-default))
