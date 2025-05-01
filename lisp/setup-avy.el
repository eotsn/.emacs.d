;;; -*- lexical-binding: t; -*-

(use-package avy
  :bind (("C-;" . avy-goto-char-2)
         :map isearch-mode-map
         ("C-;" . avy-isearch))
  :config
  (avy-setup-default))
