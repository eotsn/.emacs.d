;;; -*- lexical-binding: t; -*-

(use-package jinx
  :bind (("M-$" . jinx-correct)
         ("C-M-$" . jinx-languages))
  :hook ((org-mode markdown-mode) . jinx-mode))
