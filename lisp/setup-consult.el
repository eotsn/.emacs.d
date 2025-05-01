;;; -*- lexical-binding: t; -*-

(use-package consult
  :bind (("C-c h" . consult-history)
         ("C-c i" . consult-info)
         ("C-x b" . consult-buffer) ;; orig. `switch-to-buffer'
         ("C-x 4 b" . consult-buffer-other-window) ;; orig. `switch-to-buffer-other-window'
         ("C-x p b" . consult-project-buffer) ;; orig. `project-switch-to-buffer'
         ("M-g o" . consult-outline)
         ("M-g i" . consult-imenu)
         ("M-s g" . consult-ripgrep)
         ("M-s l" . consult-line))
  :config
  ;; Use Consult to select xref locations with preview.
  (setq xref-show-xrefs-function #'consult-xref
        xref-show-definitions-function #'consult-xref))
