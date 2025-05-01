;;; -*- lexical-binding: t; -*-

(use-package vertico
  :custom
  (vertico-cycle t)
  :config
  (bind-keys :map vertico-map
             ("C-M-n" . vertico-next-group)
             ("C-M-p" . vertico-previous-group))
  (vertico-mode 1))

(use-package vertico-multiform :ensure nil
  :after vertico
  :custom
  (vertico-multiform-commands
   '((consult-ripgrep buffer)
     (consult-xref buffer)
     (embark-bindings buffer)))
  :config
  (vertico-multiform-mode 1))
