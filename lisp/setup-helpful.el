;;; -*- lexical-binding: t; -*-

(use-package helpful
  :bind (("C-c C-d" . helpful-at-point)
         ([remap describe-command] . helpful-command)
         ([remap describe-function] . helpful-callable)
         ([remap describe-key] . helpful-key)
         ([remap describe-variable] . helpful-variable)
         ([remap describe-symbol] . helpful-symbol)))
