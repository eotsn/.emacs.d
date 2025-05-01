;;; -*- lexical-binding: t; -*-

(use-package embark
  :bind (("C-." . embark-act)
         ("M-." . embark-dwim) ;; orig. `xref-find-definitions'
         ([remap describe-bindings] . embark-bindings))
  :config
  (setq prefix-help-command #'embark-prefix-help-command)

  ;; Embark actions for this buffer/file.
  (defun embark-target-this-buffer-file ()
    (cons 'this-buffer-file (buffer-name)))

  (add-to-list 'embark-target-finders #'embark-target-this-buffer-file 'append)

  (defvar-keymap embark-this-buffer-file-map
    :doc "Commands to act on the current file or buffer."
    :parent embark-general-map)

  (add-to-list 'embark-keymap-alist '(this-buffer-file . embark-this-buffer-file-map)))

(use-package embark-consult)
