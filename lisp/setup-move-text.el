;;; -*- lexical-binding: t; -*-

(use-package move-text
  :bind (("C-M-n" . move-text-down) ;; orig. `forward-list'
         ("C-M-p" . move-text-up)) ;; orig. `backward-list'
  :preface
  (defun indent-region-advice (&rest ignored)
    (let ((deactivate deactivate-mark))
      (if (region-active-p)
          (indent-region (region-beginning) (region-end))
        (indent-region (line-beginning-position) (line-end-position)))
      (setq deactivate-mark deactivate)))
  :config
  (move-text-default-bindings)

  (advice-add #'move-text-up :after 'indent-region-advice)
  (advice-add #'move-text-down :after 'indent-region-advice))
