;;; -*- lexical-binding: t; -*-

(use-package modus-themes
  :custom
  (modus-themes-italic-constructs t)
  :config
  (defun my/modus-themes-custom-faces (&rest _)
    (modus-themes-with-colors
      (setq lsp-ui-doc-border border)
      (custom-set-faces
       `(lsp-ui-doc-background ((,c :background ,bg-dim))))))
  (add-hook 'modus-themes-post-load-hook #'my/modus-themes-custom-faces)

  (when IS-MAC (modus-themes-load-theme 'modus-operandi))
  (when IS-LINUX (modus-themes-load-theme 'modus-vivendi))

  (bind-key "<f5>" #'modus-themes-toggle))
