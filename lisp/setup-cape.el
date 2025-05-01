;;; -*- lexical-binding: t; -*-

(use-package cape
  :bind ("C-c ." . cape-prefix-map)
  :init
  ;; The order here is important, as the first function returning a
  ;; result wins.  Note that the list of buffer-local completion
  ;; functions takes precedence over the global list, for example when
  ;; using `lsp-mode'.
  (add-hook 'completion-at-point-functions #'cape-dabbrev)
  (add-hook 'completion-at-point-functions #'cape-file)
  (add-hook 'completion-at-point-functions #'cape-abbrev))
