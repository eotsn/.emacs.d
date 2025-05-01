;;; -*- lexical-binding: t; -*-

;; Ensure we get the latest version instead of relying on transient
;; dependencies to avoid issues with loading `magit'.
(use-package transient)

(use-package magit
  :hook (magit-mode . hl-line-mode)
  :custom
  (magit-define-global-key-bindings 'recommended)
  (magit-diff-refine-hunk 'all)
  :config
  (defun my/magit-disable-whitespace-mode ()
    "Disable `whitespace-mode' in Magit buffers."
    (whitespace-mode -1))
  (add-hook 'magit-section-mode-hook #'my/magit-disable-whitespace-mode)

  (when IS-MAC
    ;; This is done for the sake of performance on macOS.
    (setq magit-git-executable "/usr/bin/git")))

(use-package forge
  :after magit
  :config
  ;; Hide issues from the `magit' status buffer.
  (remove-hook 'magit-status-sections-hook 'forge-insert-issues))
