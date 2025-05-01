;;; -*- lexical-binding: t; -*-

(use-package jtsx
  :mode (("\\.jsx?\\'" . jtsx-jsx-mode)
         ("\\.tsx\\'" . jtsx-tsx-mode)
         ("\\.ts\\'" . jtsx-typescript-mode))
  :hook ((jtsx-jsx-mode . hs-minor-mode)
         (jtsx-tsx-mode . hs-minor-mode)
         (jtsx-typescript-mode . hs-minor-mode))
  :custom
  (jtsx-enable-jsx-element-tags-auto-sync t)
  :config
  (defvar jtsx-command-bindings
    '(("C-c C-j" . jtsx-jump-jsx-element-tag-dwim)
      ("C-c C-e n" . jtsx-move-jsx-element-forward)
      ("C-c C-e p" . jtsx-move-jsx-element-backward)
      ("C-c C-e C-n" . jtsx-move-jsx-element-step-in-forward)
      ("C-c C-e C-p" . jtsx-move-jsx-element-step-in-backward)
      ("C-c C-e w" . jtsx-wrap-in-jsx-element)
      ("C-c C-e u" . jtsx-unwrap-jsx)
      ("C-c C-e k" . jtsx-delete-jsx-node)
      ("C-c C-a k" . jtsx-delete-jsx-attribute)
      ("C-c C-a /" . jtsx-toggle-jsx-attributes-orientation)))

  (defun my/jtsx-bind-keys-to-mode-map (mode-map)
    (dolist (binding jtsx-command-bindings)
      (define-key mode-map (kbd (car binding)) (cdr binding))))

  (add-hook 'jtsx-jsx-mode-hook (lambda () (my/jtsx-bind-keys-to-mode-map jtsx-jsx-mode-map)))
  (add-hook 'jtsx-tsx-mode-hook (lambda () (my/jtsx-bind-keys-to-mode-map jtsx-tsx-mode-map))))
