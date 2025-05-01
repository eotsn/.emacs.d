;;; -*- lexical-binding: t; -*-

(use-package corfu
  :custom
  (completion-ignore-case t)
  (tab-always-indent 'complete)
  :config
  (bind-key "SPC" #'corfu-insert-separator corfu-map)
  (global-corfu-mode 1))

(use-package corfu-popupinfo :ensure nil
  :hook (corfu-mode . corfu-popupinfo-mode)
  :custom
  (corfu-popupinfo-delay '(nil . 0.2)) ;; hide the popup initially
  (corfu-popupinfo-max-height 20))
