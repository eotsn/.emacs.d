;;; -*- lexical-binding: t; -*-

(use-package gptel
  :commands (gptel gptel-send gptel-rewrite)
  :bind (("C-c C-SPC" . gptel-menu)
         ("C-c <return>" . gptel-send)
         ("C-c r" . gptel-rewrite)
         ("C-c C-g" . gptel-abort)
         :map embark-region-map
         ("+" . gptel-add)
         :map embark-this-buffer-file-map
         ("+" . gptel-add))
  :hook (gptel-mode . visual-line-mode)
  :custom
  (gptel-default-mode 'org-mode)
  :config
  (require 'gptel-gh)

  (defvar gptel--copilot
    (gptel-make-gh-copilot "Copilot"))

  (setq-default gptel-model 'claude-3.5-sonnet
                gptel-backend gptel--copilot))

(use-package gptel-quick :ensure (:host github :repo "karthink/gptel-quick")
  :config
  (bind-key "?" 'gptel-quick embark-general-map))
