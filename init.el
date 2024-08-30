;;; -*- lexical-binding: t -*-

(require 'package)
(add-to-list 'package-archives
             '("melpa" . "https://melpa.org/packages/") t)

(setq custom-file (locate-user-emacs-file "custom.el"))
(when (file-exists-p custom-file)
  (load custom-file))

(require-theme 'modus-themes)

(defun my/modus-themes-custom-faces (&rest _)
  (modus-themes-with-colors
    (custom-set-faces
     `(acm-frame-border-face ((,c :background ,bg-active)))
     `(acm-frame-default-face ((,c :background ,bg-dim)))
     `(acm-frame-select-face ((,c :inherit bold :background ,bg-completion)))
     `(lsp-bridge-alive-mode-line ((,c :inherit italic :weight normal)))
     `(lsp-bridge-diagnostics-error-face ((,c :underline (:style wave :color ,underline-err))))
     `(lsp-bridge-diagnostics-hint-face ((,c :underline (:style wave :color ,underline-note))))
     `(lsp-bridge-diagnostics-info-face ((,c :underline (:style wave :color ,underline-note))))
     `(lsp-bridge-diagnostics-warning-face ((,c :underline (:style wave :color ,underline-warning))))
     `(lsp-bridge-kill-process ((,c :inherit bold :foreground ,err))))))

(add-hook 'modus-themes-post-load-hook #'my/modus-themes-custom-faces)

(modus-themes-load-theme 'modus-operandi-tinted)

;; Make C-m discernible from RET
(define-key input-decode-map [?\C-m] [C-m])

(bind-keys
 ("C-c o" . customize-option)
 ("C-<return>" . other-window)
 ("C-;" . comment-dwim)
 ("C-'" . jump-to-register)
 ("M-'" . point-to-register)
 ("<f5>" . modus-themes-toggle))

(defun my/toggle-display-line-numbers ()
  "Toggle display of relative line numbers in the buffer."
  (interactive)
  (setq display-line-numbers
        (unless display-line-numbers 'relative)))

(bind-keys :prefix-map toggle-map
           :prefix "C-c x"
           ("n" . my/toggle-display-line-numbers))

(use-package consult
  :ensure t
  :bind (("C-x b" . consult-buffer)
         ("C-x 4 b" . consult-buffer-other-window)
         ("C-x p b" . consult-project-buffer)
         ("M-s g" . consult-ripgrep)
         ("M-s l" . consult-line)
         ("C-M-'" . consult-register)))

(use-package embark
  :ensure t
  :bind (("C-." . embark-act)
         ("C-h b" . embark-bindings)))

(use-package embark-consult
  :ensure t)

(use-package exec-path-from-shell
  :if (memq window-system '(mac ns))
  :ensure t
  :config
  (exec-path-from-shell-initialize))

(use-package expand-region
  :ensure t
  :bind ("C-," . er/expand-region))

(use-package go-mode
  :ensure t
  :mode "\\.go\\'")

(use-package helpful
  :ensure t
  :bind (("C-h f" . helpful-callable)
         ("C-h v" . helpful-variable)
         ("C-h k" . helpful-key)
         ("C-h x" . helpfu-command)
         ("C-c C-d" . helpful-at-point)))

(use-package keycast
  :ensure t
  :bind (:map toggle-map
              ("k" . keycast-mode-line-mode)
              ("h" . keycast-header-line-mode)))

(use-package lsp-bridge
  :demand t
  :load-path "site-lisp/lsp-bridge"
  :bind* (:map lsp-bridge-mode-map
               ("M-." . lsp-bridge-find-def)
               ("M-," . lsp-bridge-find-def-return)
               ("M-?" . lsp-bridge-find-references)
               ("M-n" . lsp-bridge-diagnostic-jump-next)
               ("M-p" . lsp-bridge-diagnostic-jump-prev))
  :config
  (global-lsp-bridge-mode))

(use-package magit
  :ensure t)

(use-package marginalia
  :ensure t
  :config
  (marginalia-mode))

(use-package markdown-mode
  :ensure t
  :mode (("README\\.md\\'" . gfm-mode)
         ("\\.md\\'" . markdown-mode)
         ("\\.markdown\\'" . markdown-mode)))

(use-package multiple-cursors
  :ensure t
  :bind (("<C-m> e" . mc/edit-lines)
         ("<C-m> ^" . mc/edit-beginnings-of-lines)
         ("<C-m> $" . mc/edit-ends-of-lines)
         ("<C-m> x" . mc/mark-more-like-this-extended)
         ("<C-m> a" . mc/mark-all-like-this-dwim)))

(use-package orderless
  :ensure t)

(use-package vertico
  :ensure t
  :config
  (vertico-mode))

(use-package vterm
  :ensure t
  :bind (("C-c t" . vterm)
         ("C-c 4 t" . vterm-other-window)))

(use-package wgrep
  :ensure t)

(use-package which-key
  :config
  (which-key-mode))

(use-package yasnippet
  :ensure t
  :config
  (yas-global-mode))
