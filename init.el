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

(defun my/newline-above-and-indent ()
  "Insert a newline above the current line and move point to the beginning
of the line."
  (interactive)
  (previous-line)
  (end-of-line)
  (newline-and-indent))

(defun my/newline-below-and-indent ()
  "Insert a newline below the current line and move point to the beginning
of the line."
  (interactive)
  (end-of-line)
  (newline-and-indent))

(defun my/unfill-paragraph (&optional region)
  "Takes a multi-line paragraph and makes it into a single line of text."
  (interactive (progn (barf-if-buffer-read-only) '(t)))
  (let ((fill-column (point-max))
        ;; This would override `fill-column' if it's an integer.
        (emacs-lisp-docstring-fill-column t))
    (fill-paragraph nil region)))

;; Make C-m discernible from RET
(define-key input-decode-map [?\C-m] [C-m])

(bind-keys
 ("C-c o" . customize-option)
 ("C-<return>" . other-window)
 ("C-c ;" . comment-dwim)
 ("C-'" . jump-to-register)
 ("M-'" . point-to-register)
 ("M-o" . my/newline-below-and-indent)
 ("M-O" . my/newline-above-and-indent)
 ("M-Q" . my/unfill-paragraph)
 ("<f5>" . modus-themes-toggle))

(defun my/toggle-display-line-numbers ()
  "Toggle display of relative line numbers in the buffer."
  (interactive)
  (setq display-line-numbers
        (unless display-line-numbers 'relative)))

(bind-keys :prefix-map toggle-map
           :prefix "C-c x"
           ("n" . my/toggle-display-line-numbers)
           ("w" . whitespace-mode))

(use-package avy
  :ensure t
  :bind ("C-;" . avy-goto-char-2)
  :config
  (avy-setup-default))

(use-package consult
  :ensure t
  :bind (("C-x b" . consult-buffer)
         ("C-x 4 b" . consult-buffer-other-window)
         ("C-x p b" . consult-project-buffer)
         ("M-s g" . consult-ripgrep)
         ("M-s l" . consult-line)
         ("C-M-'" . consult-register)))

(use-package diminish :ensure t :defer t)

(use-package eldoc :diminish)

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
          ("M-g d" . lsp-bridge-find-def)
          ("M-g ," . lsp-bridge-find-def-return)
          ("M-g r" . lsp-bridge-find-references)
          ("M-g D" . lsp-bridge-peek)
          ("M-g e" . lsp-bridge-diagnostic-list)
          ("M-g h" . lsp-bridge-popup-documentation)
          ("M-n" . lsp-bridge-diagnostic-jump-next)
          ("M-p" . lsp-bridge-diagnostic-jump-prev))
  :config
  ;; Free the RET key for less intrusive behavior
  (keymap-unset acm-mode-map "RET")
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
         ("\\.md\\'" . markdown-mode)))

(use-package multiple-cursors
  :ensure t
  :bind (("<C-m> e" . mc/edit-lines)
         ("<C-m> ^" . mc/edit-beginnings-of-lines)
         ("<C-m> $" . mc/edit-ends-of-lines)
         ("<C-m> x" . mc/mark-more-like-this-extended)
         ("<C-m> a" . mc/mark-all-like-this-dwim)))

(use-package orderless
  :ensure t)

(use-package org-mode
  :bind (("C-c c" . org-capture)
         ("C-c a" . org-agenda)
         ("C-c s" . org-store-link)
         :map org-mode-map
         ("C-,") ; I use this for `er/expand-region'
         ("C-'")) ; I use this for `jump-to-register'
  :hook (org-mode . turn-on-auto-fill))

(use-package pragmatapro-lig
  :diminish
  :load-path "site-lisp"
  :hook (prog-mode text-mode))

(use-package rainbow-mode
  :ensure t
  :bind (:map toggle-map
         ("r" . rainbow-mode)))

(use-package treesit-auto
  :ensure t
  :custom
  (treesit-auto-install 'prompt)
  :config
  (setq treesit-auto-langs
        (cl-set-difference treesit-auto-langs
                           '(janet latex markdown)))
  (treesit-auto-add-to-auto-mode-alist 'all)
  (global-treesit-auto-mode))

(use-package vertico
  :ensure t
  :config
  (vertico-mode))

(use-package vterm
  :ensure t
  :bind (("C-c t" . vterm)
         ("C-c 4 t" . vterm-other-window)))

(use-package vundo
  :ensure t
  :bind ("C-c C-/" . vundo)
  :config
  (setq vundo-glyph-alist vundo-unicode-symbols))

(use-package wgrep
  :ensure t)

(use-package which-key
  :diminish
  :config
  (which-key-mode))

(use-package yasnippet
  :ensure t
  :diminish yas-minor-mode
  :bind (:map yas-minor-mode-map
         ("TAB") ; I use a dedicated key for snippet expansion
         ("C-<tab>" . yas-expand))
  :config
  (yas-global-mode))

;; This package must be loaded after `treesit-auto' to override the
;; entries inserted into `auto-mode-alist'. Because `:after' doesn't
;; seem to work we have to rely on ordering instead.
(use-package jtsx
  :ensure t
  :mode (("\\.tsx\\'" . jtsx-tsx-mode)
         ("\\.ts\\'" . jtsx-typescript-mode))
  :hook ((jtsx-tsx-mode . hs-minor-mode)
         (jtsx-typescript-mode . hs-minor-mode))
  :bind (:map jtsx-tsx-mode-map
         ("C-c C-j" . jtsx-jump-jsx-element-tag-dwim)
         ("C-c j o" . jtsx-jump-jsx-opening-tag)
         ("C-c j c" . jtsx-jump-jsx-closing-tag)
         ("C-c j r" . jtsx-rename-jsx-element)
         ("C-c <down>" . jtsx-move-jsx-element-tag-forward)
         ("C-c <up>" . jtsx-move-jsx-element-tag-backward)
         ("C-c C-<down>" . jtsx-move-jsx-element-forward)
         ("C-c C-<up>" . jtsx-move-jsx-element-backward)
         ("C-c C-S-<down>" . jtsx-move-jsx-element-step-in-forward)
         ("C-c C-S-<up>" . jtsx-move-jsx-element-step-in-backward)
         ("C-c j w" . jtsx-wrap-in-jsx-element)
         ("C-c j u" . jtsx-unwrap-jsx)
         ("C-c j d" . jtsx-delete-jsx-node)
         ("C-c j t" . jtsx-toggle-jsx-attributes-orientation)
         ("C-c j h" . jtsx-rearrange-jsx-attributes-horizontally)
         ("C-c j v" . jtsx-rearrange-jsx-attributes-vertically)))
