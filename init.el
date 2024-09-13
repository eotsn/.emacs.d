;;; -*- lexical-binding: t -*-

(setq custom-file (make-temp-file "emacs-custom-"))

(require 'package)
(add-to-list 'package-archives
             '("melpa" . "https://melpa.org/packages/") t)

(add-to-list 'load-path
             (expand-file-name "site-lisp" user-emacs-directory))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; GUI
;;

(custom-set-variables
 '(inhibit-startup-screen t)
 '(column-number-mode t)
 '(scroll-bar-mode nil)
 '(tool-bar-mode nil))

(custom-set-faces
 '(default ((t (:family "PragmataPro Liga" :height 190)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; Theme customization
;;

(require-theme 'modus-themes)

(custom-set-variables
 '(modus-themes-bold-constructs t)
 '(modus-themes-to-toggle '(modus-operandi-tinted modus-vivendi-tinted)))

(bind-key "<f5>" #'modus-themes-toggle)

(defun my/modus-themes-custom-faces (&rest _)
  (modus-themes-with-colors
    (custom-set-faces
     ;; lsp-bridge
     `(acm-frame-border-face ((,c :background ,bg-active)))
     `(acm-frame-default-face ((,c :background ,bg-dim)))
     `(acm-frame-select-face ((,c :inherit bold :background ,bg-completion)))
     `(lsp-bridge-alive-mode-line ((,c :inherit italic :weight normal)))
     `(lsp-bridge-diagnostics-error-face ((,c :underline (:style wave :color ,underline-err))))
     `(lsp-bridge-diagnostics-hint-face ((,c :underline (:style wave :color ,underline-note))))
     `(lsp-bridge-diagnostics-info-face ((,c :underline (:style wave :color ,underline-note))))
     `(lsp-bridge-diagnostics-warning-face ((,c :underline (:style wave :color ,underline-warning))))
     `(lsp-bridge-kill-process ((,c :inherit bold :foreground ,err))))))

;; Ensure that faces are updated when switching between themes
(add-hook 'modus-themes-post-load-hook #'my/modus-themes-custom-faces)

(modus-themes-load-theme 'modus-operandi-tinted)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; Misc
;;

(custom-set-variables
 '(delete-selection-mode t)
 '(ediff-window-setup-function 'ediff-setup-windows-plain)
 '(electric-pair-mode t)
 '(indent-tabs-mode nil)
 '(native-comp-async-report-warnings-errors 'silent)
 '(project-vc-ignores '("node_modules/"))
 '(read-extended-command-predicate 'command-completion-default-include-p)
 '(recentf-mode t)
 '(save-place-mode t)
 '(savehist-mode t)
 '(tab-always-indent 'complete))

;; Enable some useful commands which are disabled by default
(dolist (cmd '(downcase-region
               narrow-to-region
               upcase-region))
  (put cmd 'disabled nil))

;; Make C-m discernible from RET
(define-key input-decode-map [?\C-m] [C-m])

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; Personal utilities and bindings
;;

(defun my/unfill-paragraph (&optional region)
  (interactive (progn (barf-if-buffer-read-only) '(t)))
  (let ((fill-column (point-max))
        (emacs-lisp-docstring-fill-column t))
    (fill-paragraph nil region)))

(defun my/toggle-display-line-numbers ()
  (interactive)
  (setq display-line-numbers
        (unless display-line-numbers 'relative)))

(bind-keys ("C-;" . comment-dwim)
           ("C-c M-q" . my/unfill-paragraph)
           ("C-x C-d" . duplicate-line)
           ("C-x C-e" . pp-eval-last-sexp)
           ("C-<return>" . other-window))

(bind-keys :prefix-map my-toggle-map
           :prefix "C-c x"
           ("h" . hl-line-mode)
           ("n" . my/toggle-display-line-numbers)
           ("w" . whitespacle-mode))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; Packages
;;

(use-package avy
  :ensure t
  :bind ("C-'" . avy-goto-char-2)
  :custom
  (avy-single-candidate-jump nil)
  :config
  (avy-setup-default))

(use-package consult
  :ensure t
  :bind (;; C-c bindings in `mode-specific-map'
         ("C-c i" . consult-info)
         ([remap Info-search] . consult-info)
         ;; C-x bindings in `ctl-x-map'
         ("C-x M-:" . consult-complex-command)
         ("C-x b" . consult-buffer)
         ("C-x 4 b" . consult-buffer-other-window)
         ("C-x 5 b" . consult-buffer-other-frame)
         ("C-x t b" . consult-buffer-other-tab)
         ("C-x r b" . consult-bookmark)
         ("C-x p b" . consult-project-buffer)
         ;; Custom M-# bindings for fast register access
         ("M-#" . consult-register-load)
         ("M-'" . consult-register-store)
         ("C-M-#" . consult-register)
         ;; Other custom bindings
         ("M-y" . consult-yank-pop)
         ;; M-g bindings in `goto-map'
         ("M-g e" . consult-compile-error)
         ("M-g f" . consult-flymake)
         ("M-g g" . consult-goto-line)
         ("M-g M-g" . consult-goto-line)
         ("M-g o" . consult-outline)
         ("M-g m" . consult-mark)
         ("M-g k" . consult-global-mark)
         ("M-g i" . consult-imenu)
         ("M-g I" . consult-imenu-multi)
         ;; M-s bindings in `search-map'
         ("M-s f" . consult-fd)
         ("M-s g" . consult-ripgrep)
         ("M-s l" . consult-line)
         ("M-s L" . consult-line-multi)
         ("M-s k" . consult-keep-lines)
         ("M-s u" . consult-focus-lines)
         ;; Isearch integration
         ("M-s e" . consult-isearch-history)
         :map isearch-mode-map
         ("M-e" . consult-isearch-history)
         ("M-s e" . consult-isearch-history)
         ("M-s l" . consult-line) ; Needed by consult-line to detect isearch
         ("M-s L" . consult-line-multi) ; Needed by consult-line to detect isearch
         ;; Minibuffer history
         :map minibuffer-local-map
         ("M-s" . consult-history)
         ("M-r" . consult-history))
  :custom
  (consult-narrow-key "<")
  (register-preview-function #'consult-register-format)
  (xref-show-definitions-function #'consult-xref)
  (xref-show-xrefs-function #'consult-xref)
  :config
  (advice-add #'register-preview :override #'consult-register-window))

(use-package diminish :ensure t :defer t)

(use-package embark
  :ensure t
  :bind (("C-." . embark-act)
         ("C-h b" . embark-bindings))
  :config
  (setq prefix-help-command #'embark-prefix-help-command))

(use-package embark-consult
  :ensure t)

(use-package exec-path-from-shell
  :if (memq window-system '(mac ns))
  :ensure t
  :config
  (exec-path-from-shell-initialize))

(use-package expand-region
  :ensure t
  :bind ("C-=" . er/expand-region))

(use-package forge
  :ensure t
  :after magit)

(use-package format-all
  :commands format-all-mode
  :hook (prog-mode . format-all-mode)
  :config
  (add-hook 'format-all-mode-hook #'format-all-ensure-formatter)
  (setq-default format-all-formatters
		'(("Lua" (stylua "-s")))))

(use-package git-link
  :ensure t
  :bind ("C-c Y" . git-link))

(use-package helpful
  :ensure t
  :bind (("C-c C-d" . helpful-at-point)
         ([remap describe-function] . helpful-callable)
         ([remap describe-key] . helpful-key)
         ([remap describe-variable] . helpful-variable)
         ([remap describe-command] . helpful-command)))

(use-package jinx
  :ensure t
  :bind (("M-$" . jinx-correct) ; orig. `ispell-word'
         ("C-M-$" . jinx-languages))
  :hook ((org-mode markdown-mode) . jinx-mode))

(use-package keycast
  :ensure t
  :bind ( :map my-toggle-map
          ("k" . keycast-mode-line-mode)
          ("h" . keycast-header-line-mode)))

(use-package lsp-bridge
  :demand t
  :load-path "site-lisp/lsp-bridge"
  :bind ( :map lsp-bridge-mode-map
          ("M-n" . lsp-bridge-diagnostic-jump-next)
          ("M-p" . lsp-bridge-diagnostic-jump-prev)
          ("s-l G d" . lsp-bridge-peek)
          ("s-l a" . lsp-bridge-code-action)
          ("s-l e" . lsp-bridge-diagnostic-list)
          ("s-l g d" . lsp-bridge-find-def)
          ("s-l g h" . lsp-bridge-incoming-call-hierarchy)
          ("s-l g i" . lsp-bridge-find-impl)
          ("s-l g r" . lsp-bridge-find-references)
          ("s-l g t" . lsp-bridge-find-type-def)
          ("s-l h" . lsp-bridge-popup-documentation)
          ("s-l r" . lsp-bridge-rename))
  :custom
  (acm-enable-icon nil)
  (lsp-bridge-enable-hover-diagnostic t)
  :config
  (add-to-list 'native-comp-jit-compilation-deny-list
               '("lsp-bridge"))
  ;; Free the RET key for less intrusive behavior
  (keymap-unset acm-mode-map "RET")
  (global-lsp-bridge-mode))

(use-package magit
  :ensure t
  :custom
  (magit-diff-refine-hunk t))

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
  :bind (("<C-m> $" . mc/edit-ends-of-lines)
         ("<C-m> ^" . mc/edit-beginnings-of-lines)
         ("<C-m> W" . mc/mark-all-words-like-this)
         ("<C-m> Y" . mc/mark-all-symbols-like-this)
         ("<C-m> a" . mc/mark-all-like-this-dwim)
         ("<C-m> n" . mc/mark-next-like-this)
         ("<C-m> p" . mc/mark-previous-like-this)
         ("<C-m> w" . mc/mark-next-like-this-word)
         ("<C-m> x" . mc/mark-more-like-this-extended)
         ("<C-m> y" . mc/mark-next-like-this-symbol)))

(use-package orderless
  :ensure t
  :custom
  (completion-styles '(orderless basic))
  (completion-category-overrides '((file (styles basic partial-completion)))))

(use-package org-mode
  :bind (("C-c a" . org-agenda)
         ("C-c c" . org-capture)
         ("C-c s" . org-store-link))
  :hook (org-mode . turn-on-auto-fill)
  :custom
  (org-agenda-files '("~/.todo.org"))
  (org-default-notes-file "~/.todo.org")
  (org-startup-indented t))

(use-package pragmatapro-lig
  :diminish
  :hook (prog-mode text-mode))

(use-package rainbow-mode
  :ensure t
  :bind ( :map my-toggle-map
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

(use-package vterm
  :ensure t
  :bind (("C-c t" . vterm)
         ("C-c 4 t" . vterm-other-window)))

(use-package vertico
  :ensure t
  :config
  (vertico-mode))

(use-package wgrep
  :ensure t
  :custom
  (wgrep-auto-save-buffer t))

(use-package yasnippet
  :ensure t
  :diminish yas-minor-mode
  :bind (("C-c y f" . yas-visit-snippet-file)
         ("C-c y i" . yas-insert-snippet)
         ("C-c y n" . yas-new-snippet)
         ("C-c y r" . yas-reload-all)
         :map yas-minor-mode-map
         ("C-<tab>" . yas-expand))
  :hook (prog-mode . yas-minor-mode))
