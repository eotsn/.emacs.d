(require 'package)
(add-to-list 'package-archives
             '("melpa" . "https://melpa.org/packages/") t)

(setq custom-file (locate-user-emacs-file "custom.el"))
(when (file-exists-p custom-file)
  (load custom-file))

(when (eq system-type 'darwin)
  (setopt mac-option-modifier 'meta
          mac-right-option-modifier nil))

;; Make <C-m> distinguishable from RET.
(define-key input-decode-map [?\C-m] [C-m])

;; Powerful search and navigation commands with live previews for
;; minibuffer candidates. It also provides several enhanced variants
;; of built-in commands.
(use-package consult
  :bind (("C-x b"   . consult-buffer)
         ("C-x p b" . consult-project-buffer)
         ("M-s g"   . consult-ripgrep))

  ;; Enable automatic preview at point in the *Completions*
  ;; buffer. This is relevant when you use the default completion UI.
  :hook (completion-list-mode . consult-preview-at-point-mode)

  :init
  ;; Use Consult to select xref locations with preview.
  (setq xref-show-xrefs-function #'consult-xref
        xref-show-definitions-function #'consult-xref))

;; Show a "context menu" for the target around point (i.e. the cursor)
;; with contextual actions (commands) you can perform. For example,
;; open URLs, close buffers, collect and export candidates, etc.
(use-package embark
  :bind (("C-." . embark-act)
         ;; In its default configuration `embark-dwim' will find the
         ;; definition of the identifier at point, so it's safe to
         ;; override `xref-find-definitions'.
         ("M-." . embark-dwim)))

;; Integrate Embark and Consult.
(use-package embark-consult
  :hook (embark-collect-mode . consult-preview-at-point-mode))

;; An Emacs package for inserting, changing, and deleting surrounding
;; pairs of quotes, braces, etc.
(use-package embrace
  :bind ("M-'" . embrace-commander))

;; Expand (increase) the selected region by semantic units. Just keep
;; pressing the key until it selects what you want!
(use-package expand-region
  :bind (("C-," . er/expand-region)))

;; A better Emacs *Help* buffer.
(use-package helpful
  :bind (("C-h f"   . helpful-callable)
         ("C-h v"   . helpful-variable)
         ("C-h x"   . helpful-command))
         ("C-c C-d" . helpful-at-point))

;; The BEST text-based user interface to Git, period.
(use-package magit)

;; Add helpful annotations at the margin of the minibuffer for
;; completion candidates.
(use-package marginalia
  :config
  (marginalia-mode 1))

;; Highly accessible themes with excellent color contrast.
(use-package modus-themes
  :custom
  (modus-themes-bold-constructs t)
  :config
  (load-theme 'modus-operandi-tinted))

;; Place multiple cursors in the buffer to allow for parallel
;; editing. Very complex, but VERY powerful.
(use-package multiple-cursors
  :bind (("<C-m> e" . mc/edit-lines)
         ("<C-m> ^" . mc/edit-beginnings-of-lines)
         ("<C-m> $" . mc/edit-ends-of-lines)
         ("<C-m> x" . mc/mark-more-like-this-extended)
         ("<C-m> a" . mc/mark-all-like-this-dwim)))

;; Completion style which enables out-of-order pattern matching with
;; space-separated components. For example, the pattern "ins pac"
;; matches `package-menu-mark-install' as well as `package-install'.
(use-package orderless
  :custom
  (completion-styles '(orderless basic))
  (completion-category-defaults nil)
  (completion-category-overrides '((file (styles partial-completion)))))

;; Performant and minimalistic vertical completion UI for the
;; minibuffer, based on the default completion system.
(use-package vertico
  :custom
  (vertico-cycle t) ;; Enable cycling for `vertico-next/previous'
  :config
  (vertico-mode 1))

;; Fully-fledged terminal emulator based on libvterm, a C library. As
;; good as terminal emulation gets in Emacs.
(use-package vterm)

;; Edit grep buffers and apply those changes to the file buffer
;; interactively. Similar to sed and Vim's cdo commands.
(use-package wgrep
  :bind (:map grep-mode-map
         ("C-c C-c" . wgrep-finish-edit)))

;; Displays the key bindings following your currently entered
;; incomplete command (a prefix) in a popup.
(use-package which-key
  :config
  (which-key-mode 1))
