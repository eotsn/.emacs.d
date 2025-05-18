;;; init.el -*- lexical-binding: t; -*-

(eval-when-compile (require 'cl-lib))
(let ((emacs-start-time (current-time)))
  (add-hook 'emacs-startup-hook
            (lambda ()
              (let ((elapsed (float-time (time-subtract (current-time) emacs-start-time))))
                (message "[Emacs initialized in %.3fs]" elapsed)))))

;; ============================================================================
;; Elpaca - https://github.com/progfolio/elpaca
;; ============================================================================

(defvar elpaca-installer-version 0.11)
(defvar elpaca-directory (expand-file-name "elpaca/" user-emacs-directory))
(defvar elpaca-builds-directory (expand-file-name "builds/" elpaca-directory))
(defvar elpaca-repos-directory (expand-file-name "repos/" elpaca-directory))
(defvar elpaca-order '(elpaca :repo "https://github.com/progfolio/elpaca.git"
                              :ref nil :depth 1 :inherit ignore
                              :files (:defaults "elpaca-test.el" (:exclude "extensions"))
                              :build (:not elpaca--activate-package)))
(let* ((repo  (expand-file-name "elpaca/" elpaca-repos-directory))
       (build (expand-file-name "elpaca/" elpaca-builds-directory))
       (order (cdr elpaca-order))
       (default-directory repo))
  (add-to-list 'load-path (if (file-exists-p build) build repo))
  (unless (file-exists-p repo)
    (make-directory repo t)
    (when (<= emacs-major-version 28) (require 'subr-x))
    (condition-case-unless-debug err
        (if-let* ((buffer (pop-to-buffer-same-window "*elpaca-bootstrap*"))
                  ((zerop (apply #'call-process `("git" nil ,buffer t "clone"
                                                  ,@(when-let* ((depth (plist-get order :depth)))
                                                      (list (format "--depth=%d" depth) "--no-single-branch"))
                                                  ,(plist-get order :repo) ,repo))))
                  ((zerop (call-process "git" nil buffer t "checkout"
                                        (or (plist-get order :ref) "--"))))
                  (emacs (concat invocation-directory invocation-name))
                  ((zerop (call-process emacs nil buffer nil "-Q" "-L" "." "--batch"
                                        "--eval" "(byte-recompile-directory \".\" 0 'force)")))
                  ((require 'elpaca))
                  ((elpaca-generate-autoloads "elpaca" repo)))
            (progn (message "%s" (buffer-string)) (kill-buffer buffer))
          (error "%s" (with-current-buffer buffer (buffer-string))))
      ((error) (warn "%s" err) (delete-directory repo 'recursive))))
  (unless (require 'elpaca-autoloads nil t)
    (require 'elpaca)
    (elpaca-generate-autoloads "elpaca" repo)
    (let ((load-source-file-function nil)) (load "./elpaca-autoloads"))))
(add-hook 'after-init-hook #'elpaca-process-queues)
(elpaca `(,@elpaca-order))

;; Install use-package support.
(elpaca elpaca-use-package
  ;; Enable use-package :ensure support for Elpaca.
  (elpaca-use-package-mode)
  (setq use-package-always-ensure t))

;; ============================================================================
;; Custom functions, bindings, etc.
;; ============================================================================

(defun my/load-setup-files ()
  "Ad hoc package loader which automatically loads Elisp files in the
user's 'lisp' directory, whose names begins with 'setup-'."
  (let ((lisp-dir (expand-file-name "lisp/" user-emacs-directory)))
    (dolist (file (directory-files lisp-dir t "setup-"))
      (load file))))
(my/load-setup-files)

(defun my/start-new-line ()
  "Start a new line below, or above if called with a prefix argument, the
current line with the correct indentation."
  (interactive)
  (when current-prefix-arg
    (previous-line))
  (end-of-line)
  (comment-indent-new-line))

(bind-keys* ("<f6>" . whitespace-mode)
            ("<f7>" . display-line-numbers-mode)
            ("<f8>" . hl-line-mode)
            ("C-<return>" . my/start-new-line)
            ("C-x C-d" . duplicate-line) ;; orig. `list-directory'
            ("C-c z" . delete-trailing-whitespace))

;; ============================================================================
;; Core - https://www.gnu.org/software/emacs/manual/
;; ============================================================================

(use-package emacs :ensure nil
  :custom
  ;; Open the *scratch* buffer when Emacs starts.
  (inhibit-startup-screen t)
  (initial-buffer-choice t)

  ;; Show column numbers in the mode line.
  (column-number-mode 1)

  ;; Show line numbers in all buffers.
  (global-display-line-numbers-mode 1)

  ;; Show the current file's full path in the title.
  (frame-title-format
   '(:eval (if buffer-file-name default-directory "%b")))

  ;; Write customizations to a temporary file.
  (custom-file (make-temp-file "emacs-custom-"))

  ;; Replace active selection when typing.  Makes non-modal editing
  ;; bearable, in conjunction with `er/expand-region'.
  (delete-selection-mode 1)

  ;; Automatically detect and reload files when they change on disk.
  (auto-revert-verbose t)
  (global-auto-revert-mode 1)

  ;; Hide commands in M-x which do not work in the current mode.
  (read-extended-command-predicate #'command-completion-default-include-p)

  ;; Save minibuffer history.
  (savehist-mode 1)

  ;; Enable numbered backup files and store them in a central
  ;; location.
  (version-control t)
  (delete-old-versions t)
  (backup-directory-alist `(("." . ,(expand-file-name
                                     (concat user-emacs-directory "backups")))))

  ;; Use SPC instead of TAB for indentation.
  (indent-tabs-mode nil)

  ;; The default behavior in Emacs is that TAB just indents the
  ;; current line.  We want TAB to both indent and complete the thing
  ;; at point.
  (tab-always-indent 'complete)
  (tab-width 2)

  ;; Display the key bindings for incomplete commands in a popup.
  (which-key-mode 1)

  :config
  ;; Highlight trailing whitespace for programming modes.
  (add-hook 'prog-mode-hook (lambda ()
                              (setq show-trailing-whitespace t)))

  ;; Enable some useful commands which are disabled by default.
  (dolist (cmd '(dired-find-alternate-file
                 downcase-region
                 narrow-to-region
                 upcase-region))
    (put cmd 'disabled nil)))

(use-package dired :ensure nil
  :custom
  (dired-kill-when-opening-new-dired-buffer t)
  (dired-listing-switches "-alh")
  (dired-vc-rename-file t)
  :config
  (bind-key "-" #'dired-up-directory dired-mode-map))

(use-package ediff :ensure nil
  :custom
  (ediff-diff-options "-w")
  (ediff-window-setup-function #'ediff-setup-windows-plain))

;; ============================================================================
;; Tree-sitter - https://tree-sitter.github.io/tree-sitter/
;; ============================================================================

(setq treesit-language-source-alist
      '((css "https://github.com/tree-sitter/tree-sitter-css")
        (javascript "https://github.com/tree-sitter/tree-sitter-javascript")
        (html "https://github.com/tree-sitter/tree-sitter-html")
        (tsx "https://github.com/tree-sitter/tree-sitter-typescript" "master" "tsx/src")
        (typescript "https://github.com/tree-sitter/tree-sitter-typescript" "master" "typescript/src")))

(defun my/treesit-install-language-grammars ()
  "Downloads and installs all available language grammars from
`treesit-language-source-alist'."
  (interactive)
  (dolist (lang treesit-language-source-alist)
    (unless (treesit-language-available-p (car lang))
      (treesit-install-language-grammar (car lang)))))

(setq major-mode-remap-alist
      '((css-mode . css-ts-mode)
        (js-mode . js-ts-mode)
        (html-mode . html-ts-mode)))

;; ============================================================================
;; LSP - https://github.com/emacs-lsp/lsp-mode
;; ============================================================================

(defun lsp-booster--advice-json-parse (old-fn &rest args)
  "Try to parse bytecode instead of json."
  (or
   (when (equal (following-char) ?#)
     (let ((bytecode (read (current-buffer))))
       (when (byte-code-function-p bytecode)
         (funcall bytecode))))
   (apply old-fn args)))
(advice-add (if (progn (require 'json)
                       (fboundp 'json-parse-buffer))
                'json-parse-buffer
              'json-read)
            :around
            #'lsp-booster--advice-json-parse)

(defun lsp-booster--advice-final-command (old-fn cmd &optional test?)
  "Prepend emacs-lsp-booster command to lsp CMD."
  (let ((orig-result (funcall old-fn cmd test?)))
    (if (and (not test?)                             ;; for check lsp-server-present?
             (not (file-remote-p default-directory)) ;; see lsp-resolve-final-command, it would add extra shell wrapper
             lsp-use-plists
             (not (functionp 'json-rpc-connection))  ;; native json-rpc
             (executable-find "emacs-lsp-booster"))
        (progn
          (when-let ((command-from-exec-path (executable-find (car orig-result))))  ;; resolve command from exec-path (in case not found in $PATH)
            (setcar orig-result command-from-exec-path))
          (message "Using emacs-lsp-booster for %s!" orig-result)
          (cons "emacs-lsp-booster" orig-result))
      orig-result)))
(advice-add 'lsp-resolve-final-command :around #'lsp-booster--advice-final-command)

(use-package lsp-mode
  :bind ("C-c C-l" . lsp-mode)
  :custom
  (lsp-completion-provider :none) ;; we use Corfu!
  (lsp-headerline-breadcrumb-enable nil)
  (lsp-keymap-prefix "C-c l")
  (lsp-modeline-code-action-fallback-icon "")
  (lsp-progress-prefix "")
  :config
  (setq read-process-output-max (* 1024 1024)) ;; 1 MB

  (defun my/lsp-help-mode-face-remap ()
    (face-remap-add-relative 'markdown-code-face 'default))
  (add-hook 'lsp-help-mode-hook #'my/lsp-help-mode-face-remap)

  (defun my/lsp-mode-setup-completion ()
    (setf (alist-get 'styles (alist-get 'lsp-capf completion-category-defaults))
          '(orderless))
    ;; The Capf-Buster ensures that you always get a fresh set of candidates!
    (setq-local completion-at-point-functions (list (cape-capf-buster #'lsp-completion-at-point))))
  (add-hook 'lsp-completion-mode-hook #'my/lsp-mode-setup-completion))

(use-package lsp-ui
  :custom
  (lsp-ui-doc-position 'at-point))

(use-package flycheck
  :bind (("M-n" . flycheck-next-error)
         ("M-p" . flycheck-previous-error))
  :config
  (add-hook 'after-init-hook #'global-flycheck-mode))

;; ============================================================================
;; VCS - https://github.com/magit/magit
;; ============================================================================

;; Ensure we get the latest version instead of relying on transient
;; dependencies to avoid issues with loading `magit'.
(use-package transient :defer t)

(use-package magit
  :hook (magit-mode . hl-line-mode)
  :custom
  (magit-diff-refine-hunk 'all)
  :config
  (transient-bind-q-to-quit)
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

;; ============================================================================
;; Programming languages, tools, etc.
;; ============================================================================

(use-package dockerfile-mode
  :mode "\\Dockerfile\\'")

(use-package go-mode
  :mode "\\.go\\'")

(use-package jtsx
  :mode (("\\.[mc]?jsx?\\'" . jtsx-jsx-mode)
         ("\\.[mc]?ts\\'" . jtsx-typescript-mode)
         ("\\.tsx\\'" . jtsx-tsx-mode))
  :custom
  (jtsx-enable-electric-open-newline-between-jsx-element-tags t)
  (jtsx-enable-jsx-electric-closing-element t)
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
  (add-hook 'jtsx-tsx-mode-hook (lambda () (my/jtsx-bind-keys-to-mode-map jtsx-tsx-mode-map)))

  (add-to-list 'lsp--formatting-indent-alist '(jtsx-jsx-mode . js-indent-level))
  (add-to-list 'lsp--formatting-indent-alist '(jtsx-tsx-mode . typescript-ts-mode-indent-offset))
  (add-to-list 'lsp--formatting-indent-alist '(jtsx-typescript-mode . typescript-ts-mode-indent-offset))

  (dolist (hook '(jtsx-jsx-mode-hook
                  jtsx-tsx-mode-hook
                  jtsx-typescript-mode-hook))
    (add-hook hook #'hs-minor-mode)
    (add-hook hook #'lsp-deferred))

  (dolist (mode-id '((jtsx-jsx-mode . "javascriptreact")
                     (jtsx-tsx-mode . "typescriptreact")
                     (jtsx-typescript-mode . "typescript")))
    (add-to-list 'lsp-language-id-configuration mode-id)))

(use-package markdown-mode
  :mode (("README\\.md\\'" . gfm-mode)
         ("\\.md\\'" . markdown-mode)))

(use-package terraform-mode
  :mode "\\.tf\\'")

(use-package yaml-mode
  :mode "\\.yml\\'")

;; ============================================================================
;; Packages - Everything else which isn't categorized
;; ============================================================================

(use-package apheleia
  :custom
  (apheleia-formatters-respect-indent-level nil)
  :config
  (apheleia-global-mode 1))

(use-package avy
  :bind (("C-;" . avy-goto-char-2)
         :map isearch-mode-map
         ("C-;" . avy-isearch))
  :config
  (avy-setup-default))

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

(use-package consult
  :bind (("C-c h" . consult-history)
         ("C-c i" . consult-info)
         ("C-x b" . consult-buffer) ;; orig. `switch-to-buffer'
         ("C-x 4 b" . consult-buffer-other-window) ;; orig. `switch-to-buffer-other-window'
         ("C-x p b" . consult-project-buffer) ;; orig. `project-switch-to-buffer'
         ("M-g o" . consult-outline)
         ("M-g i" . consult-imenu)
         ("M-s g" . consult-ripgrep)
         ("M-s l" . consult-line))
  :config
  ;; Use Consult to select xref locations with preview.
  (setq xref-show-xrefs-function #'consult-xref
        xref-show-definitions-function #'consult-xref))

(use-package corfu
  :custom
  (completion-ignore-case t)
  (corfu-auto t)
  (corfu-auto-delay 0)
  (corfu-auto-prefix 1)
  (tab-always-indent 'complete)
  :config
  (bind-key "C-SPC" #'corfu-insert-separator corfu-map)
  (add-hook 'eshell-mode-hook (lambda ()
                                (setq-local corfu-auto nil)))
  ;; Free the RET key for less intrusive behavior.
  (keymap-unset corfu-map "RET")
  (global-corfu-mode 1))

(use-package corfu-echo :ensure nil
  :hook (corfu-mode . corfu-echo-mode)
  :custom
  (corfu-echo-delay '(0.5 . 0.2)))

(use-package corfu-popupinfo :ensure nil
  :hook (corfu-mode . corfu-popupinfo-mode)
  :custom
  (corfu-popupinfo-delay '(nil . 0.2)) ;; hide the popup initially
  (corfu-popupinfo-max-height 20))

(use-package eat
  :hook (eshell-mode . eat-eshell-mode))

(use-package embark
  :bind (("C-." . embark-act)
         ("M-." . embark-dwim) ;; orig. `xref-find-definitions'
         ([remap describe-bindings] . embark-bindings))
  :config
  (setq prefix-help-command #'embark-prefix-help-command)

  ;; Embark actions for this buffer/file.
  (defun embark-target-this-buffer-file ()
    (cons 'this-buffer-file (buffer-name)))

  (add-to-list 'embark-target-finders #'embark-target-this-buffer-file 'append)

  (defvar-keymap embark-this-buffer-file-map
    :doc "Commands to act on the current file or buffer."
    :parent embark-general-map)

  (add-to-list 'embark-keymap-alist '(this-buffer-file . embark-this-buffer-file-map)))

(use-package embark-consult)

(use-package exec-path-from-shell
  :config
  (when (memq window-system '(ns pgtk))
    (exec-path-from-shell-initialize)))

(use-package expand-region
  :bind* ("C-," . er/expand-region))

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

(use-package harpun
  :load-path "lisp/harpun"
  :bind ("C-'" . harpun-set-register-or-swap)
  :config
  (harpun-setup-default))

(use-package helpful
  :bind (("C-c C-d" . helpful-at-point)
         ([remap describe-command] . helpful-command)
         ([remap describe-function] . helpful-callable)
         ([remap describe-key] . helpful-key)
         ([remap describe-variable] . helpful-variable)
         ([remap describe-symbol] . helpful-symbol)))

(use-package jinx
  :bind (("M-$" . jinx-correct)
         ("C-M-$" . jinx-languages))
  :hook ((org-mode markdown-mode) . jinx-mode))

(use-package marginalia
  :config
  (marginalia-mode 1))

(use-package modus-themes
  :custom
  (modus-themes-italic-constructs t)
  (modus-themes-to-toggle '(modus-operandi-tinted modus-vivendi-tinted))
  :config
  (defun my/modus-themes-custom-faces (&rest _)
    (modus-themes-with-colors
      (setq lsp-ui-doc-border border)
      (custom-set-faces
       `(lsp-ui-doc-background ((,c :background ,bg-dim))))))
  (add-hook 'modus-themes-post-load-hook #'my/modus-themes-custom-faces)

  (when IS-MAC (modus-themes-load-theme 'modus-operandi-tinted))
  (when IS-LINUX (modus-themes-load-theme 'modus-vivendi-tinted))

  (bind-key "<f5>" #'modus-themes-toggle))

(use-package move-text
  :bind (("C-M-n" . move-text-down) ;; orig. `forward-list'
         ("C-M-p" . move-text-up)) ;; orig. `backward-list'
  :preface
  (defun indent-region-advice (&rest ignored)
    (let ((deactivate deactivate-mark))
      (if (region-active-p)
          (indent-region (region-beginning) (region-end))
        (indent-region (line-beginning-position) (line-end-position)))
      (setq deactivate-mark deactivate)))
  :config
  (move-text-default-bindings)

  (advice-add #'move-text-up :after 'indent-region-advice)
  (advice-add #'move-text-down :after 'indent-region-advice))

(use-package orderless
  :custom
  (completion-styles '(orderless basic))
  (completion-category-overrides '((file (styles basic partial-completion)))))

(use-package vertico
  :custom
  (vertico-cycle t)
  :config
  (bind-keys :map vertico-map
             ("C-M-n" . vertico-next-group)
             ("C-M-p" . vertico-previous-group))
  (vertico-mode 1))

(use-package vertico-multiform :ensure nil
  :after vertico
  :custom
  (vertico-multiform-commands
   '((consult-ripgrep buffer)
     (consult-xref buffer)
     (embark-bindings buffer)))
  :config
  (vertico-multiform-mode 1))

(use-package wgrep
  :custom
  (wgrep-auto-save-buffer t))

(use-package yasnippet
  :hook (prog-mode . yas-minor-mode-on))
