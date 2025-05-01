;;; -*- lexical-binding: t; -*-

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
