;;; -*- lexical-binding: t; -*-

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
