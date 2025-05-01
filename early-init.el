;;; early-init.el -*- lexical-binding: t; -*-

;; Temporarily increase the garbage collection threshold to improve
;; startup time.  It's VERY IMPORTANT to reset `gc-cons-threshold' to
;; ensure normal operation.
(setq gc-cons-threshold most-positive-fixnum
      gc-cons-percentage 0.5)

(add-hook 'emacs-startup-hook
	  (lambda ()
	    (setq gc-cons-threshold (* 128 1024 1024) ;; 128 MB
		  gc-cons-percentage 0.1)))

(setq package-enable-at-startup nil) ;; we use Elpaca!

(push '(menu-bar-lines . 0) default-frame-alist)
(push '(tool-bar-lines . 0) default-frame-alist)
(push '(vertical-scroll-bars) default-frame-alist)

(defconst IS-MAC (eq system-type 'darwin))
(defconst IS-LINUX (eq system-type 'gnu/linux))

(if IS-MAC
    (push '(font . "PragmataPro-18") default-frame-alist)
  (push '(font . "PragmataPro-13") default-frame-alist))

;; Use plists for deserialization with `lsp-mode'.
(setenv "LSP_USE_PLISTS" "true")
