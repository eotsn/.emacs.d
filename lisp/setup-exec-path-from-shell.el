;;; -*- lexical-binding: t; -*-

(use-package exec-path-from-shell
  :config
  (when (memq window-system '(ns pgtk))
    (exec-path-from-shell-initialize)))
