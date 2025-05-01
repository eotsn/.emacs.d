;;; -*- lexical-binding: t; -*-

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
