;;; harpun.el --- Whaling with registers -*- lexical-binding: t; -*-

(defun harpun--get-register-for-buffer ()
  (let ((register nil))
    (dolist (r (number-sequence ?1 ?5) register)
      (let ((contents (get-register r)))
        (when (and contents
                   (stringp (cdr contents))
                   (string= (buffer-name) (cdr contents)))
          (setq register (cons r contents)))))))

;;;###autoload
(defun harpun-swap-register (register &optional arg)
  (interactive (list (register-read-with-preview
                      "Swap with register: ")
                     current-prefix-arg))
  (let ((old (harpun--get-register-for-buffer))
        (new (get-register register)))
    (set-register register (cdr old))
    (set-register (car old) new)))

;;;###autoload
(defun harpun-buffer-to-register (register &optional arg)
  (interactive (list (register-read-with-preview
                      "Buffer to register: ")
                     current-prefix-arg))
  ;; Reset the marker if the buffer is killed.
  (add-hook 'kill-buffer-hook `(lambda () (set-register ,register nil)) nil t)
  (set-register register `(buffer . ,(buffer-name))))

;;;###autoload
(defun harpun-set-register-or-swap ()
  (interactive)
  (let ((register (harpun--get-register-for-buffer)))
    (if register
        (call-interactively #'harpun-swap-register)
      (call-interactively #'harpun-buffer-to-register))))

;;;###autoload
(defun harpun-setup-default ()
  (eval-after-load "register"
    (dotimes (i 5)
      (let ((key (format "C-%d" (1+ i)))
            (register (int-to-string (1+ i))))
        (global-set-key (kbd key)
                        `(lambda ()
                           (interactive)
                           (jump-to-register ,(string-to-char register))))))))

(provide 'harpun)
;;; harpun.el ends here
