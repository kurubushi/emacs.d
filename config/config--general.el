;;; config--general.el --- Configuration of general.

;;; Commentary:

;;; Code:

(eval-when-compile
  (add-to-list 'load-path (concat user-emacs-directory "el-get/cl-lib"))
  (add-to-list 'load-path (concat user-emacs-directory "el-get/use-package")))

(require 'cl-lib)
(require 'use-package)


;;; buffer

(defun is-asterisked (buffer)
  "Check if BUFFER's name is surrounded asterisks."
  (string-match "\\*.*\\*\\'" (buffer-name buffer)))

(defun kill-all-buffers-except-asterisked-buffers ()
  "Kill all asterisked buffers."
  (interactive)
  (mapc 'kill-buffer
        (cl-remove-if 'is-asterisked (buffer-list))))

(defun is-dired (buffer)
  "Check if BUFFER is directory buffer."
  (eq 'dired-mode (buffer-local-value 'major-mode buffer)))

(defun kill-all-dired-buffers ()
  "Kill all directory buffers."
  (interactive)
  (mapc 'kill-buffer
        (cl-remove-if-not 'is-dired (buffer-list))))


;;; eval

(defun eval-and-replace ()
  "Replace the preceding sexp with its value.
http://emacsredux.com/blog/2013/06/21/eval-and-replace/"
  (interactive)
  (backward-kill-sexp)
  (condition-case nil
      (prin1 (eval (read (current-kill 0)))
             (current-buffer))
    (error (message "Invalid expression")
           (insert (current-kill 0)))))


;;; general

(use-package general
  :el-get general
  :config
  (setq general-default-keymaps 'evil-normal-state-map)
  :general
  (general-define-key :keymaps '(insert)
                      ;; eval
                      "C-x C-r" 'eval-and-replace)
  (general-define-key :keymaps '(normal)
                      :prefix "SPC"
                      ;; view
                      "vs" 'text-scale-adjust
                      "vS" 'set-my-font-with-size
                      "vR" 'reload-my-theme
                      "vl" 'load-theme
                      "ve" 'enable-theme
                      "vd" 'disable-theme
                      ;; buffer
                      "bd" 'kill-this-buffer
                      "bD" 'kill-all-dired-buffers
                      "bA" 'kill-all-buffers-except-asterisked-buffers
                      "bn" 'next-buffer
                      "bp" 'previous-buffer
                      "bk" 'kill-some-buffers)
  ;; remove dangerous binds by typing miss
  (general-define-key :keymaps '(global-map)
                      "C-x C-u" 'nil ; delete 'upcase-region insted of 'disable
                      "C-x C-l" 'nil ; delete 'downcase-region insted of 'disable
                      ))


(provide 'config--general)


;;; config--general.el ends here
