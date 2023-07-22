;;; config--ruby-mode.el --- Configuration of ruby-mode.

;;; Commentary:

;;; Code:

(require 'use-package)
(require 'quelpa-use-package)

(use-package ruby-mode
  :quelpa
  :after utils--buffer
  :mode ("\\.rb\\'" . ruby-mode)

  :config
  (setq ruby-insert-encoding-magic-comment nil)

  ;; If you want stree format the buffer on save,
  ;; add the following to your .dir-locals.el:
  ;;
  ;; ((ruby-mode . ((stree-write-on-save . t))))
  ;;
  (defvar stree-write-on-save nil
    "If non-nil, execute stree write command on save.")

  (defun execute-stree-write (buffer)
    "Execute stree write command at the BUFFER."
    (let* ((file-path (buffer-file-name buffer))
           (root-path (locate-dominating-file (file-name-directory file-path) ".streerc"))
           (dir-path (or root-path (file-name-directory file-path))))
      (message "Formatting %s." file-path)
      (with-cd dir-path (shell-command (format "stree write %s" file-path)))
      (message "Formatted %s." file-path)
      (with-current-buffer buffer (revert-buffer t t t))))

  (defun execute-stree-write-on-save ()
    "If `stree-write-on-save' is not `nil', execute stree write command on save."
    (when stree-write-on-save
      (message (format "Executing at %s" (buffer-file-name)))
      (execute-stree-write (current-buffer))))

  :hook
  (after-save . execute-stree-write-on-save))

(provide 'config--ruby-mode)

;;; config--ruby-mode.el ends here
