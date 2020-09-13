;;; config--persp-mode.el --- Configuration of persp-mode.

;;; Commentary:

;; conflict with `config--persp-mode'.

;;; Code:

(eval-when-compile
  (add-to-list 'load-path (concat user-emacs-directory "el-get/use-package")))

(require 'use-package)


;;; persp-mode

(use-package persp-mode
  :el-get persp-mode
  :config
  (persp-mode 1)
  (defun persp-helm-mini ()
    (interactive)
    (with-persp-buffer-list () (helm-mini)))
  :general
  (general-define-key :keymaps '(normal)
                      :prefix "SPC"
                      "ps" 'persp-frame-switch
                      "pS" 'persp-window
                      "pr" 'persp-rename
                      "pc" 'persp-copy
                      "pk" 'persp-kill
                      "pp" 'persp-helm-mini))


(provide 'config--persp-mode)


;;; config--persp-mode.el ends here
