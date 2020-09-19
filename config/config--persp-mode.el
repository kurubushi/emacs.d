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
  :after config--ivy

  :config
  (defun persp-ignore-other-workspace-buffers (buffer)
    "Ignore BUFFER if it is in other workspaces."
    (when persp-mode
      (not (persp-contain-buffer-p buffer (get-current-persp)))))
  (add-hook 'ivy-ignore-buffers 'persp-ignore-other-workspace-buffers)
  (persp-mode 1)

  :general
  (general-define-key
   :keymaps 'normal
   :prefix "SPC p"
   "p" 'persp-frame-switch
   "w" 'persp-window
   "r" 'persp-rename
   "c" 'persp-copy
   "k" 'persp-kill))

(provide 'config--persp-mode)


;;; config--persp-mode.el ends here
