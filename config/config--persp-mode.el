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
  :after (config--emacs config--ivy)

  :custom
  (ivy-use-ignore-default 'always)

  :config
  ;; Ignore buffers not included in the current workspace.
  (defun persp-ignore-other-workspace-buffers (buffer)
    "Ignore BUFFER if it is in other workspaces."
    (when persp-mode
      (not (persp-contain-buffer-p buffer (get-current-persp)))))
  (add-hook 'ivy-ignore-buffers 'persp-ignore-other-workspace-buffers)

  ;; Add a buffer to current workspace even if the buffer is open in other workspaces.
  (defun persp-add-or-not-on-find-file-with-any-args (&rest args)
    "Execute `persp-add-or-not-on-find-file' with ignoring ARGS."
    (persp-add-or-not-on-find-file))
  (remove-hook 'find-file-hooks 'persp-add-or-not-on-find-file) ; This hook only fires when creating a buffer.
  (add-hook 'after-find-file-hooks 'persp-add-or-not-on-find-file-with-any-args)

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
