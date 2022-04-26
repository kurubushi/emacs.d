;;; config--persp-mode.el --- Configuration of persp-mode.

;;; Commentary:

;; conflict with `config--persp-mode'.

;;; Code:

(require 'use-package)
(require 'quelpa-use-package)
(require 'general)

(use-package persp-mode
  :quelpa
  :demand
  :after (utils--find-file config--ivy)

  :custom (ivy-use-ignore-default 'always)

  :config
  (defun persp-ignore-other-workspace-buffers (buffer)
    "Ignore BUFFER if it is in other workspaces."
    (when persp-mode
      (not (persp-contain-buffer-p buffer (get-current-persp)))))

  (defun persp-add-or-not-on-find-file-with-any-args (&rest args)
    "Execute `persp-add-or-not-on-find-file' with ignoring ARGS."
    (persp-add-or-not-on-find-file))

  (defun persp-setup-initial-buffers (persp &rest args)
    "Create a buffer for scratch and Share some buffers in PERSP, ignoring ARGS."
    (let* ((scratch-buf (format "*scratch<%s>*" (format-time-string "%s")))
           (shared-buffers `("*Messages*" ,scratch-buf)))

      ;; create a buffer for scratch.
      (switch-to-buffer scratch-buf)
      (funcall initial-major-mode)

      ;; add buffers to new workspace
      (persp-add-buffer shared-buffers persp)

      (switch-to-buffer scratch-buf)))

  (defun persp-save-state-to-default-file ()
    "Save persp-mode state to default file.
  The default file path is `(expand-file-name persp-auto-save-fname persp-save-dir)'."
    (interactive)
    (persp-save-state-to-file))

  (defun persp-add-current-buffer-to-current-persp (&rest args)
    "Add current buffer to current perspective."
    (when (and persp-mode
               (not persp-temporarily-display-buffer))
      (persp-add-buffer (current-buffer))))

  ;; Ignore buffers which are in not current perspective.
  (add-to-list 'ivy-ignore-buffers 'persp-ignore-other-workspace-buffers)

  ;; Remove needless hook. This hook only fires when creating a buffer.
  (remove-hook 'find-file-hook 'persp-add-or-not-on-find-file)

  ;; Setup initial buffers (add-to-hook after persp-mode is enabled to avoid applyint to the first workspace)
  (add-to-list 'persp-created-functions 'persp-setup-initial-buffers)

  (persp-mode 1)

  :hook ((after-find-file         . persp-add-current-buffer-to-current-persp)
         (after-ivy-switch-buffer . persp-add-current-buffer-to-current-persp))

  :general (general-define-key :keymaps 'normal
                               :prefix "SPC p"
                               "p" 'persp-frame-switch
                               "w" 'persp-window
                               "r" 'persp-rename
                               "c" 'persp-copy
                               "k" 'persp-kill
                               "s" 'persp-save-state-to-default-file))

(provide 'config--persp-mode)

;;; config--persp-mode.el ends here
