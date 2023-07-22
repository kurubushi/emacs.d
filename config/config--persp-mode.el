;;; config--persp-mode.el --- Configuration of persp-mode.

;;; Commentary:

;; conflict with `config--persp-mode'.

;;; Code:

(require 'cl-lib)
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
    (let* ((scratch-buf "*scratch*")
           (shared-buffers `(,scratch-buf)))

      ;; add buffers to new workspace
      (persp-add-buffer shared-buffers persp)

      (switch-to-buffer scratch-buf)))

  (defun persp-save-state-to-default-file ()
    "Save persp-mode state to default file.
  The default file path is
  `(expand-file-name persp-auto-save-fname persp-save-dir)'."
    (interactive)
    (persp-save-state-to-file))

  (defun persp-add-current-buffer-to-current-persp (&rest args)
    "Add current buffer to current perspective."
    (when (and persp-mode
               (not persp-temporarily-display-buffer))
      (persp-add-buffer (current-buffer))
      (persp-save-state-to-default-file)))

  ;; Ignore buffers which are in not current perspective.
  (add-to-list 'ivy-ignore-buffers 'persp-ignore-other-workspace-buffers)

  ;; Remove needless hook. This hook only fires when creating a buffer.
  (remove-hook 'find-file-hook 'persp-add-or-not-on-find-file)

  ;; Setup initial buffers (add-to-hook after persp-mode is enabled to avoid applyint to the first workspace)
  (add-to-list 'persp-created-functions 'persp-setup-initial-buffers)

  (defconst persp-selected-candidates '(file-buffers asterisked-buffers all-buffers free-buffers)
    "Candidates of buffers to be killed.")

  (defun kill-persp-selected-buffers (buffer-type)
    "Kill selected buffers at the current perspective."
    (interactive
     (list (intern (completing-read "Kill buffers: " persp-selected-candidates))))
    (let* ((filter (cl-case buffer-type
                     ('file-buffers 'buffer-file-name)
                     ('asterisked-buffers 'buffer-asterisked-p)
                     ('all-buffers 'identity)
                     ('free-buffers 'persp-buffer-free-p)
                     (t 'not)))
           (is-scratch (lambda (buf)
                         (string-match "\\*scratch\\*\\'" (buffer-name buf))))
           (buffers (cl-remove-if is-scratch (persp-buffer-list))))
      (mapc 'kill-buffer
            (cl-remove-if-not filter buffers))))

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
                               "s" 'persp-save-state-to-default-file)
           (general-define-key :keymaps 'normal
                               :prefix "SPC b"
                               "D" 'kill-persp-selected-buffers))

(provide 'config--persp-mode)

;;; config--persp-mode.el ends here
