;;; config--neotree.el --- Configuration of neotree.

;;; Commentary:

;;; Code:

(require 'use-package)
(require 'quelpa-use-package)
(require 'general)

(use-package neotree
  :quelpa
  :after (config--evil utils--buffer)
  :functions (;; evil
              evil-set-initial-state
              ;; utils--buffer
              with-killing-mru-file-buffer
              mru-buffer)
  :defines kill-mru-file-buffer-before-find-file-p
  :commands (neotree-show neotree-hide neotree-dir neotree-find)
  :custom (neo-theme 'nerd2)

  :config
  (evil-set-initial-state 'neotree-mode 'emacs)

  (defun neotree-find-current-buffer ()
    "Open neotree buffer with the current buffer."
    (interactive)
    (let ((filepath (buffer-file-name)))
      (neotree-find filepath)))

  (defun neotree-go-up-node ()
    "Go to parent of selected node."
    (interactive)
    (goto-char (point-min))
    (neotree-select-up-node))

  (defun neotree-toggle ()
    "Open and close the side bar for neotree."
    (interactive)
    (if (neo-global--window-exists-p)
        (neotree-hide)
      (neotree-show)))

  (defun neotree-enter-with-killing-mru-file-buffer (&rest args)
    "Enter the selected node with killing the most recently used buffer.
ARG are the same as `neotree-enter'."
    (interactive "P")
    (with-killing-mru-file-buffer (apply 'neotree-enter args)))

  (defun neotree-back-to-mru-buffer ()
    (interactive)
    "Back to the most recently used buffer."
    (switch-to-buffer (mru-buffer)))

  :general
  (general-define-key :keymaps 'normal
                      :prefix "SPC f"
                      "d" 'neotree-show
                      "D" 'neotree-find-current-buffer)
  (general-define-key :keymaps 'neotree-mode-map
                      :states 'normal
                      "RET" 'neotree-enter
                      "C-<return>" 'neotree-enter-with-killing-mru-file-buffer
                      "j" 'neotree-next-line
                      "k" 'neotree-previous-line
                      "l" 'neotree-change-root
                      "h" 'neotree-go-up-node
                      ">" 'neotree-back-to-mru-buffer
                      "<" 'neotree-hide))

(provide 'config--neotree)

;;; config--neotree.el ends here
