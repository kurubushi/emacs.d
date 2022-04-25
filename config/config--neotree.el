;;; config--neotree.el --- Configuration of neotree.

;;; Commentary:

;;; Code:

(require 'use-package)
(require 'quelpa-use-package)
(require 'general)

(use-package neotree
  :quelpa
  :after (config--evil)
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

  :general
  (general-define-key :keymaps 'normal
                      :prefix "SPC f"
                      "d" 'neotree-toggle
                      "D" 'neotree-find-current-buffer)
  (general-define-key :keymaps 'neotree-mode-map
                      :states 'normal
                      "j" 'neotree-next-line
                      "k" 'neotree-previous-line
                      "l" 'neotree-change-root
                      "h" 'neotree-go-up-node))

(provide 'config--neotree)

;;; config--neotree.el ends here
