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
  :custom
  (neo-theme 'nerd2)
  :config
  (evil-set-initial-state 'neotree-mode 'emacs)
  (defun neotree-toggle-on-current-buffer ()
    "Open neotree buffer with the current buffer."
    (interactive)
    (if (neo-global--window-exists-p)
        (neotree-hide)
      (let ((filepath (buffer-file-name)))
        (neotree-find filepath))))
  (defun neotree-go-up-node ()
    "Go to parent of selected node."
    (interactive)
    (goto-char (point-min))
    (neotree-select-up-node))
  :general
  (general-define-key :keymaps 'normal
                      :prefix "SPC f"
                      "d" 'neotree-toggle-on-current-buffer)
  (general-define-key :keymaps 'neotree-mode-map
                      :states 'normal
                      "j" 'neotree-next-line
                      "k" 'neotree-previous-line
                      "l" 'neotree-change-root
                      "h" 'neotree-go-up-node))

(provide 'config--neotree)

;;; config--neotree.el ends here
