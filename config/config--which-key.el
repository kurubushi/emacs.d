;;; config--which-key.el --- Configuration of which-key.

;;; Commentary:

;;; Code:

(require 'use-package)
(require 'quelpa-use-package)
(require 'general)

(use-package which-key
  :quelpa
  :config
  (which-key-mode)
  :general
  (general-define-key :keymaps '(normal)
                      :prefix "SPC"
                      "ww" 'which-key-show-top-level
                      "wk" 'describe-key
                      "wm" 'describe-mode
                      "wM" 'describe-keymap))

(provide 'config--which-key)

;;; config--which-key.el ends here
