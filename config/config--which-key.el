;;; config--which-key.el --- Configuration of which-key.

;;; Commentary:

;;; Code:

(eval-when-compile
  (add-to-list 'load-path (concat user-emacs-directory "el-get/use-package")))

(require 'use-package)


;;; whitch-key

(use-package which-key
  :quelpa which-key
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
