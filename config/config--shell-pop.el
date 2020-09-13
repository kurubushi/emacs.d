;;; config--shell-pop.el --- Configuration of shell-pop.

;;; Commentary:

;;; Code:

(eval-when-compile
  (add-to-list 'load-path (concat user-emacs-directory "el-get/use-package")))

(require 'use-package)


;;; shell-pop

(use-package shell-pop
  :el-get shell-pop
  :general
  (general-define-key :keymaps '(normal insert visual emacs)
                      "<f8>" 'shell-pop))


(provide 'config--shell-pop)


;;; config--shell-pop.el ends here
