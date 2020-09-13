;;; config--git-gutter.el --- Configuration of git-gutter.

;;; Commentary:

;;; Code:

(eval-when-compile
  (add-to-list 'load-path (concat user-emacs-directory "el-get/use-package")))

(require 'use-package)


;;; git-gutter

(use-package git-gutter
  :el-get git-gutter
  :custom
  (git-gutter:added-sign    "+")
  (git-gutter:deleted-sign  "-")
  (git-gutter:modified-sign "=")
  :config
  (global-git-gutter-mode +1))


(provide 'config--git-gutter)


;;; config--git-gutter.el ends here
