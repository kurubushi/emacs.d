;;; config--helm-git-grep.el --- Configuration of helm-git-grep.

;;; Commentary:

;;; Code:

(eval-when-compile
  (add-to-list 'load-path (concat user-emacs-directory "el-get/use-package")))

(require 'use-package)


;;; use-package

(use-package helm-git-grep
  :el-get helm-git-grep
  :after helm
  :general
  (general-define-key
   :keymaps 'normal
   :prefix "SPC g"
   "g" 'helm-git-grep))

(provide 'config--helm-git-grep)


;;; config--helm-git-grep.el ends here
