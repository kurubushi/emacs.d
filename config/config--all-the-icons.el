;;; config--all-the-icons.el --- Configuration of all-the-icons.

;;; Commentary:

;;; Code:

(eval-when-compile
  (add-to-list 'load-path (concat user-emacs-directory "el-get/use-package")))

(require 'use-package)


;;; all-the-icons

(use-package all-the-icons
  :el-get all-the-icons
  :config
  (all-the-icons-install-fonts t))


(provide 'config--all-the-icons)


;;; config--all-the-icons.el ends here
