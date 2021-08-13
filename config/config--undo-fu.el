;;; config--undo-fu.el --- Configuration of undo-fu.

;;; Commentary:

;;; Code:

(eval-when-compile
  (add-to-list 'load-path (concat user-emacs-directory "el-get/use-package")))

(require 'use-package)


;;; use-package

(use-package undo-fu
  :el-get undo-fu)


(provide 'config--undo-fu)


;;; config--undo-fu.el ends here
