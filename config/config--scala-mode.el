;;; config--scala-mode.el --- Configuration of scala-mode.

;;; Commentary:

;;; Code:

(eval-when-compile
  (add-to-list 'load-path (concat user-emacs-directory "el-get/use-package")))

(require 'use-package)


;;; use-package

(use-package scala-mode
  :quelpa scala-mode
  :mode (("\\.scala\\'" . scala-mode)))


(provide 'config--scala-mode)


;;; config--scala-mode.el ends here
