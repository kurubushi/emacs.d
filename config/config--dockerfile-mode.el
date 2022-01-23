;;; config--dockerfile-mode.el --- Configuration of dockerfile-mode.

;;; Commentary:

;;; Code:

(eval-when-compile
  (add-to-list 'load-path (concat user-emacs-directory "el-get/use-package")))

(require 'use-package)


;;; use-package

(use-package dockerfile-mode
  :el-get dockerfile-mode)


(provide 'config--dockerfile-mode)


;;; config--dockerfile-mode.el ends here
