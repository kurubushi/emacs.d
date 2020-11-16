;;; config--yaml-mode.el --- Configuration of yaml-mode.

;;; Commentary:

;;; Code:

(eval-when-compile
  (add-to-list 'load-path (concat user-emacs-directory "el-get/use-package")))

(require 'use-package)


;;; use-package

(use-package yaml-mode
  :el-get yaml-mode
  :mode (("\\.yaml\\'" . yaml-mode)
         ("\\.yml\\'" . yaml-mode)))


(provide 'config--yaml-mode)


;;; config--yaml-mode.el ends here
