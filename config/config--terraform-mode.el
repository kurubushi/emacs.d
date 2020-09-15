;;; config--terraform-mode.el --- Configuration of terraform-mode.

;;; Commentary:

;;; Code:

(eval-when-compile
  (add-to-list 'load-path (concat user-emacs-directory "el-get/use-package")))

(require 'use-package)


;;; use-package

(use-package terraform-mode
  :el-get (terraform-mode)
  :mode (("\\.tf\\'" . terraform-mode)))


(provide 'config--terraform-mode)


;;; config--terraform-mode.el ends here
