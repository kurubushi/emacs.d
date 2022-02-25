;;; config--flycheck.el --- Configuration of Flycheck.

;;; Commentary:

;;; Code:

(eval-when-compile
  (add-to-list 'load-path (concat user-emacs-directory "el-get/use-package")))

(require 'use-package)


;;; Flycheck

(use-package flycheck
  :quelpa flycheck
  :config (global-flycheck-mode))


(provide 'config--flycheck)


;;; config--flycheck.el ends here
