;;; config--json-mode.el --- Configuration of json-mode.

;;; Commentary:

;;; Code:

(eval-when-compile
  (add-to-list 'load-path (concat user-emacs-directory "el-get/use-package")))

(require 'use-package)


;;; use-package

(use-package json-mode
  :el-get json-mode
  :mode (("\\.json\\'" . json-mode)))


(provide 'config--json-mode)


;;; config--json-mode.el ends here
