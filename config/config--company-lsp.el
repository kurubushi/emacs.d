;;; config--company-lsp.el --- Configuration of company-lsp.

;;; Commentary:

;;; Code:

(eval-when-compile
  (add-to-list 'load-path (concat user-emacs-directory "el-get/use-package")))

(require 'use-package)


;;; use-package

(use-package company-lsp
  :el-get company-lsp
  :after (config--company config--lsp-mode))


(provide 'config--company-lsp)


;;; config--company-lsp.el ends here
