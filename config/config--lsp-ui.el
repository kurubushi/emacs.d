;;; config--lsp-ui.el --- Configuration of lsp-ui.

;;; Commentary:

;;; Code:

(eval-when-compile
  (add-to-list 'load-path (concat user-emacs-directory "el-get/use-package")))

(require 'use-package)


;;; use-package

(use-package lsp-ui
  :quelpa lsp-ui
  :after (config--lsp-mode)
  :commands lsp-ui-mode)


(provide 'config--lsp-ui)


;;; config--lsp-ui.el ends here
