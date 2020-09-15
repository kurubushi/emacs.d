;;; config--markdown-mode.el --- Configuration of markdown-mode.

;;; Commentary:

;;; Code:

(eval-when-compile
  (add-to-list 'load-path (concat user-emacs-directory "el-get/use-package")))

(require 'use-package)


;;; use-package

(use-package markdown-mode
  :el-get markdown-mode
  :mode (("\\.md\\'" . markdown-mode)))


(provide 'config--markdown-mode)


;;; config--markdown-mode.el ends here
