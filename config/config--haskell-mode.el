;;; config--haskell-mode.el --- Configuration of haskell-mode.

;;; Commentary:

;;; Code:

(eval-when-compile
  (add-to-list 'load-path (concat user-emacs-directory "el-get/use-package")))

(require 'use-package)


;;; use-package

(use-package haskell-mode
  :el-get haskell-mode
  :mode (("\\.hs\\'" . haskell-mode)))


(provide 'config--haskell-mode)


;;; config--haskell-mode.el ends here
