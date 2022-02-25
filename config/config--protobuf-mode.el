;;; config--protobuf-mode.el --- Configuration of protobuf-mode.

;;; Commentary:

;;; Code:

(eval-when-compile
  (add-to-list 'load-path (concat user-emacs-directory "el-get/use-package")))

(require 'use-package)


;;; use-package

(use-package protobuf-mode
  :quelpa protobuf-mode
  :mode (("\\.proto\\'" . protobuf-mode)))


(provide 'config--protobuf-mode)


;;; config--protobuf-mode.el ends here
