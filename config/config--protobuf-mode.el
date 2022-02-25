;;; config--protobuf-mode.el --- Configuration of protobuf-mode.

;;; Commentary:

;;; Code:

(require 'use-package)
(require 'quelpa-use-package)

(use-package protobuf-mode
  :quelpa
  :mode (("\\.proto\\'" . protobuf-mode)))

(provide 'config--protobuf-mode)

;;; config--protobuf-mode.el ends here
