;;; config--scala-mode.el --- Configuration of scala-mode.

;;; Commentary:

;;; Code:

(require 'use-package)
(require 'quelpa-use-package)

(use-package scala-mode
  :quelpa
  :mode (("\\.scala\\'" . scala-mode)))

(provide 'config--scala-mode)

;;; config--scala-mode.el ends here
