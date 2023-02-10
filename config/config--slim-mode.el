;;; config--slim-mode.el --- Configuration of slim-mode.

;;; Commentary:

;;; Code:

(require 'use-package)
(require 'quelpa-use-package)
(require 'general)

(use-package slim-mode
  :quelpa
  :mode ("\\.slim\\'" . slim-mode))

(provide 'config--slim-mode)

;;; config--slim-mode.el ends here
