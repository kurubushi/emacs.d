;;; config--fish-mode.el --- Configuration of fish-mode.

;;; Commentary:

;;; Code:

(require 'use-package)
(require 'quelpa-use-package)

(use-package fish-mode
  :quelpa
  :mode ("\\.fish\\'" . fish-mode))

(provide 'config--fish-mode)

;;; config--fish-mode.el ends here
