;;; config--flycheck.el --- Configuration of Flycheck.

;;; Commentary:

;;; Code:

(require 'use-package)
(require 'quelpa-use-package)

(use-package flycheck
  :quelpa
  :config (global-flycheck-mode))

(provide 'config--flycheck)

;;; config--flycheck.el ends here
