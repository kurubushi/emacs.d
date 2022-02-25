;;; config--terraform-mode.el --- Configuration of terraform-mode.

;;; Commentary:

;;; Code:

(require 'use-package)
(require 'quelpa-use-package)

(use-package terraform-mode
  :quelpa
  :mode (("\\.tf\\'" . terraform-mode)))

(provide 'config--terraform-mode)

;;; config--terraform-mode.el ends here
