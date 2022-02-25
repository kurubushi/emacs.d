;;; config--yaml-mode.el --- Configuration of yaml-mode.

;;; Commentary:

;;; Code:

(require 'use-package)
(require 'quelpa-use-package)

(use-package yaml-mode
  :quelpa
  :mode (("\\.yaml\\'" . yaml-mode)
         ("\\.yml\\'" . yaml-mode)))

(provide 'config--yaml-mode)

;;; config--yaml-mode.el ends here
