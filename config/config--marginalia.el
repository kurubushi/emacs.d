;;; config--marginalia.el --- Configuration of marginalia.

;;; Commentary:

;;; Code:

(require 'use-package)
(require 'quelpa-use-package)

(use-package marginalia
  :quelpa
  :config (marginalia-mode))

(provide 'config--marginalia)

;;; config--marginalia.el ends here
