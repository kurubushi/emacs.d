;;; config--all-the-icons.el --- Configuration of all-the-icons.

;;; Commentary:

;;; Code:

(require 'use-package)
(require 'quelpa-use-package)

(use-package all-the-icons
  :quelpa
  :config
  (all-the-icons-install-fonts t))

(provide 'config--all-the-icons)

;;; config--all-the-icons.el ends here
