;;; config--nerd-icons.el --- Configuration of nerd-icons.

;;; Commentary:

;;; Code:

(require 'use-package)
(require 'quelpa-use-package)
(require 'utils--font)

(use-package nerd-icons
  :quelpa
  :config
  (unless (installed-font-family-p nerd-icons-font-family)
    (nerd-icons-install-fonts t)))

(provide 'config--nerd-icons)

;;; config--nerd-icons.el ends here
