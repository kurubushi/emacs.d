;;; config--nerd-icons.el --- Configuration of nerd-icons.

;;; Commentary:

;;; Code:

(require 'use-package)
(require 'quelpa-use-package)

(use-package nerd-icons
  :quelpa
  :config
  (unless (find-font (font-spec :name nerd-icons-font-family))
    (nerd-icons-install-fonts t)))

(provide 'config--nerd-icons)

;;; config--nerd-icons.el ends here
