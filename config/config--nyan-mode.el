;;; config--nyan-mode.el --- Configuration of nyan-mode.

;;; Commentary:

;;; Code:

(require 'use-package)
(require 'quelpa-use-package)

(use-package nyan-mode
  :quelpa
  :config (nyan-mode t))

(provide 'config--nyan-mode)

;;; config--nyan-mode.el ends here
