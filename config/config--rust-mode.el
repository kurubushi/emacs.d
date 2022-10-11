;;; config--rust-mode.el --- Configuration of rust-mode.

;;; Commentary:

;;; Code:

(require 'use-package)
(require 'quelpa-use-package)

(use-package rust-mode
  :quelpa
  :mode ("\\.rs\\'" . rust-mode)
  :custom (rust-format-on-save t))

(use-package cargo
  :quelpa
  :hook (rust-mode . cargo-minor-mode))

(provide 'config--rust-mode)

;;; config--rust-mode.el ends here
