;;; config--haskell-mode.el --- Configuration of haskell-mode.

;;; Commentary:

;;; Code:

(require 'use-package)
(require 'quelpa-use-package)

(use-package haskell-mode
  :quelpa
  :mode (("\\.hs\\'" . haskell-mode)))

(provide 'config--haskell-mode)

;;; config--haskell-mode.el ends here
