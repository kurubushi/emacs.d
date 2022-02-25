;;; config--lsp-ui.el --- Configuration of lsp-ui.

;;; Commentary:

;;; Code:

(require 'use-package)
(require 'quelpa-use-package)

(use-package lsp-ui
  :quelpa
  :after (config--lsp-mode)
  :commands lsp-ui-mode)

(provide 'config--lsp-ui)

;;; config--lsp-ui.el ends here
