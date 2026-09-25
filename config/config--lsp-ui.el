;;; config--lsp-ui.el --- Configuration of lsp-ui.

;;; Commentary:

;;; Code:

(require 'use-package)
(require 'quelpa-use-package)

(use-package lsp-ui
  :quelpa
  :after (config--lsp-mode)
  :commands lsp-ui-mode
  :general
  (general-define-key :keymaps 'normal
                      :prefix "SPC l"
                      "i" 'lsp-ui-imenu))

(provide 'config--lsp-ui)

;;; config--lsp-ui.el ends here
