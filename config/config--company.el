;;; config--company.el --- Configuration of company-mode.

;;; Commentary:

;;; Code:

(require 'use-package)
(require 'quelpa-use-package)
(require 'general)

(use-package company
  :quelpa
  :config
  (global-company-mode)
  :general
  (general-define-key :keymaps '(insert)
                      "C-k" 'company-complete))

(provide 'config--company)

;;; config--company.el ends here
