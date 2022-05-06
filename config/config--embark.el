;;; config--embark.el --- Configuration of embark.

;;; Commentary:

;;; Code:

(require 'use-package)
(require 'quelpa-use-package)

(use-package embark
  :quelpa
  :demand
  :after vertico
  :general
  (general-define-key :keymaps 'vertico-map
                      "C-." #'embark-act))

(provide 'config--embark)

;;; config--embark.el ends here
