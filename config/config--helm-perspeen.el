;;; config--helm-perspeen.el --- Configuration of helm-perspeen.

;;; Commentary:

;; conflict with `config--persp-mode'

;;; Code:

(require 'use-package)
(require 'quelpa-use-package)
(require 'general)

(use-package helm-perspeen
  :quelpa
  :after (config--helm config--perspeen)
  :general (general-define-key :keymaps '(normal)
                               :prefix "SPC"
                               "pp" 'helm-perspeen))

(provide 'config--helm-perspeen)

;;; config--helm-perspeen.el ends here
