;;; config--magit.el --- Configuration of magit.

;;; Commentary:

;;; Code:

(require 'use-package)
(require 'quelpa-use-package)
(require 'general)

(use-package magit
  :quelpa
  :general
  (general-define-key :keymaps '(normal)
                      :prefix "SPC"
                      "gs" 'magit-status))

(use-package magit-popup
  :quelpa
  :general
  (general-define-key :keymaps '(normal)
                      :prefix "SPC"
                      "gl" 'magit-log-popup))

(provide 'config--magit)

;;; config--magit.el ends here
