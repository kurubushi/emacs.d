;;; config--magit.el --- Configuration of magit.

;;; Commentary:

;;; Code:

(eval-when-compile
  (add-to-list 'load-path (concat user-emacs-directory "el-get/use-package")))

(require 'use-package)


;;; use-package

(use-package magit
  :quelpa magit
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
