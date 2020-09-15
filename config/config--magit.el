;;; config--magit.el --- Configuration of magit.

;;; Commentary:

;;; Code:

(eval-when-compile
  (add-to-list 'load-path (concat user-emacs-directory "el-get/use-package")))

(require 'use-package)


;;; use-package

(use-package magit
  :el-get (magit magit-popup)
  :general
  (general-define-key :keymaps '(normal)
                      :prefix "SPC"
                      "gs" 'magit-status
                      "gl" 'magit-log-popup))


(provide 'config--magit)


;;; config--magit.el ends here
