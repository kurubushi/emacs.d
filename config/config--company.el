;;; config--company.el --- Configuration of company-mode.

;;; Commentary:

;;; Code:

(eval-when-compile
  (add-to-list 'load-path (concat user-emacs-directory "el-get/use-package")))

(require 'use-package)


;;; company-mode

(use-package company
  :el-get company-mode

  :config
  (global-company-mode)


  :general
  (general-define-key :keymaps '(insert)
                      "C-k" 'company-complete))


(provide 'config--company)


;;; config--company.el ends here
