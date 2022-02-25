;;; config--helm-perspeen.el --- Configuration of helm-perspeen.

;;; Commentary:

;; conflict with `config--persp-mode'

;;; Code:

(eval-when-compile
  (add-to-list 'load-path (concat user-emacs-directory "el-get/use-package")))

(require 'use-package)


;;; use-package

(use-package helm-perspeen
  :quelpa helm-perspeen
  :after (config--helm config--perspeen)
  :general
  (general-define-key :keymaps '(normal)
                      :prefix "SPC"
                      "pp" 'helm-perspeen))


(provide 'config--helm-perspeen)


;;; config--helm-perspeen.el ends here
