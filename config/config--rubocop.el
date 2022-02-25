;;; config--rubocop.el --- Configuration of rubocop.

;;; Commentary:

;;; Code:

(eval-when-compile
  (add-to-list 'load-path (concat user-emacs-directory "el-get/use-package")))

(require 'use-package)


;;; use-package

(use-package rubocop
  :quelpa (rubocop)
  :after config--flycheck
  :config
  (add-hook 'ruby-mode-hook 'rubocop-mode)
  (add-hook 'ruby-mode-hook
            '(lambda ()
               (setq flycheck-checker 'ruby-rubocop))))


(provide 'config--rubocop)


;;; config--rubocop.el ends here
