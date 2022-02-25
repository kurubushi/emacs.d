;;; config--rubocop.el --- Configuration of rubocop.

;;; Commentary:

;;; Code:

(require 'use-package)
(require 'quelpa-use-package)

;;; use-package

(use-package rubocop
  :quelpa
  :after (config--flycheck config--ruby-mode)
  :config
  (add-hook 'ruby-mode-hook 'rubocop-mode)
  (add-hook 'ruby-mode-hook
            '(lambda ()
               (setq flycheck-checker 'ruby-rubocop))))

(provide 'config--rubocop)

;;; config--rubocop.el ends here
