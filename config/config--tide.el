;;; config--tide.el --- Configuration of tide.

;;; Commentary:

;;; Code:

(require 'use-package)
(require 'quelpa-use-package)

(use-package tide
  :quelpa
  :after (config--flycheck config--company)

  :custom
  (typescript-indent-level 2)

  :config
  (defun setup-tide-mode ()
    (interactive)
    (tide-setup)
    (flycheck-mode +1)
    (setq flycheck-check-syntax-automatically '(save mode-enabled))
    (eldoc-mode +1)
    (tide-hl-identifier-mode +1)
    ;; company is an optional dependency. You have to
    ;; install it separately via package-install
    ;; `M-x package-install [ret] company`
    (company-mode +1)))

(provide 'config--tide)

;;; config--tide.el ends here
