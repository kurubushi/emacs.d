;;; config--merlin.el --- Configuration of merlin.

;;; Commentary:

;;; Code:

(require 'use-package)
(require 'quelpa-use-package)

(use-package merlin
  :quelpa
  :after config--tuareg
  :commands merlin-mode
  :init
  (add-hook 'tuareg-mode-hook 'merlin-mode)
  :config
  (setq merlin-use-auto-complete-mode t)
  (setq merlin-error-after-save nil))

(provide 'config--merlin)

;;; config--merlin.el ends here
