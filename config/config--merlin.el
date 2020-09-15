;;; config--merlin.el --- Configuration of merlin.

;;; Commentary:

;;; Code:

(eval-when-compile
  (add-to-list 'load-path (concat user-emacs-directory "el-get/use-package")))

(require 'use-package)


;;; use-package

(use-package merlin
  :el-get merlin
  :after config--tuareg
  :commands merlin-mode
  :init
  (add-hook 'tuareg-mode-hook 'merlin-mode)
  :config
  (setq merlin-use-auto-complete-mode t)
  (setq merlin-error-after-save nil))


(provide 'config--merlin)


;;; config--merlin.el ends here
