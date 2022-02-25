;;; config--recentf.el --- Configuration of recentf.

;;; Commentary:

;;; Code:

(require 'use-package)

(use-package recentf
  :init
  (setq recentf-auto-cleanup 'never) ; disable before we start recentf!
  (setq recentf-max-saved-items 2000)
  (setq recentf-exclude '("~$" "recentf$"))
;;  (setq recentf-auto-save-timer (run-with-idle-timer 30 t 'recentf-save-list))
  :config
  (add-hook 'find-file-hook 'recentf-save-list)
  (recentf-mode 1))

(provide 'config--recentf)

;;; config--recentf.el ends here
