;;; config--go-mode.el --- Configuration of go-mode.

;;; Commentary:

;;; Code:

(eval-when-compile
  (add-to-list 'load-path (concat user-emacs-directory "el-get/use-package")))

(require 'use-package)


;;; use-package

(use-package go-mode
  :el-get (go-mode go-autocomplete)
  :mode (("\\.go\\'" . go-mode))
  :init
  (add-to-list 'exec-path (concat (getenv "HOME") "/.go/bin"))
  (add-hook 'before-save-hook 'gofmt-before-save))

(provide 'config--go-mode)


;;; config--go-mode.el ends here
