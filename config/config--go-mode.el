;;; config--go-mode.el --- Configuration of go-mode.

;;; Commentary:

;;; Code:

(eval-when-compile
  (add-to-list 'load-path (concat user-emacs-directory "el-get/use-package")))

(require 'use-package)


;;; use-package

(use-package go-mode
  :el-get go-mode
  :after pre-config
  :mode (("\\.go\\'" . go-mode))
  :init
  ;; depends on gofmt(built-in) and godef
  ;; $ go get -u github.com/rogpeppe/godef
  (add-to-list 'exec-path (concat env--gopath "/bin"))
  (add-hook 'before-save-hook 'gofmt-before-save))

(provide 'config--go-mode)


;;; config--go-mode.el ends here
