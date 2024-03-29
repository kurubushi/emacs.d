;;; config--go-mode.el --- Configuration of go-mode.

;;; Commentary:

;;; Code:

(require 'use-package)
(require 'quelpa-use-package)

;; Depends on gofmt(built-in) and godef
;; $ go get -u github.com/rogpeppe/godef

(use-package go-mode
  :quelpa
  :after pre-config
  :mode ("\\.go\\'" . go-mode)
  :init (add-to-list 'exec-path (concat env--gopath "/bin"))
  :hook (before-save . gofmt-before-save))

(provide 'config--go-mode)

;;; config--go-mode.el ends here
