;;; config--lsp-mode.el --- Configuration of lsp-mode.

;;; Commentary:

;;; Code:

(eval-when-compile
  (add-to-list 'load-path (concat user-emacs-directory "el-get/use-package")))

(require 'use-package)


;;; use-package

(use-package lsp-mode
  :el-get lsp-mode
  ;; [Go] install gopls:
  ;; $ go get golang.org/x/tools/gopls
  :hook (go-mode . lsp)
  :commands lsp
  :init
  ;; define functions for backward compatibility
  (defun lsp--sort-completions (completions)
  (lsp-completion--sort-completions completions))

  (defun lsp--annotate (item)
    (lsp-completion--annotate item))

  (defun lsp--resolve-completion (item)
    (lsp-completion--resolve item)))


(provide 'config--lsp-mode)


;;; config--lsp-mode.el ends here
