;;; config--lsp-mode.el --- Configuration of lsp-mode.

;;; Commentary:

;;; Code:

(require 'use-package)
(require 'quelpa-use-package)

(use-package lsp-mode
  :quelpa
  :hook
  (;; [Go] install gopls:
   ;; $ go get golang.org/x/tools/gopls
   (go-mode . lsp)
   ;; [Ruby] install solargraph
   ;; $ gem install --user solargraph
   (ruby-mode . lsp))
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
