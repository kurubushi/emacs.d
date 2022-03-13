;;; config--ruby-mode.el --- Configuration of ruby-mode.

;;; Commentary:

;;; Code:

(require 'use-package)
(require 'quelpa-use-package)

;; Install solargraph to use an lsp server.
;; $ gem install --user solargraph

(use-package ruby-mode
  :quelpa
  :after lsp-mode
  :mode (("\\.rb\\'" . ruby-mode))
  :init (setq ruby-insert-encoding-magic-comment nil)
  :hook (ruby-mode . lsp))

(provide 'config--ruby-mode)

;;; config--ruby-mode.el ends here
