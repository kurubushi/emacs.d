;;; config--ruby-mode.el --- Configuration of ruby-mode.

;;; Commentary:

;;; Code:

(require 'use-package)
(require 'quelpa-use-package)

(use-package ruby-mode
  :quelpa
  :mode ("\\.rb\\'" . ruby-mode)
  :config (setq ruby-insert-encoding-magic-comment nil))

(provide 'config--ruby-mode)

;;; config--ruby-mode.el ends here
