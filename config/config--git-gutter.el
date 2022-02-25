;;; config--git-gutter.el --- Configuration of git-gutter.

;;; Commentary:

;;; Code:

(require 'use-package)
(require 'quelpa-use-package)

(use-package git-gutter
  :quelpa
  :custom
  (git-gutter:added-sign    "+")
  (git-gutter:deleted-sign  "-")
  (git-gutter:modified-sign "=")
  :config
  (global-git-gutter-mode +1))

(provide 'config--git-gutter)

;;; config--git-gutter.el ends here
