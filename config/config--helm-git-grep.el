;;; config--helm-git-grep.el --- Configuration of helm-git-grep.

;;; Commentary:

;;; Code:

(require 'use-package)
(require 'quelpa-use-package)
(require 'general)

(use-package helm-git-grep
  :quelpa
  :after helm
  :general (general-define-key :keymaps 'normal
                               :prefix "SPC g"
                               "g" 'helm-git-grep))

(provide 'config--helm-git-grep)

;;; config--helm-git-grep.el ends here
