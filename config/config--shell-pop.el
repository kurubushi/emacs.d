;;; config--shell-pop.el --- Configuration of shell-pop.

;;; Commentary:

;;; Code:

(require 'use-package)
(require 'quelpa-use-package)
(require 'general)

(use-package shell-pop
  :quelpa
  :general
  (general-define-key :keymaps '(normal insert visual emacs)
                      "<f8>" 'shell-pop))

(provide 'config--shell-pop)

;;; config--shell-pop.el ends here
