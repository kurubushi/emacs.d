;;; config--prettier.el --- Configuration of prettier.

;;; Commentary:

;;; Code:

(require 'use-package)
(require 'quelpa-use-package)

;; Install Prettier:
;; $ npm install -g prettier

(use-package prettier
  :quelpa prettier

  :config
  (defvar prettier-enabled nil
    "If non-nil, prettier-mode is enabled.

If you want to use prettier on a project, create .dir-locals.el:
((ruby-mode . ((prettier-enabled . t))))")

  (defun prettier-mode-turn-on-if-enabled ()
    "Turn on prettier-mode if needed."
    (when prettier-enabled
      (prettier-mode 1)))

  :hook ((web-mode . prettier-mode)
         (hack-local-variables . prettier-mode-turn-on-if-enabled)))

(provide 'config--prettier)

;;; config--prettier.el ends here
