;;; config--ispell.el --- Configuration of ispell.

;;; Commentary:

;;; Code:

(require 'use-package)

(use-package ispell
  :custom
  (ispell-program-name "aspell") ; depend on aspell
  (ispell-local-dictionary "en_US")

  :config
  (add-to-list 'ispell-skip-region-alist '("[^\000-\377]+"))) ; skip non English

(provide 'config--ispell)

;;; config--ispell.el ends here
