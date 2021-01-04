;;; config--ispell.el --- Configuration of ispell.

;;; Commentary:

;;; Code:

(eval-when-compile
  (add-to-list 'load-path (concat user-emacs-directory "el-get/use-package")))

(require 'use-package)


;;; use-package

(use-package ispell
  :custom
  (ispell-program-name "aspell") ; depend on aspell
  (ispell-local-dictionary "en_US")

  :config
  (add-to-list 'ispell-skip-region-alist '("[^\000-\377]+"))) ; skip non English


;;; config--ispell.el ends here
