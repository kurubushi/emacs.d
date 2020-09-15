;;; config--ruby-mode.el --- Configuration of ruby-mode.

;;; Commentary:

;;; Code:

(eval-when-compile
  (add-to-list 'load-path (concat user-emacs-directory "el-get/use-package")))

(require 'use-package)


;;; use-package

(use-package ruby-mode
  :el-get ruby-mode
  :mode (("\\.rb\\'" . ruby-mode))
  :init
  (setq ruby-insert-encoding-magic-comment nil))


(provide 'config--ruby-mode)


;;; config--ruby-mode.el ends here
