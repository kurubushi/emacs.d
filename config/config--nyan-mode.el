;;; config--nyan-mode.el --- Configuration of nyan-mode.

;;; Commentary:

;;; Code:

(eval-when-compile
  (add-to-list 'load-path (concat user-emacs-directory "el-get/use-package")))

(require 'use-package)


;;; nyan-mode

(use-package nyan-mode
  :quelpa nyan-mode
  :config (nyan-mode t))


(provide 'config--nyan-mode)


;;; config--nyan-mode.el ends here
