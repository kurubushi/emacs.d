;;; config--flyspell.el --- Configuration of flyspell.

;;; Commentary:

;;; Code:

(eval-when-compile
  (add-to-list 'load-path (concat user-emacs-directory "el-get/use-package")))

(require 'use-package)


;;; use-package

(use-package flyspell
  :quelpa flyspell
  :hook
  (text-mode . flyspell-mode)
  (prog-mode . flyspell-prog-mode))


(provide 'config--flyspell)


;;; config--flyspell.el ends here
