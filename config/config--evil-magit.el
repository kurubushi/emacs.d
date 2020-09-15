;;; config--evil-magit.el --- Configuration of evil-magit.

;;; Commentary:

;;; Code:

(eval-when-compile
  (add-to-list 'load-path (concat user-emacs-directory "el-get/use-package")))

(require 'use-package)


;;; use-package

(use-package evil-magit
  :el-get evil-magit
  :after (config--evil config--magit))


(provide 'config--evil-magit)


;;; config--evil-magit.el ends here
