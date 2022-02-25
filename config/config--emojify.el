;;; config--emojify.el --- Configuration of emojify.

;;; Commentary:

;;; Code:

(eval-when-compile
  (add-to-list 'load-path (concat user-emacs-directory "el-get/use-package")))

(require 'use-package)


;;; use-package

;; GitHub: https://github.com/iqbalansari/emacs-emojify

(use-package emojify
  :quelpa emojify
  :hook (after-init . global-emojify-mode)
  :config (emojify-set-emoji-styles '(github)))


(provide 'config--emojify)


;;; config--emojify.el ends here
