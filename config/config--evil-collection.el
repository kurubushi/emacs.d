;;; config--evil-collection.el --- Configuration of evil-collection.

;;; Commentary:

;;; Code:

(eval-when-compile
  (add-to-list 'load-path (concat user-emacs-directory "el-get/use-package")))

(require 'use-package)


;; use-package

(use-package evil-collection
  :quelpa evil-collection
  :after config--evil
  :init
  (custom-set-variables '(evil-want-keybinding nil)) ; for evil-collection
  :config
  (evil-collection-init))

(provide 'config--evil-collection)


;;; config--evil-collection.el ends here
