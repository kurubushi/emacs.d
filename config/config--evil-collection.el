;;; config--evil-collection.el --- Configuration of evil-collection.

;;; Commentary:

;;; Code:

(require 'use-package)
(require 'quelpa-use-package)

(use-package evil-collection
  :quelpa
  :after config--evil
  :init
  (custom-set-variables '(evil-want-keybinding nil)) ; for evil-collection
  :config
  (evil-collection-init))

(provide 'config--evil-collection)

;;; config--evil-collection.el ends here
