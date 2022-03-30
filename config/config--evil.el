;;; config--evil.el --- Configuration of Evil.

;;; Commentary:

;;; Code:

(require 'use-package)
(require 'quelpa-use-package)
(require 'general)

(use-package evil
  :quelpa
  :after config--undo-tree

  :init
  (custom-set-variables '(evil-want-keybinding nil)) ; for evil-collection

  :config
  (custom-set-variables '(search-invisible t)) ;https://github.com/syl20bnr/spacemacs/issues/3623
  (custom-set-variables '(evil-want-C-u-scroll t))
  (custom-set-variables  '(evil-want-visual-char-semi-exclusive t)) ; exclusive \n in visual state
  (custom-set-variables '(evil-want-integration nil)) ; for evil-collection
  (custom-set-variables  '(evil-search-module 'isearch))

  ;; https://github.com/emacs-evil/evil/pull/1360
  (evil-set-undo-system 'undo-tree)
  (evil-mode 1))

(provide 'config--evil)

;;; config--evil.el ends here
