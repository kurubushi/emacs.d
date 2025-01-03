;;; config--evil.el --- Configuration of Evil.

;;; Commentary:

;;; Code:

(require 'use-package)
(require 'quelpa-use-package)
(require 'general)

(use-package evil
  :quelpa
  :after config--undo-tree

  :custom
  (evil-want-C-u-scroll t)
  (evil-search-module 'isearch)

  ;; Enable only evil-v$-excludes-newline temporarily.
  ;; (evil-want-visual-char-semi-exclusive nil)
  (evil-v$-excludes-newline t)

  ;; evil-collection assumes the following values.
  ;; ref. https://github.com/emacs-evil/evil-collection
  (evil-want-keybinding nil)
  (evil-want-integration t)

  ;; https://github.com/syl20bnr/spacemacs/issues/3623
  (search-invisible t)

  :config
  ;; https://github.com/emacs-evil/evil/pull/1360
  (evil-set-undo-system 'undo-tree)

  (evil-mode 1))

(provide 'config--evil)

;;; config--evil.el ends here
