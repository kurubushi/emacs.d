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

  ;; `evil-want-visual-char-semi-exclusive' is deprecated
  ;; and `evil-v$-excludes-newline' is recommended,
  ;; but I dare to use `evil-want-visual-char-semi-exclusive'.
  ;; Only with `evil-want-visual-char-semi-exclusive',
  ;; the visual mode can select an end of line,
  ;; and the next line is contained in selected range.
  ;; For example, consider the following text:
  ;; -----
  ;; abc
  ;;
  ;; def
  ;; -----
  ;; Only with `evil-want-visual-char-semi-exclusive',
  ;; the command `vjD' at the beginning of the text
  ;; deletes the `def' line against my will.
  (evil-want-visual-char-semi-exclusive t)

  ;; evil-collection assumes the following values.
  ;; ref. https://github.com/emacs-evil/evil-collection
  (evil-want-keybinding nil)
  (evil-want-integration t)

  ;; https://github.com/syl20bnr/spacemacs/issues/3623
  (search-invisible t)

  :config
  (evil-set-undo-system 'undo-tree) ; https://github.com/emacs-evil/evil/pull/1360
  (evil-mode 1))

(provide 'config--evil)

;;; config--evil.el ends here
