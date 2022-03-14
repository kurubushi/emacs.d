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
  (evil-mode 1)

  ;;keymap
  ;; original `evil-execute-in-normal-state` executes as "C-\ C-o" of Vim.
  ;; `evil-execute-in-normal-state-natively` executes as "C-o" of Vim.
  (defun evil-execute-in-normal-state-natively ()
    "Execute the next command in Normal state, natively."
    (interactive)
    (setq evil-move-cursor-back_ evil-move-cursor-back)
    (evil-execute-in-normal-state)
    ;; returns to original value without wating evil-delay
    (setq evil-move-cursor-back evil-move-cursor-back_))
  (general-define-key :keymaps '(insert)
                      "C-o" 'evil-execute-in-normal-state-natively))

(provide 'config--evil)

;;; config--evil.el ends here
