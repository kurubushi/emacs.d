;;; config--evil.el --- Configuration of Evil.

;;; Commentary:

;;; Code:

(eval-when-compile
  (add-to-list 'load-path (concat user-emacs-directory "el-get/use-package")))

(require 'use-package)


;;; Evil

(use-package evil
  :el-get evil
  :init
  (custom-set-variables '(evil-want-keybinding nil)) ; for evil-collection
  :config
  (custom-set-variables '(search-invisible t)) ;https://github.com/syl20bnr/spacemacs/issues/3623
  (custom-set-variables '(evil-want-C-u-scroll t))
  (custom-set-variables  '(evil-want-visual-char-semi-exclusive t)) ; exclusive \n in visual state
  (custom-set-variables '(evil-want-integration nil)) ; for evil-collection
  (custom-set-variables  '(evil-search-module 'isearch))
  (custom-set-variables '(evil-move-cursor-back t)) ; goes back when reterning from insert and prevents going eol
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
