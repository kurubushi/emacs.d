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
                      "C-o" 'evil-execute-in-normal-state-natively)
  (general-define-key :keymaps '(normal)
                      :prefix "SPC"
                      "SPC" 'execute-extended-command))


;;; Evil collection

;; provides some keymaps.

(use-package evil-collection
  :el-get evil-collection
  :after evil
  :init
  (custom-set-variables '(evil-want-keybinding nil)) ; for evil-collection
  :config
  (evil-collection-init)

  ;; https://github.com/emacs-evil/evil-collection/issues/79
  ;; commited code was wrong.
  ;; this is called by (evil-collection-paren-setup)
  (defun evil-collection-paren-show-paren-function (f &rest args)
    "Integrate `show-paren-function' with `evil'."
    (if (not (bound-and-true-p evil-mode))
        (apply f args)
      (if (if (memq 'not evil-highlight-closing-paren-at-point-states)
              (memq evil-state evil-highlight-closing-paren-at-point-states)
            (not (memq evil-state evil-highlight-closing-paren-at-point-states)))
          (apply f args)
        (let ((pos (point)) syntax narrow)
          (setq pos
                (catch 'end
                  (dotimes (var (1+ (* 2 evil-show-paren-range)))
                    (if (zerop (mod var 2))
                        (setq pos (+ pos var))
                      (setq pos (- pos var)))
                    (setq syntax (syntax-class (syntax-after pos)))
                    (cond
                     ((eq syntax 4)
                      (setq narrow pos)
                      (throw 'end pos))
                     ((eq syntax 5)
  ;;                    (throw 'end (1+ pos))))))) ; wrong line
                      (throw 'end pos)))))) ; this is correct
          (if pos
              (save-excursion
                (goto-char pos)
                (save-restriction
                  (when narrow
                    (narrow-to-region narrow (point-max)))
                  (apply f args)))
            ;; prevent the preceding pair from being highlighted
            (dolist (ov '(show-paren--overlay
                          show-paren--overlay-1
                          show-paren-overlay
                          show-paren-overlay-1))
              (let ((ov (and (boundp ov) (symbol-value ov))))
                (when (overlayp ov) (delete-overlay ov))))))))))


(provide 'config--evil)


;;; config--evil.el ends here
