;;; config--evil-collection.el --- Configuration of evil-collection.

;;; Commentary:

;;; Code:

(eval-when-compile
  (add-to-list 'load-path (concat user-emacs-directory "el-get/use-package")))

(require 'use-package)


;; use-package

(use-package evil-collection
  :el-get evil-collection
  :after config--evil
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


(provide 'config--evil-collection)


;;; config--evil-collection.el ends here
