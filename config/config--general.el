;;; config--general.el --- Configuration of general.

;;; Commentary:

;;; Code:

(require 'use-package)
(require 'quelpa-use-package)

;;; eval


;;; general

(use-package general
  :quelpa general
  :custom (general-default-keymaps 'evil-normal-state-map)

  :config
  (defun eval-and-replace ()
    "Replace the preceding sexp with its value.
http://emacsredux.com/blog/2013/06/21/eval-and-replace/"
    (interactive)
    (backward-kill-sexp)
    (condition-case nil
        (prin1 (eval (read (current-kill 0)))
               (current-buffer))
      (error (message "Invalid expression")
             (insert (current-kill 0)))))

  ;; ここで :general キーワードは利用できない
  (general-define-key :keymaps '(insert)
                      ;; eval
                      "C-x C-r" 'eval-and-replace)
  (general-define-key :keymaps '(normal)
                      :prefix "SPC"
                      ;; view
                      "vs" 'text-scale-adjust
                      "vS" 'set-my-font-size
                      "vF" 'set-my-font-family
                      "vR" 'reload-my-theme
                      "vl" 'load-theme
                      "ve" 'enable-theme
                      "vd" 'disable-theme
                      ;; buffer
                      "bd" 'kill-current-buffer
                      "bn" 'next-buffer
                      "bp" 'previous-buffer
                      "bk" 'kill-some-buffers)
  ;; remove dangerous binds by typing miss
  (general-define-key :keymaps '(global-map)
                      "C-x C-u" 'nil ; delete 'upcase-region insted of 'disable
                      "C-x C-l" 'nil ; delete 'downcase-region insted of 'disable
                      ))

(provide 'config--general)

;;; config--general.el ends here
