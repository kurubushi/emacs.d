;;; config--ivy.el --- Configuration of ivy/counsel.

;;; Commentary:

;;; Code:

(require 'use-package)
(require 'quelpa-use-package)
(require 'general)

;; https://takaxp.github.io/articles/qiita-helm2ivy.html

(use-package swiper
  :quelpa)

(use-package counsel ; requires ivy and swiper
  :quelpa
  :after utils--buffer
  :functions (;; utils--buffer
              with-killing-mru-file-buffer)

  :config
  (setf (alist-get t ivy-re-builders-alist) 'ivy--regex-ignore-order) ; 絞り込み方法
  (setf (alist-get 'counsel-M-x ivy-initial-inputs-alist) "") ; 絞り込み文字プリセット

  ;; hooks

  (defvar after-ivy-switch-buffe-hook nil)

  (defun after-ivy-switch-buffer-advice (&rest args)
    "Advice of `ivy-switch-buffer' with ARGS."
    (apply 'run-hook-with-args 'after-ivy-switch-buffer-hook args))

  (advice-add 'ivy-switch-buffer :after 'after-ivy-switch-buffer-advice)

  (defun ivy-done-with-killing-mru-file-buffer (&rest args)
    "Execute 'ivy-done' with killing the most recently buffer for file.
ARGS are parameters for 'ivy-done'."
    (interactive)
    (with-killing-mru-file-buffer (apply 'ivy-done args)))

  ;; Ignore *-ed buffers (i.e. *Messages*).
  ;; `ivy-toggle-ignore' (C-c C-a) shows ignored buffers.
  ;; (add-to-list 'ivy-ignore-buffers "\\*.*\\*\\'")

  ;; switch-mode
  (ivy-mode 1)
  (counsel-mode 1)

  :general (general-define-key :keymaps '(normal visual)
                               :prefix "SPC"
                               "SPC" 'counsel-M-x)
           (general-define-key :keymaps 'normal
                               :prefix "SPC b"
                               "b" 'counsel-switch-buffer)
           (general-define-key :keymaps 'normal
                               :prefix "SPC f"
                               "f" 'counsel-find-file
                               "r" 'counsel-recentf)
           (general-define-key :keymaps 'normal
                               :prefix "SPC g"
                               "g" 'counsel-git-grep)
           (general-define-key :keymaps 'ivy-minibuffer-map
                               "RET" 'ivy-done
                               "C-<return>" 'ivy-done-with-killing-mru-file-buffer))

(provide 'config--ivy)

;;; config--ivy.el ends here
