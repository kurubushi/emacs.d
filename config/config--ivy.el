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

  :config
  (setf (alist-get t ivy-re-builders-alist) 'ivy--regex-ignore-order) ; 絞り込み方法
  (setf (alist-get 'counsel-M-x ivy-initial-inputs-alist) "") ; 絞り込み文字プリセット

  ;; Ignore *-ed buffers (i.e. *Messages*).
  ;; `ivy-toggle-ignore' (C-c C-a) shows ignored buffers.
  ;; (add-to-list 'ivy-ignore-buffers "\\*.*\\*\\'")

  ;; switch-mode

  (ivy-mode 1)
  (counsel-mode 1)

  :general
  (general-define-key ; for executer
   :keymaps '(normal visual)
   :prefix "SPC"
   "SPC" 'counsel-M-x)

  (general-define-key ; for buffers
   :keymaps 'normal
   :prefix "SPC b"
   "b" 'counsel-switch-buffer)

  (general-define-key ; for files
   :keymaps 'normal
   :prefix "SPC f"
   "f" 'counsel-find-file
   "r" 'counsel-recentf)

  (general-define-key ; for Git
   :keymaps 'normal
   :prefix "SPC g"
   "g" 'counsel-git-grep))

(provide 'config--ivy)

;;; config--ivy.el ends here
