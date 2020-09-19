;;; config--ivy.el --- Configuration of ivy/counsel.

;;; Commentary:

;;; Code:

(eval-when-compile
  (add-to-list 'load-path (concat user-emacs-directory "el-get/use-package")))

(require 'use-package)


;;; use-package

;; ref. https://takaxp.github.io/articles/qiita-helm2ivy.html

(use-package counsel ; requires ivy and swiper
  :el-get swiper

  :config
  (setf (alist-get t ivy-re-builders-alist) 'ivy--regex-ignore-order) ; 絞り込み方法
  (setf (alist-get 'counsel-M-x ivy-initial-inputs-alist) "") ; 絞り込み文字プリセット
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
   "g" 'counsel-gitgrep))


(provide 'config--ivy)


;;; config--ivy.el ends here
