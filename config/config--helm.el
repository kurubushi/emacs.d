;;; config--helm.el --- Configuration of Helm.

;;; Commentary:

;;; Code:

(eval-when-compile
  (add-to-list 'load-path (concat user-emacs-directory "el-get/use-package")))

(require 'use-package)


;;; Helm

(use-package helm-config
  :quelpa helm

  :custom
  (helm-boring-buffer-regexp-list '("\\` *\\*.*\\*\\'" "\\`magit: " "\\`magit-[^:]+:"))
  (helm-white-buffer-regexp-list '("\\`*scratch*" "\\`*Message*"))
  (helm-ff-keep-cached-candidates nil) ; disable cache

  :config
  (defun helm-kill-selected-buffer ()
    "Kill a selected buffer in helm-mini."
    (interactive)
    (with-helm-alive-p
      (with-helm-window
        (kill-buffer (helm-get-selection))
        (helm-delete-current-selection))))

  (general-define-key :keymaps '(normal visual)
                      :prefix "SPC"
                      "SPC" 'helm-M-x)
  (general-define-key :keymaps '(normal)
                      :prefix "SPC"
                      ;; file
                      "ff" 'helm-find-files
                      "fr" 'helm-recentf
                      ;; buffer
                      "bb" 'helm-mini)
  (general-define-key :keymaps 'helm-buffer-map
                      "C-k" 'helm-kill-selected-buffer)
  (helm-mode 1))


(provide 'config--helm)


;;; config--helm.el ends here
