;;; config--markdown-mode.el --- Configuration of markdown-mode.

;;; Commentary:

;;; Code:

(eval-when-compile
  (add-to-list 'load-path (concat user-emacs-directory "el-get/use-package")))

(require 'use-package)


;;; use-package

(use-package markdown-mode
  :quelpa markdown-mode
  :mode (("\\.md\\'" . markdown-mode))
  :custom
  (markdown-header-scaling t)
  (markdown-header-scaling-values '(1.6 1.4 1.2 1.0 1.0 1.0))
  (markdown-hide-urls t)
  (markdown-make-gfm-checkboxes-buttons t)
  (markdown-enable-math t)
  (markdown-display-remote-images t))


(provide 'config--markdown-mode)


;;; config--markdown-mode.el ends here
