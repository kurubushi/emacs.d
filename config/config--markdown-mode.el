;;; config--markdown-mode.el --- Configuration of markdown-mode.

;;; Commentary:

;;; Code:

(require 'use-package)
(require 'quelpa-use-package)

(use-package markdown-mode
  :quelpa
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
