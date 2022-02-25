;;; config--json-mode.el --- Configuration of json-mode.

;;; Commentary:

;;; Code:

(require 'use-package)
(require 'quelpa-use-package)

(use-package json-mode
  :quelpa
  :custom (js-indent-level 2)
  :mode (("\\.json\\'" . json-mode)))

(provide 'config--json-mode)

;;; config--json-mode.el ends here
