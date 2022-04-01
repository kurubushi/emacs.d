;;; config--auto-spacing.el --- Configuration of auto-spacing.

;;; Commentary:

;;; Code:

(require 'use-package)
(require 'quelpa-use-package)

(use-package auto-spacing
  :quelpa (auto-spacing :fetcher github :repo "kurubushi/auto-spacing")
  :custom
  ;; where to insert a space
  (auto-spacing-english-regexp (rx (in "a-zA-Z0-9$.`")))
  ;; add insert functions
  (auto-spacing-self-insert-command-list '(skk-insert
                                           TeX-insert-dollar)))

(provide 'config--auto-spacing)

;;; config--auto-spacing.el ends here
