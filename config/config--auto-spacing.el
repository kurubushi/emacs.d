;;; config--auto-spacing.el --- Configuration of auto-spacing.

;;; Commentary:

;;; Code:

(require 'use-package)
(require 'quelpa-use-package)

(use-package auto-spacing
  :quelpa (auto-spacing :fetcher github :repo "kurubushi/auto-spacing")
  :config
    ;; where to insert a space
    (setq auto-spacing-english-regexp (rx (in "a-zA-Z0-9$.`")))
    (setq auto-spacing-non-english-regexp (rx (category japanese)))
    (setq auto-spacing-non-english-exception-regexp (rx (in "。，．！？；：「」（）、")))
    ;; add insert functions
    (setq auto-spacing-self-insert-command-list
          '(skk-insert
            TeX-insert-dollar)))

(provide 'config--auto-spacing)

;;; config--auto-spacing.el ends here
