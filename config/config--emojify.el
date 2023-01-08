;;; config--emojify.el --- Configuration of emojify.

;;; Commentary:

;;; Code:

(require 'use-package)
(require 'quelpa-use-package)

;; https://github.com/iqbalansari/emacs-emojify
(use-package emojify
  :quelpa
  :hook (after-init . global-emojify-mode)
  :custom (emojify-display-style 'unicode)
  :config (emojify-set-emoji-styles '(github)))

(provide 'config--emojify)

;;; config--emojify.el ends here
