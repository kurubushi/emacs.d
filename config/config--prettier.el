;;; config--prettier.el --- Configuration of prettier.

;;; Commentary:

;;; Code:

(require 'use-package)
(require 'quelpa-use-package)

;; Install Prettier:
;; $ npm install -g prettier

(use-package prettier
  :quelpa prettier
  :hook ((web-mode . prettier-mode)))

(provide 'config--prettier)

;;; config--prettier.el ends here
