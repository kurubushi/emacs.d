;;; config--web-mode.el --- Configuration of web-mode.

;;; Commentary:

;;; Code:

(require 'use-package)
(require 'quelpa-use-package)

(use-package web-mode
  :quelpa
  :after flycheck
  :custom
  (web-mode-markup-indent-offset 2)
  (web-mode-css-indent-offset 2)
  (web-mode-code-indent-offset 2)

  :mode (("\\.js\\'" . web-mode)
         ("\\.ts\\'" . web-mode)
         ("\\.tsx\\'" . web-mode))

  :config
  (flycheck-add-mode 'javascript-eslint 'web-mode))

(provide 'config--web-mode)

;;; config--web-mode.el ends here
