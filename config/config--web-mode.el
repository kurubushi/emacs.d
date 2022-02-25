;;; config--web-mode.el --- Configuration of web-mode.

;;; Commentary:

;;; Code:

(require 'use-package)
(require 'quelpa-use-package)

(use-package add-node-modules-path
  :quelpa)

(use-package prettier-js
  :quelpa)

(use-package web-mode
  :quelpa
  :after config--flycheck
  :mode (("\\.js\\'" . web-mode)
         ("\\.ts\\'" . web-mode)
         ("\\.tsx\\'" . web-mode))
  :init
  (setq web-mode-markup-indent-offset 2)
  (setq web-mode-css-indent-offset 2)
  (setq web-mode-code-indent-offset 2)
  :config
  (flycheck-add-mode 'javascript-eslint 'web-mode)
  (add-hook 'web-mode-hook 'add-node-modules-path)
  (add-hook 'web-mode-hook 'prettier-js-mode))

(provide 'config--web-mode)

;;; config--web-mode.el ends here
