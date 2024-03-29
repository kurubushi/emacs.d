;;; config--exec-path-from-shell.el --- Configuration of exec-path-from-shell.

;;; Commentary:

;;; Code:

(require 'use-package)
(require 'quelpa-use-package)

(use-package exec-path-from-shell
  :quelpa
  :custom
  (exec-path-from-shell-variables '("PATH"
                                    "LD_LIBRARY_PATH"))
  :config
  (exec-path-from-shell-initialize)
  (setenv "LANG" "ja_JP.UTF-8")) ; to pass "flycheck invalid multibyte char"

(provide 'config--exec-path-from-shell)

;;; config--exec-path-from-shell.el ends here
