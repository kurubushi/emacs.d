;;; config--exec-path-from-shell.el --- Configuration of exec-path-from-shell.

;;; Commentary:

;;; Code:

(eval-when-compile
  (add-to-list 'load-path (concat user-emacs-directory "el-get/use-package")))

(require 'use-package)


;;; exec-path-from-shell

(use-package exec-path-from-shell
  :el-get exec-path-from-shell
  :config
  (setq exec-path-from-shell-shell-name (getenv "ESHELL"))
  (setq exec-path-from-shell-variables
        '("PATH" "LD_LIBRARY_PATH"
          "SSH_AGENT_PID" "SSH_AUTH_SOCK")) ; ssh-agent
  (exec-path-from-shell-initialize)
  (setenv "LANG" "ja_JP.UTF-8")) ; to pass "flycheck invalid multibyte char"

(provide 'config--exec-path-from-shell)


;;; config--exec-path-from-shell.el ends here
