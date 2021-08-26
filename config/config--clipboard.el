;;; config--clipboard.el --- Configuration of clipboard.

;;; Commentary:

;;; Code:

(eval-when-compile
  (add-to-list 'load-path (concat user-emacs-directory "el-get/use-package")))

(require 'use-package)


;;; use-package

(defcustom clipboard-nc-send-port "8001"
  "Port to send the kill ring")
(defcustom clipboard-nc-recv-port "8002"
  "Port to receive the clipboard")

(defun send-to-nc (str)
  ;; remote$ while :; do nc -l 8001 | pbcopy; done
  (let* ((process-connection-type nil)
         (proc (start-process "send-to-nc" nil "nc" "-N" "localhost" clipboard-nc-send-port)))
    (process-send-string proc str)
    (process-send-eof proc)))

(defun recv-from-nc ()
  ;; remote$ ncat -l 8002 --exec (which pbpaste)
  (with-temp-buffer
    (call-process "nc" nil t nil "localhost" clipboard-nc-recv-port)
    (buffer-string)))

(setq interprogram-cut-function
      (lambda (text &optional push) (send-to-nc text)))

(setq interprogram-paste-function 'recv-from-nc)


(provide 'config--clipboard)


;;; config--clipboard.el ends here
