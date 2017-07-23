;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; package.el
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(package-initialize)




;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; el-get
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(when load-file-name
  (setq user-emacs-directory (file-name-directory load-file-name)))

(add-to-list 'load-path "~/.emacs.d/el-get/el-get")

(unless (require 'el-get nil 'noerror)
  (with-current-buffer
      (url-retrieve-synchronously
       "https://raw.githubusercontent.com/dimitri/el-get/master/el-get-install.el")
    (goto-char (point-max))
    (eval-print-last-sexp)))

;(el-get 'sync)




;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; use-package
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(el-get-bundle! use-package)




;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; init-loader
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; load local config
(el-get-bundle! init-loader
  (setq init-loader-show-log-after-init t)
  (init-loader-load "~/.emacs.d/rc.d"))

;EOF
