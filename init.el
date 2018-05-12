;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; package.el
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(package-initialize)




;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; el-get
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(when load-file-name
  (setq user-emacs-directory (file-name-directory load-file-name)))

(add-to-list 'load-path (concat user-emacs-directory "el-get/el-get"))

(unless (require 'el-get nil t)
  (with-current-buffer
      (url-retrieve-synchronously
       "https://raw.github.com/dimitri/el-get/master/el-get-install.el")
;;    (let (el-get-install-skip-emacswiki-recipes)
      (goto-char (point-max))
      (eval-print-last-sexp)))

(add-to-list 'el-get-recipe-path (concat user-emacs-directory "el-get-recipes"))
;;(el-get 'sync)




;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; use-package
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(el-get-bundle! use-package)




;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; init-loader
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; load local config
(el-get-bundle! init-loader
  (setq init-loader-show-log-after-init t)
  (init-loader-load (concat user-emacs-directory "rc.d")))

;;EOF
