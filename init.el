;;; init.el --- Personal configuration.

;;; Commentary:

;; A configuration entry point.

;;; Code:

(package-initialize)

(when load-file-name
  (setq user-emacs-directory (file-name-directory load-file-name)))

(eval-when-compile
  ;; Packages are stored at ~/.emacs.d/el-get.
  (add-to-list 'load-path (concat user-emacs-directory "el-get/el-get")))

(require 'el-get)


;;; Variables

(defvar el-get--directory (concat user-emacs-directory "el-get/"))


;;; El-Get

;; https://github.com/dimitri/el-get

(unless (require 'el-get nil t)
  (with-current-buffer
      (url-retrieve-synchronously
       "https://raw.github.com/dimitri/el-get/master/el-get-install.el")
      (goto-char (point-max))
      (eval-print-last-sexp)))

(add-to-list 'el-get-recipe-path (concat user-emacs-directory "el-get-recipes"))


;;; use-package

;; https://github.com/jwiegley/use-package

(declare-function el-get-bundle-el-get (concat el-get--directory "el-get/el-get-bundle.el"))

(el-get-bundle! use-package)
(el-get-bundle! use-package--el-get)


;;; Configurations with use-package

;; example: loader.el.sample

(let ((file-name (concat user-emacs-directory "loader.el")))
  (when (file-exists-p file-name)
    (load file-name)))


;; Local Variables:
;; byte-compile-warnings: (not cl-functions obsolete)
;; End:


;;; init.el ends here
