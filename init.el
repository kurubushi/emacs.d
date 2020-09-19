;;; init.el --- Emacs configuration.

;;; Commentary:

;; A configuration entry point.

;;; Code:

(package-initialize)

(when load-file-name
  (setq user-emacs-directory (file-name-directory load-file-name)))

(eval-when-compile
  (add-to-list 'load-path (concat user-emacs-directory "el-get/el-get"))
  (add-to-list 'load-path (concat user-emacs-directory "el-get/use-package"))
  (add-to-list 'load-path (concat user-emacs-directory "config"))
  (add-to-list 'load-path (concat user-emacs-directory "custom")))


;;; El-Get

;; https://github.com/dimitri/el-get

(unless (require 'el-get nil t)
  (with-current-buffer
      (url-retrieve-synchronously
       "https://raw.github.com/dimitri/el-get/master/el-get-install.el")
      (goto-char (point-max))
      (eval-print-last-sexp)))

(require 'el-get) ; avoids Flycheck errors

(defvar el-get--directory (concat user-emacs-directory "el-get/"))

(add-to-list 'el-get-recipe-path (concat user-emacs-directory "el-get-recipes"))


;;; use-package

;; https://github.com/jwiegley/use-package

(declare-function el-get-bundle-el-get (concat el-get--directory "el-get/el-get-bundle.el"))

(el-get-bundle! use-package)
(el-get-bundle! use-package--el-get)

(require 'use-package) ; avoids Flycheck errors


;;; Emacs

(use-package config--emacs)
(use-package config--exec-path-from-shell)
(use-package config--general) ; provides :general keyword


;;; file/buffer

(use-package config--recentf)
(use-package config--neotree)

;; ivy + persp-mode
(use-package config--ivy)
(use-package config--persp-mode)

;; helm + perspeen
;;(use-package config--helm)
;;(use-package config--perspeen)
;;(use-package config--helm-perspeen)


;;; edit

(use-package config--evil)
(use-package config--evil-collection)
(use-package config--company)
(use-package config--skk)
(use-package config--auto-spacing)


;;; view

(use-package config--all-the-icons)
(use-package config--nyan-mode)
(use-package config--doom-themes)
(use-package config--doom-modeline)


;;; etc

(use-package config--which-key)
(use-package config--flycheck)
(use-package config--shell-pop)


;;; Git

(use-package config--magit)
(use-package config--evil-magit)
(use-package config--git-gutter)
;;(use-package config--helm-git-grep)


;;; Markdown

(use-package config--markdown-mode)


;;; Haskell

(use-package config--haskell-mode)


;;; Scala

(use-package config--scala-mode)


;;; OCaml

(use-package config--tuareg)
(use-package config--merlin)


;;; TeX

(use-package config--tex-site)


;;; Go

(use-package config--go-mode)


;;; Ruby

(use-package config--ruby-mode)
(use-package config--rubocop)


;;; Node

(use-package config--web-mode)


;;; TypeScript

(use-package config--tide)


;;; Terraform

(use-package config--terraform-mode)


;;; Device-specific configuration

(use-package post-config) ; sample: custom/post-config.sample.el


;; Local Variables:
;; byte-compile-warnings: (not cl-functions obsolete)
;; End:


;;; init.el ends here
