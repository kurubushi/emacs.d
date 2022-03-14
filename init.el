;;; init.el --- Emacs configuration.

;;; Commentary:

;; A configuration entry point.

;;; Code:

;;; Constants

(when load-file-name
  (setq user-emacs-directory (file-name-directory load-file-name)))

;;; Repositories

(require 'package)

(setq package-archives
      '(("gnu" . "https://elpa.gnu.org/packages/")
        ("melpa" . "https://melpa.org/packages/")))

(package-initialize)
(package-refresh-contents)
(package-install 'gnu-elpa-keyring-update)

;;; quelpa

;; https://github.com/quelpa/quelpa

(unless (package-installed-p 'quelpa)
  (with-temp-buffer
    (url-insert-file-contents
     "https://raw.githubusercontent.com/quelpa/quelpa/master/quelpa.el")
    (eval-buffer)
    (quelpa-self-upgrade)))

(require 'quelpa)

;;; use-package

;; https://github.com/jwiegley/use-package

(quelpa 'use-package)
(require 'use-package)

(quelpa '(quelpa-use-package
          :fetcher git
          :url "https://github.com/quelpa/quelpa-use-package.git"))
(require 'quelpa-use-package)

;;; Load configurations

(eval-when-compile
  (add-to-list 'load-path (concat user-emacs-directory "config"))
  (add-to-list 'load-path (concat user-emacs-directory "custom")))

;;; Device-specific configuration

(use-package pre-config) ; sample: custom/pre-config.sample.el

;;; Emacs

(use-package config--emacs)
(use-package config--exec-path-from-shell)
(use-package config--general) ; provides :general keyword

;;; file/buffer

(use-package config--recentf)
(use-package config--neotree)

;; ivy + persp-mode
(use-package config--ivy)
(use-package config--ivy-rich)
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
(use-package config--ispell)
(use-package config--flyspell)
(use-package config--undo-tree)

;;; view

(use-package config--all-the-icons)
(use-package config--emojify)
(use-package config--nyan-mode)
(use-package config--doom-themes)
(use-package config--doom-modeline)

;;; etc

(use-package config--which-key)
(use-package config--flycheck)
(use-package config--shell-pop)

;;; LSP

(use-package config--lsp-mode)
(use-package config--lsp-ui)

;;; Git

(use-package config--magit)
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

;;; Protocol Buffer

(use-package config--protobuf-mode)

;;; YAML

(use-package config--yaml-mode)

;;; JSON

(use-package config--json-mode)

;;; Dockerfile

(use-package config--dockerfile-mode)

;;; Device-specific configuration

(use-package post-config) ; sample: custom/post-config.sample.el

;; Local Variables:
;; byte-compile-warnings: (not cl-functions obsolete)
;; End:

;;; init.el ends here
