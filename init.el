;;; init.el --- Emacs configuration.

;;; Commentary:

;; A configuration entry point.

;;; Code:

;;; Constants

(when load-file-name
  (setq user-emacs-directory (file-name-directory load-file-name)))

;; Avoid Error:Invalid image type: 'svg' on macOS
;; ref. https://debbugs.gnu.org/cgi/bugreport.cgi?bug=59081
(when (eq system-type 'darwin)
  (setq inhibit-splash-screen t))

;;; Repositories

(require 'package)

(setq package-archives
      '(("gnu" . "https://elpa.gnu.org/packages/")
        ("melpa" . "https://melpa.org/packages/")))

(package-initialize)
(package-refresh-contents)
(package-install 'gnu-elpa-keyring-update)

;; Uncomment to avoid `bad-signature' temporarily
;; (setq package-check-signature nil)

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
(setq use-package-compute-statistics t)

(quelpa '(quelpa-use-package
          :fetcher git
          :url "https://github.com/quelpa/quelpa-use-package.git"))
(require 'quelpa-use-package)

;; https://github.com/quelpa/quelpa/pull/232
(quelpa 'package-build)
(require 'package-build)
(custom-set-variables '(quelpa-build-default-files-spec
                        package-build-default-files-spec))

;;; Load configurations

(eval-when-compile
  (add-to-list 'load-path (concat user-emacs-directory "utils"))
  (add-to-list 'load-path (concat user-emacs-directory "config"))
  (add-to-list 'load-path (concat user-emacs-directory "custom")))

;;; Device-specific configuration

(use-package pre-config
  :if (file-exists-p (concat user-emacs-directory "custom/pre-config.el")))

;;; Utilities

(use-package utils--sequence)
(use-package utils--find-file)
(use-package utils--buffer)

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
(use-package config--parentheses)

;;; view

(use-package config--all-the-icons)
(use-package config--nerd-icons)
(use-package config--emojify)
(use-package config--nyan-mode)
(use-package config--doom-themes)
(use-package config--doom-modeline)

;;; etc

(use-package config--which-key)
(use-package config--flycheck)
(use-package config--shell-pop)
(use-package config--org-mode)
(use-package config--clipboard)

;;; LSP

(use-package config--lsp-mode)
(use-package config--lsp-ui)

;;; Prettier

(use-package config--prettier)

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

;;; Slim

(use-package config--slim-mode)

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

;;; CSV

(use-package config--csv-mode)

;;; PlantUML

(use-package config--plantuml-mode)

;;; fish shell

(use-package config--fish-mode)

;;; Copilot

(use-package config--copilot)

;;; Device-specific configuration

(use-package post-config
  :if (file-exists-p (concat user-emacs-directory "custom/post-config.el")))

;; Local Variables:
;; byte-compile-warnings: (not cl-functions obsolete)
;; End:

;;; init.el ends here
