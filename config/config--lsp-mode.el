;;; config--lsp-mode.el --- Configuration of lsp-mode.

;;; Commentary:

;;; Code:

(require 'use-package)
(require 'quelpa-use-package)

;; If you want use other lsp server on each project, create .dir-locals.el:
;;
;; ((ruby-mode . ((lsp-enabled-clients . (ruby-syntax-tree-ls))
;;                (lsp-enabled-format-on-save . t))))
;;

;;; Haskell

;; Install ghc, cabal, and hls to use an lsp server.
;; $ ghcup install ghc
;; $ ghcup install cabal
;; $ ghcup install hls

(use-package lsp-haskell
  :quelpa
  :after lsp-mode
  :init
  (add-to-list 'exec-path (concat (getenv "HOME") "/.ghcup/bin")))

;;; Ruby

;; Install solargraph to use an lsp server.
;; $ gem install --user solargraph

;; Provide lsp-client `ruby-syntax-tree-ls'.
(use-package lsp-ruby-syntax-tree
  :after lsp-mode)

;;; Go

;; Install gopls to use an lsp server:
;; $ go get golang.org/x/tools/gopls

;;; TypeScript

;; Install typescript-language-server and typescript to use lsp server:
;; $ npm install -g typescript-language-server typescript
;; ref. https://emacs-lsp.github.io/lsp-mode/page/lsp-typescript/

;;; Rust

;; Install binaries via `rustup'.
;; $ curl --proto '=https' --tlsv1.2 -sSf https://sh.rustup.rs | sh

;;; use-package

(use-package lsp-mode
  :quelpa
  :commands lsp

  :init
  ;; define functions for backward compatibility
  (defun lsp--sort-completions (completions)
  (lsp-completion--sort-completions completions))

  (defun lsp--annotate (item)
    (lsp-completion--annotate item))

  (defun lsp--resolve-completion (item)
    (lsp-completion--resolve item))

  :config
  (defun lsp-restart-if-running ()
    "Restart lsp-mode if lsp-mode is running."
    (when lsp-mode
      (lsp-disconnect)
      (lsp)))

  (defvar lsp-enabled-format-on-save nil
    "If non-nil, lsp-mode formats buffer on save.")

  (defun lsp-format-buffer-on-save ()
    "Format the current buffer if `lsp-enabled-format-on-save' is not `nil'."
    (when lsp-enabled-format-on-save
       (lsp-format-buffer)))

  :hook
  ((haskell-mode . lsp)
   (ruby-mode    . lsp)
   (go-mode      . lsp)
   (web-mode     . lsp)
   (rust-mode    . lsp)

   ;; Restart lsp-mode when .dir-locals.el exists.
   (hack-local-variables . lsp-restart-if-running)

   ;; Format buffer on save.
   (before-save . lsp-format-buffer-on-save)))

(provide 'config--lsp-mode)

;;; config--lsp-mode.el ends here
