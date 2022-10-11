;;; config--lsp-mode.el --- Configuration of lsp-mode.

;;; Commentary:

;;; Code:

(require 'use-package)
(require 'quelpa-use-package)

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

  :hook ((haskell-mode . lsp)
         (ruby-mode    . lsp)
         (go-mode      . lsp)
         (web-mode     . lsp)
         (rust-mode    . lsp)))

(provide 'config--lsp-mode)

;;; config--lsp-mode.el ends here
