;;; config--haskell-mode.el --- Configuration of haskell-mode.

;;; Commentary:

;;; Code:

(require 'use-package)
(require 'quelpa-use-package)

;; Install ghc, cabal, and hls to use an lsp server.
;; $ ghcup install ghc
;; $ ghcup install cabal
;; $ ghcup install hls

(use-package haskell-mode
  :quelpa
  :after lsp-mode
  :mode (("\\.hs\\'" . haskell-mode))
  :hook (haskell-mode . lsp))

(provide 'config--haskell-mode)

;;; config--haskell-mode.el ends here
