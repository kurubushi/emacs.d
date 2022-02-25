;;; config--lsp-haskell.el --- Configuration of lsp-haskell.

;;; Commentary:

;;; Code:

(require 'use-package)
(require 'quelpa-use-package)

(use-package lsp-haskell
  :quelpa
  :after (config--haskell-mode config--lsp-mode)
  :hook
  (;; [Haskell] install hls
   ;; $ ghcup install hls
   (haskell-mode . lsp))
  :init
  (add-to-list 'exec-path (concat (getenv "HOME") "/.ghcup/bin")))

(provide 'config--lsp-haskell)

;;; config--lsp-haskell.el ends here
