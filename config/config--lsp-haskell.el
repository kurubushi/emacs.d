;;; config--lsp-haskell.el --- Configuration of lsp-haskell.

;;; Commentary:

;;; Code:

(eval-when-compile
  (add-to-list 'load-path (concat user-emacs-directory "el-get/use-package")))

(require 'use-package)


;;; use-package

(use-package lsp-haskell
  :el-get lsp-haskell
  :after (config--haskell-mode config--lsp-mode)
  ;; depens on haskell-language-server(hls)
  ;; $ stack install hls
  :hook (haskell-mode . lsp))


(provide 'config--lsp-haskell)


;;; config--lsp-haskell.el ends here
