;;; config--undo-tree.el --- Configuration of undo-tree.

;;; Commentary:

;;; Code:

(require 'use-package)
(require 'quelpa-use-package)

(defvar undo-tree-directory
  (concat user-emacs-directory "undo-tree/")
  "Directory for undo-tree.")

(defvar undo-tree-history-directory
  (concat undo-tree-directory "histories/")
  "Directory for undo-tree histories.")

(use-package undo-tree
  :custom (undo-tree-history-directory-alist `(("." . ,undo-tree-history-directory)))
  :quelpa (undo-tree :fetcher git
		     :url "git@github.com:emacsmirror/undo-tree.git")
  :config (global-undo-tree-mode))

(provide 'config--undo-tree)

;;; config--undo-tree.el ends here
