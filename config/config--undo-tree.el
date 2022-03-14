;;; config--undo-tree.el --- Configuration of undo-tree.

;;; Commentary:

;;; Code:

(require 'use-package)
(require 'quelpa-use-package)

(use-package undo-tree
  :quelpa (undo-tree :fetcher git
		     :url "git@gitlab.com:tsc25/undo-tree.git")
  :config (global-undo-tree-mode))

(provide 'config--undo-tree)

;;; config--undo-tree.el ends here
