;;; config--flyspell.el --- Configuration of flyspell.

;;; Commentary:

;;; Code:

(require 'use-package)
(require 'quelpa-use-package)

(use-package flyspell
  :quelpa
  :hook
  (text-mode . flyspell-mode)
  (prog-mode . flyspell-prog-mode))

(provide 'config--flyspell)

;;; config--flyspell.el ends here
