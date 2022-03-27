;;; config--csv-mode.el --- Configuration of csv-mode.

;;; Commentary:

;;; Code:

(require 'use-package)
(require 'quelpa-use-package)
(require 'general)

(use-package csv-mode
  :quelpa (csv-mode :fetcher git
                    :url "git@github.com:emacsmirror/csv-mode.git")
  :mode ("\\.csv\\'" . csv-align-mode))

(provide 'config--csv-mode)

;;; config--csv-mode.el ends here
