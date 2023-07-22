;;; config--all-the-icons.el --- Configuration of all-the-icons.

;;; Commentary:

;;; Code:

(require 'use-package)
(require 'quelpa-use-package)
(require 'cl-lib)
(require 'utils--font)

(use-package all-the-icons
  :quelpa
  :config
  (let* ((get-family  (lambda (f) (funcall (all-the-icons--family-name f))))
         (installed-p (lambda (f) (installed-font-family-p (funcall get-family f)))))
    (when (cl-remove-if installed-p all-the-icons-font-families)
      (all-the-icons-install-fonts t))))

(provide 'config--all-the-icons)

;;; config--all-the-icons.el ends here
