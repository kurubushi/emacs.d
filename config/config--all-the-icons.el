;;; config--all-the-icons.el --- Configuration of all-the-icons.

;;; Commentary:

;;; Code:

(require 'use-package)
(require 'quelpa-use-package)

(use-package all-the-icons
  :quelpa
  :config
  (let* ((get-name    (lambda (f) (funcall (all-the-icons--family-name f))))
         (installed-p (lambda (f) (find-font (font-spec :name (funcall get-name f))))))
    (unless (and (mapcar installed-p all-the-icons-font-families))
      (all-the-icons-install-fonts t))))

(provide 'config--all-the-icons)

;;; config--all-the-icons.el ends here
