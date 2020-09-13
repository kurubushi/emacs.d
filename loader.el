;;; loader.el --- Loads emacs lisp files.

;;; Commentary:

;; A List of `(load xx.el)'

;;; Code:

(defconst active-custom-elisp-files
  '("init.d/core.el"
    "init.d/standard.el"))

(let ((load-file (lambda (file-name)
                   (load (concat user-emacs-directory file-name)))))
  (mapc load-file active-custom-elisp-files))


;;; loader.el ends here
