;;; utils--buffer.el --- Utilities about buffers.

;;; Commentary:

;;; Code:

;;; Hooks of `find-file'.

(defvar before-find-file-hook nil
  "Functions to run before 'find-file'.")
(defvar after-find-file-hook nil
  "Functions to run after 'find-file'.")

(defun before-find-file-advice (&rest args)
  "Advice of `find-file' with ARGS."
  (apply 'run-hook-with-args 'before-find-file-hook args))

(defun after-find-file-advice (&rest args)
  "Advice of `find-file' with ARGS."
  (apply 'run-hook-with-args 'after-find-file-hook args))

(advice-add 'find-file :before 'before-find-file-advice)
(advice-add 'find-file :after 'after-find-file-advice)

(advice-add 'find-file-noselect :before 'before-find-file-advice)
(advice-add 'find-file-noselect :after 'after-find-file-advice)

;;; provide

(provide 'utils--find-file)

;;; utils--find-file.el ends here
