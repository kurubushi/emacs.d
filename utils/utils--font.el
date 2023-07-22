;;; utils--font.el --- Utilities about fonts.

;;; Commentary:

;;; Code:

(require 's)

(defun installed-font-families ()
  "Get installed font families."
  (let* ((result (shell-command-to-string "fc-list"))
         (lines (split-string result "\n"))
         (drop-back-slash (lambda (str) (replace-regexp-in-string "\\\\" "" str)))
         (get-family (lambda (line)
                       (pcase (split-string line ":")
                         (`(,path ,family ,properties)
                          ;; `line' may have back slashes:
                          ;; all-the-icons.ttf: all\-the\-icons:style=Regular
                          (funcall drop-back-slash (s-trim family)))))))
    (mapcar get-family lines)))

(defun installed-font-family-p (font-family)
  "Return nil if FONT-FAMILY is not installed."
  ;; `find-font' also finds font:
  ;;   (find-font (font-spec :family "Cica"))
  ;; But it does not work before making frame (e.g. CLI mode,  daemon mode).
  (member font-family (installed-font-families)))

;;; provide

(provide 'utils--font)

;;; utils--font.el ends here
