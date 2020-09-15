;;; config--tex-site.el --- Configuration of tex-site.

;;; Commentary:

;;; Code:

(eval-when-compile
  (add-to-list 'load-path (concat user-emacs-directory "el-get/use-package")))

(require 'use-package)


;;; use-package

(use-package tex-site
  :el-get auctex
  :if (locate-file "pdftex" exec-path)
  :mode (("\\.tex\\'" . LaTeX-mode))
  :config
  (setq TeX-PDF-mode t)
  ;; enable synctex
  (setq TeX-source-correlate-method 'synctex)
  (setq TeX-source-correlate-start-server t)
  (setq TeX-source-correlate-mode t)
  (add-hook 'LaTeX-mode-hook 'pdf-sync-minor-mode)
  
  ;; C-c C-c commands
  (add-hook 'LaTeX-mode-hook
            (function (lambda ()
                        (add-to-list 'TeX-command-list
                                     '("MyLaTeX" "%`%l%(mode)%' %t"
                                       TeX-run-TeX nil
                                       (latex-mode doctex-mode) :help "Run MyLaTeX"))
                        (add-to-list 'TeX-command-list
                                     '("latexmk"
                                       "latexmk -pdfdvi %t"
                                       TeX-run-TeX nil
                                       (latex-mode doctex-mode) :help "Run latexmk"))
                        (add-to-list 'TeX-command-list
                                     '("Evince"
                                       "TeX-evince-sync-view"
                                       TeX-run-discard-or-function t t :help "Forward search with Evince")))))
  (setq TeX-command-default "latexmk")
  
  ;; to use pdfview with auctex
  ;; (setq TeX-view-program-selection '((output-pdf "pdf-tools")))
  ;; (setq TeX-view-program-list '(("pdf-tools" "TeX-pdf-tools-sync-view")))
  (setq TeX-view-program-selection '((output-pdf "Evince")))
  ;; to revert viewed pdf after compilation
  ;; given argment is "hoge.dvi" but a buffer we want revert is PDF's.
  ;; TeX-revert-PDF-buffer reverts PDF's buffer
  (defun TeX-revert-PDF-buffer (file)
    (message (concat "TeX revert from " file))
    (TeX-revert-document-buffer (replace-regexp-in-string "[^.]*$" "pdf" file)))
  (add-hook 'TeX-after-compilation-finished-functions 'TeX-revert-PDF-buffer))


(provide 'config--tex-site)


;;; config--tex-site.el ends here
