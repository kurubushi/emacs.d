;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; standerd.el
;;----------
;; write standerd configures in it.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; PDF
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(el-get-bundle pdf-tools) ; requires automake and proppler.
(use-package pdf-tools
  :init
  (pdf-tools-install)
  :config
  ;; keymap depends on evil-collection
  (evil-define-key-escape-spc 'pdf-view-mode-map)
  (add-to-list 'auto-mode-alist '("\\.pdf\\'" . pdf-view-mode))
  (add-hook 'pdf-view-mode-hook #'pdf-sync-minor-mode))




;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; markdown
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(el-get-bundle markdown-mode)
(use-package markdown-mode)




;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; haskell
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(el-get-bundle haskell-mode)
(use-package haskell-mode)

;;(el-get-bundle intero)
;;(use-package intero
;;  :config
;;  (add-hook 'haskell-mode-hook 'intero-mode))




;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; scala
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(el-get-bundle scala-mode)
(use-package scala-mode)




;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; ocaml
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(el-get-bundle tuareg-mode)
(use-package tuareg-mode
  :config
  (add-hook 'tuareg-mode-hook 'tuareg-imenu-set-imenu)
  (setq auto-mode-alist
    (append '(("\\.ml[ily]?$" . tuareg-mode)
              ("\\.topml$" . tuareg-mode))
            auto-mode-alist))
  (autoload 'utop-setup-ocaml-buffer "utop" "Toplevel for OCaml" t)
  (add-hook 'tuareg-mode-hook 'utop-setup-ocaml-buffer))

(el-get-bundle merlin)
(use-package merlin
  :config
  (add-hook 'tuareg-mode-hook 'merlin-mode)
  (setq merlin-use-auto-complete-mode t)
  (setq merlin-error-after-save nil))




;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; mgit
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(el-get-bundle magit-popup)
(el-get-bundle magit)
(use-package magit
  :config
  (general-define-key :keymaps '(normal)
                      :prefix "SPC"
                      "gs" 'magit-status
                      "gl" 'magit-log-popup))

(el-get-bundle evil-magit)
(use-package evil-magit)




;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; tex
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(el-get-bundle auctex)
(use-package tex-site
  :config
  (setq TeX-PDF-mode t)

  ;; enable synctex
  (setq TeX-source-correlate-method 'synctex)
  (setq TeX-source-correlate-start-server t)
  (setq TeX-source-correlate-mode t)
  (add-hook 'TeX-mode-hook 'pdf-sync-minor-mode)

  ;; C-c C-c commands
  (add-hook 'TeX-mode-hook
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
  (setq TeX-view-program-selection '((output-pdf "pdf-tools")))
  (setq TeX-view-program-list '(("pdf-tools" "TeX-pdf-tools-sync-view")))
  ;; to revert viewed pdf after compilation
  ;; given argment is "hoge.dvi" but a buffer we want revert is PDF's.
  ;; TeX-revert-PDF-buffer reverts PDF's buffer
  (defun TeX-revert-PDF-buffer (file)
    (message (concat "TeX revert from " file))
    (TeX-revert-document-buffer (replace-regexp-in-string "[^.]*$" "pdf" file)))
  (add-hook 'TeX-after-compilation-finished-functions 'TeX-revert-PDF-buffer))

;; auctex by zico
;;(use-package tex-site
;;  :config
;;  (require 'tex-jp)
;;  (defun replace-dot-comma ()
;;    "s/。/．/g; s/、/，/g;する"
;;    (interactive)
;;    (let ((curpos (point)))
;;      (goto-char (point-min))
;;      (while (search-forward "。" nil t) (replace-match "．"))
;;      (goto-char (point-min))
;;      (while (search-forward "、" nil t) (replace-match "，"))
;;      (goto-char curpos)
;;      ))
;;  (setq TeX-view-program-selection '((output-pdf "Evince")))
;;  (setq TeX-source-correlate-method 'synctex)
;;  (setq TeX-source-correlate-start-server t)
;;  (setq TeX-source-correlate-mode t)
;;  (setq TeX-default-mode 'japanese-latex-mode)
;;  (setq TeX-engine-alist '((platex "pLaTeX" "platex -shell-escape %S" "platex -shell-escape %S" "")
;;                           (pdflatex "PDFLaTeX" "pdflatex -shell-escape -synctex=1 %S" "pdflatex -shell-escape -synctex=1 %S" "")))
;;  (setq TeX-PDF-mode t)
;;  (setq japanese-TeX-engine-default 'platex)
;;  (setq-default TeX-engine 'platex)
;;  (setq japanese-TeX-command-default "pTeX")
;;  (setq-default TeX-master nil) ; Query for master file.
;;  (setq japanese-LaTeX-command-default "pLaTeX")
;;  (setq LaTeX-parse-self t)
;;  (setq LaTeX-auto-save  t)
;;  (add-hook 'LaTeX-mode-hook
;;            (function (lambda ()
;;                        ;(add-to-list 'TeX-command-list
;;                        ;             '("Evince" "evince '%s.pdf' " TeX-run-command t nil))
;;                        (add-to-list 'TeX-command-list
;;                                     '("Pdf" "dvipdfmx -V 4 '%s' " TeX-run-command t nil))
;;                        (add-to-list 'TeX-command-list
;;                                     '("Evince"
;;                                        ;"synctex view -i \"%n:0:%b\" -o %s.pdf -x \"evince -i %%{page+1} %%{output}\""
;;                                       "TeX-evince-sync-view"
;;                                       TeX-run-discard-or-function t t :help "Forward search with Evince"))
;;                        (add-hook 'before-save-hook 'replace-dot-comma nil 'make-it-local)))))
