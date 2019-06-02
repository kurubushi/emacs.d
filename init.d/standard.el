;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; standard.el
;;----------
;; write standard configures in it.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; PDF
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(use-package pdf-tools
  :el-get pdf-tools ; requires automake and proppler.
  :mode (("\\.pdf\\'" . pdf-view-mode))
  :config
  (pdf-tools-install)
  ;; keymap depends on evil-collection
  (evil-define-key-escape-spc 'pdf-view-mode-map)
  (add-hook 'pdf-view-mode-hook 'pdf-sync-minor-mode))




;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; markdown
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(use-package markdown-mode
  :el-get markdown-mode
  :mode (("\\.md\\'" . markdown-mode)))




;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; haskell
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(use-package haskell-mode
  :el-get haskell-mode
  :mode (("\\.hs\\'" . haskell-mode)))

;;(use-package intero
;;  :el-get intero
;;  :commands intero-mode
;;  :init
;;  (add-hook 'haskell-mode-hook 'intero-mode))




;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; scala
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(use-package scala-mode
  :el-get scala-mode
  :mode (("\\.scala\\'" . scala-mode)))




;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; ocaml
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(use-package tuareg
  :el-get tuareg-mode
  :mode (("\\.ml[ily]?\\'" . tuareg-mode))
  :config
;;  (add-hook 'tuareg-mode-hook 'tuareg-imenu-set-imenu)
;;  (autoload 'utop-setup-ocaml-buffer "utop" "Toplevel for OCaml" t)
;;  (add-hook 'tuareg-mode-hook 'utop-setup-ocaml-buffer)
;; prettify-symbols-mode
  (setq tuareg-prettify-symbols-full t)
  (add-hook 'tuareg-mode-hook 'prettify-symbols-mode))

(use-package merlin
  :el-get merlin
  :commands merlin-mode
  :init
  (add-hook 'tuareg-mode-hook 'merlin-mode)
  :config
  (setq merlin-use-auto-complete-mode t)
  (setq merlin-error-after-save nil))




;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; mgit
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(use-package magit
  :el-get (magit magit-popup)
  :general
  (general-define-key :keymaps '(normal)
                      :prefix "SPC"
                      "gs" 'magit-status
                      "gl" 'magit-log-popup))

(use-package evil-magit
  :el-get evil-magit
  :after (evil magit))




;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; tex
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;; (make-variable-buffer-local 'font-lock-extra-managed-props)
;; (add-to-list 'font-lock-extra-managed-props 'invisible)
;; (font-lock-add-keywords nil
;;                         '(("\\(\\\\\\_<xxx\\_>\{\\)[a-zA-Z0-9_]+\\(\}\\)"
;;                            (1 (prog1 () (compose-region (match-beginning 1) (match-end 1) ?@)))
;;                            (2 '(face nil invisible t)))))
;; [Using font-lock regexp groups - Emacs Stack Exchange](https://emacs.stackexchange.com/questions/28154/using-font-lock-regexp-groups)
(use-package tex-site
  :el-get auctex
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




;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; fish
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(use-package emacs-fish
  :el-get emacs-fish)




;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; go
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(use-package go-mode
  :el-get (go-mode go-autocomplete)
  :mode (("\\.go\\'" . go-mode)))




;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; ruby
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(use-package ruby-mode
  :el-get (ruby-mode)
  :mode (("\\.rb\\'" . ruby-mode))
  :init
  (setq ruby-insert-encoding-magic-comment nil))




;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; node
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(use-package js2-mode
  :el-get (js2-mode add-node-modules-path)
  :mode (("\\.js\\'" . js2-mode))

  :config
  (add-hook 'js-mode-hook #'add-node-modules-path)
  (flycheck-disable-checker '(javascript-jshint javascript-jscs)))

(use-package exec-path-from-shell
  :el-get exec-path-from-shell
  :custom
  (exec-path-from-shell-check-startup-files nil)
  :config
  (push "HISTFILE" exec-path-from-shell-variables)
  (exec-path-from-shell-initialize))
