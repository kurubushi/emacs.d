;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; standard.el
;;----------
;; write standard configures in it.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

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

(if (locate-file "pdftex" exec-path)
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
    (add-hook 'TeX-after-compilation-finished-functions 'TeX-revert-PDF-buffer)))




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

(use-package rubocop
  :el-get (rubocop)
  :config
  (add-hook 'ruby-mode-hook 'rubocop-mode)
  (add-hook 'ruby-mode-hook
            '(lambda ()
               (setq flycheck-checker 'ruby-rubocop)))




;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; node
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(use-package web-mode
  :mode (("\\.js\\'" . web-mode)))
