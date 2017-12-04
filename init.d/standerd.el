;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; standerd.el
;----------
; write standerd configures in it.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; PDF
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(el-get-bundle pdf-tools) ; requires automake and proppler.
(use-package pdf-tools
  :config
  (add-to-list 'auto-mode-alist '("\\.pdf\\'" . pdf-view-mode))
  (add-hook 'pdf-view-mode-hook #'pdf-sync-minor-mode)
  ;; configure for evil
  (evil-set-initial-state 'pdf-view-mode 'normal) ; use evil keybind
;  (evil-set-initial-state 'pdf-outline-buffer-mode 'normal)
  (evil-define-key 'normal pdf-view-mode-map ; assign functions to keys
    "g" 'pdf-view-goto-page
    "j" 'pdf-view-scroll-up-or-next-page
    "k" 'pdf-view-scroll-down-or-previous-page
    "h" 'left-char
    "l" 'right-char
    "d" 'pdf-view-next-page-command
    "u" 'pdf-view-previous-page-command
    "+" 'pdf-view-enlarge
    "-" 'pdf-view-shrink
    "=" 'pdf-view-fit-width-to-window
    "o" 'pdf-outline
    "b" 'pdf-view-position-to-register
    "B" 'pdf-view-jump-to-register
    ",vs" 'pdf-view-auto-slice-minor-mode
    ",vd" 'pdf-view-dark-minor-mode
    ",vm" 'pdf-view-midnight-minor-mode
    ",r" 'pdf-view-restore-last-page
    ",s" 'pdf-view-dump-last-page))
;    (add-to-list 'pdf-view-mode-hook
;                 #'(lambda () (setq pdf-view-display-size 'fit-page))




;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; markdown
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(el-get-bundle markdown-mode)
(use-package markdown-mode)




;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; haskell
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(el-get-bundle haskell-mode)
(use-package haskell-mode)




;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; ocaml
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

(el-get-bundle merlin
  :type github
  :pkgname "ocaml/merlin"
  :load-path "emacs")
(use-package merlin
  :config
  (add-hook 'tuareg-mode-hook 'merlin-mode)
  (setq merlin-use-auto-complete-mode t)
  (setq merlin-error-after-save nil))




;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; mgit
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(el-get-bundle magit-popup
  :type github
  :pkgname "magit/magit-popup")
(el-get-bundle magit)
(use-package magit)

(el-get-bundle evil-magit)
(use-package evil-magit)
