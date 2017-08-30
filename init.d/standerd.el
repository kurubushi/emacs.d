;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; standerd.el
;----------
; write standerd configures in it.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; PDF
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(use-package pdf-tools
  :install (el-get-bundle pdf-tools) ; requires automake and proppler.
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

(use-package markdown-mode
  :install
  (el-get-bundle markdown-mode))




;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; haskell
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(use-package haskell-mode
  :install
  (el-get-bundle haskell-mode))
