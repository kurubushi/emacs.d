;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; core.el
;----------
; write core and common configures in it.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; use-package / add :install
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(el-get-bundle! dash
  :type github
  :pkgname "magnars/dash.el")

(setq use-package-keywords (--splice-list (equal it :init) '(:install :init) use-package-keywords)) ;; to expand from the last
(defalias 'use-package-normalize/:install 'use-package-normalize-forms)
(defun use-package-handler/:install (name keyword arg rest state)
  (let ((body (use-package-process-keywords name rest state)))
    (use-package-concat
      (use-package-hook-injector (use-package-as-string name) :init arg)
      body)))




;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; settings
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; custom.el を作らせない
(setq custom-file (locate-user-emacs-file "custom.el"))

; change yes/not into y/n
(defalias 'yes-or-no-p 'y-or-n-p)

; 括弧の対応をハイライト
(show-paren-mode t)

; ファイル末尾で必ず改行
(setq require-final-newline t)

; disable tool-bar
(tool-bar-mode 0)

; disable menu-bar
(menu-bar-mode 0)

; font and size
;(defmacro add-hook-once (hook fun)
;  `(let ((hookfun #'(lambda (&rest args) (funcall ,fun args) (remove-hook ,hook hookfun))))
;    (add-hook ,hook hookfun)))

(defun set-my-font-config (&rest args)
  (let* ((size 12)
         (fontfamily "Ricty"))
    (set-face-attribute 'default nil :family fontfamily :height (* size 10))
    (set-fontset-font t 'unicode (font-spec :family fontfamily))))
(set-my-font-config)
(defun set-my-font-config-atonce (&rest args)
  (set-my-font-config)
  (remove-hook 'after-make-frame-functions #'set-my-font-config-atonce))
(add-hook 'after-make-frame-functions #'set-my-font-config-atonce) ;; systemd 経由だと適用されない．しょうがないので hook する

; theme
(use-package spacemacs-common
  :install (el-get-bundle spacemacs-theme
    :type github
    :pkgname "nashamri/spacemacs-theme"
    :post-init (add-to-list 'custom-theme-load-path default-directory))
  :config (load-theme 'spacemacs-dark t))
(use-package darkmine-theme
  :install (el-get-bundle darkmine-theme)
  :config (load-theme 'darkmine t))




;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; evil
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; evil / evil-mode
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(use-package evil
  :install (el-get-bundle evil)
  
  :init
  (message "init")
  (setq hogehoge 4)
  (setq evil-want-C-u-scroll t)

  :config
  (message "config")
  (setq hogehoge 5)
  (custom-set-variables '(evil-want-C-u-scroll t))
  (custom-set-variables  '(evil-want-visual-char-semi-exclusive t)) ;; exclusive \n in visual state
  (custom-set-variables  '(evil-search-module 'evil-search))
  (evil-mode 1)
  (define-key evil-insert-state-map (kbd "C-k") #'auto-complete))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; evil / evil-leader
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(use-package evil-leader
  :install (el-get-bundle evil-leader)

  :config
  (global-evil-leader-mode 1)
  (evil-leader/set-leader "<SPC>")
  (evil-leader/set-key "<SPC>" #'execute-extended-command))




;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; persp-mode
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(use-package persp-mode
  :install
  (el-get-bundle persp-mode
    :type github
    :pkgname "Bad-ptr/persp-mode.el")
  
  :init
  (setq persp-keymap-prefix nil) ;prefix
  
  :config
  (persp-mode 1)
  (evil-leader/set-key "pn" #'persp-next)
  (evil-leader/set-key "pp" #'persp-prev)
  (evil-leader/set-key "ps" #'persp-frame-switch)
  (evil-leader/set-key "pS" #'persp-window-switch)
  (evil-leader/set-key "pr" #'persp-rename)
  (evil-leader/set-key "pc" #'persp-copy)
  (evil-leader/set-key "pC" #'persp-kill)
  (evil-leader/set-key "pz" #'persp-save-and-kill)
  (evil-leader/set-key "pa" #'persp-add-buffer)
  (evil-leader/set-key "pb" #'persp-switch-to-buffer)
  
  (evil-leader/set-key "pi" #'persp-import-buffers)
  (evil-leader/set-key "pI" #'persp-import-win-conf)
  (evil-leader/set-key "pk" #'persp-remove-buffer)
  (evil-leader/set-key "pK" #'persp-kill-buffer)
  (evil-leader/set-key "pw" #'persp-save-state-to-file)
  (evil-leader/set-key "pW" #'persp-save-to-file-by-names)
  (evil-leader/set-key "pl" #'persp-load-state-from-file)
  (evil-leader/set-key "pL" #'persp-load-from-file-by-names)
  (evil-leader/set-key "po" #'(lambda () (interactive) (persp-mode -1))))




;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; helm
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(use-package helm-config
  :install (el-get-bundle helm)
  :config
  (evil-leader/set-key "e" #'helm-find-files)
  (evil-leader/set-key "b" #'helm-buffers-list)
  (evil-leader/set-key "<SPC>" #'helm-M-x)
  (helm-mode 1))




;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; auto-complete
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(use-package auto-complete
  :install (el-get-bundle auto-complete)

  :init
  (use-package auto-complete-config)

  :config
  (ac-config-default)
  (setq ac-use-menu-map t) ;; C-p/C-n move
  (setq ac-auto-start nil))




;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; skk
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(use-package skk
  :install (el-get-bundle ddskk)

  :init
  (setq skk-sticky-key ";")
  (setq skk-kutouten-type 'en)
  ;(setq skk-large-jisyo "/large/jisyo/path") ; setq in prefix.el

  :config
  (defun skk-mode-on-auto ()
    (skk-mode 1)
    (skk-latin-mode-on))
  (add-hook 'evil-insert-state-entry-hook #'skk-mode-on-auto)
  (add-hook 'evil-insert-state-exit-hook #'skk-mode-on-auto))

;EOF
