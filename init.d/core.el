;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; core.el
;----------
; write core and common configures in it.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; utils
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; utils/packages
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(el-get-bundle! dash
  :type github
  :pkgname "magnars/dash.el")


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; utils/definitions
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; insert current time
(defun insert-current-time()
  (interactive)
  (let ((system-time-locale "C"))
    (insert (format-time-string "%Y-%m-%d(%a) %H:%M:%S" (current-time)))))




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

; インデントはハードタブを使わない
(setq-default indent-tabs-mode nil)

; disable tool-bar
(tool-bar-mode 0)

; disable menu-bar
(menu-bar-mode 0)

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
(el-get-bundle spacemacs-theme
  :type github
  :pkgname "nashamri/spacemacs-theme"
  :post-init (add-to-list 'custom-theme-load-path default-directory))
(use-package spacemacs-common
  :config (load-theme 'spacemacs-dark t))

(el-get-bundle darkmine-theme)
(use-package darkmine-theme
  :config (load-theme 'darkmine t))




;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; general
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(el-get-bundle general
  :type github
  :pkgname "noctuid/general.el")
(use-package general
  :config
  (setq general-default-keymaps 'evil-normal-state-map))




;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; helper
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(el-get-bundle help-fns+) ; discribe-keymap
(use-package help-fns+)




;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; evil
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; evil / evil-mode
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(el-get-bundle evil)
(use-package evil
  :config
  (custom-set-variables '(evil-want-C-u-scroll t))
  (custom-set-variables  '(evil-want-visual-char-semi-exclusive t)) ;; exclusive \n in visual state
  (custom-set-variables  '(evil-search-module 'evil-search))
  (evil-mode 1)
  ;keymap
  (general-define-key :keymaps '(insert)
                      "C-k" 'auto-complete)
  (general-define-key :keymaps '(normal)
                      :prefix "SPC"
                      "SPC" 'execute-extended-command))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; evil / evil-leader
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;(el-get-bundle evil-leader)
;(use-package evil-leader
;  :config
;  (global-evil-leader-mode 1)
;  (evil-leader/set-leader "<SPC>")
;  (evil-leader/set-key "<SPC>" #'execute-extended-command))




;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; persp-mode
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(el-get-bundle persp-mode
  :type github
  :pkgname "Bad-ptr/persp-mode.el")
(use-package persp-mode
  :init
  (setq persp-keymap-prefix nil) ;prefix
  :config
  (persp-mode 1)
  (general-define-key :keymaps '(normal)
                      :prefix "SPC"
                      "pn" 'persp-next
                      "pp" 'persp-prev
                      "ps" 'persp-frame-switch
                      "pS" 'persp-window-switch
                      "pr" 'persp-rename
                      "pc" 'persp-copy
                      "pC" 'persp-kill
                      "pz" 'persp-save-and-kill
                      "pa" 'persp-add-buffer
                      "pb" 'persp-switch-to-buffer
  
                      "pi" 'persp-import-buffers
                      "pI" 'persp-import-win-conf
                      "pk" 'persp-remove-buffer
                      "pK" 'persp-kill-buffer
                      "pw" 'persp-save-state-to-file
                      "pW" 'persp-save-to-file-by-names
                      "pl" 'persp-load-state-from-file
                      "pL" 'persp-load-from-file-by-names
                      "po" '(lambda () (interactive) (persp-mode -1))))




;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; helm
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(el-get-bundle helm)
(use-package helm-config
  :config
  (general-define-key :keymaps '(normal)
                      :prefix "SPC"
                      "SPC" 'helm-M-x
                      "e" 'helm-find-files
                      "b" 'helm-buffers-list)
  (helm-mode 1))




;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; auto-complete
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(el-get-bundle auto-complete)
(use-package auto-complete
  :init
  (use-package auto-complete-config)
  :config
  (ac-config-default)
  (setq ac-use-menu-map t) ;; C-p/C-n move
  (setq ac-auto-start nil))




;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; skk
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(el-get-bundle ddskk)
(use-package skk
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




;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; shell
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(el-get-bundle shell-pop)
(use-package shell-pop
  :config
  (general-define-key :keymaps '(normal insert visual emacs)
                      "<f8>" 'shell-pop))

;EOF
