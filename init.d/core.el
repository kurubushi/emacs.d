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

; disable BEEP
(setq ring-bell-function 'ignore)

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

(setq ns-use-srgb-colorspace nil) ; smooth powerline

(el-get-bundle spacemacs-theme
  :type github
  :pkgname "nashamri/spacemacs-theme"
  :post-init (add-to-list 'custom-theme-load-path default-directory))
(use-package spacemacs-common
  :config (load-theme 'spacemacs-dark t))

(el-get-bundle darkmine-theme)
(use-package darkmine-theme
  :config (load-theme 'darkmine t))

; https://stackoverflow.com/questions/18904529/
(defun* load-my-theme (&optional (frame (selected-frame)))
  (interactive)
  (with-selected-frame frame
    (load-theme 'spacemacs-dark t)
    (load-theme 'darkmine t)))
(load-my-theme)
(defun load-my-theme-in-gui-only-once (frame)
  (when (and (display-graphic-p frame) (not loaded-theme-p))
    (setq loaded-theme-p t)
    (load-my-theme frame)))
(setq loaded-theme-p nil)
(add-hook 'after-make-frame-functions 'load-my-theme-in-gui-only-once)
; I don't know why but if a newer frame executes `load-theme`,
; color of older frames is broken.

;(el-get-bundle spacemacs-theme
;  :type github
;  :pkgname "nashamri/spacemacs-theme"
;  :post-init (add-to-list 'custom-theme-load-path default-directory))
;(use-package spacemacs-common
;  :config (load-theme 'spacemacs-dark t))
;
;
;(el-get-bundle darkmine-theme)
;(use-package darkmine-theme
;  :config (load-theme 'darkmine t))




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
  (custom-set-variables '(search-invisible t)) ;https://github.com/syl20bnr/spacemacs/issues/3623
  (custom-set-variables '(evil-want-C-u-scroll t))
  (custom-set-variables  '(evil-want-visual-char-semi-exclusive t)) ;; exclusive \n in visual state
  (custom-set-variables  '(evil-search-module 'isearch))
  (custom-set-variables '(evil-want-integration nil)) ;; for evil-collection
  (evil-mode 1)
  ;keymap
  (general-define-key :keymaps '(insert)
                      "C-k" 'auto-complete)
  (general-define-key :keymaps '(normal)
                      :prefix "SPC"
                      "SPC" 'execute-extended-command))

(el-get-bundle evil-collection
  :type github
  :pkgname "emacs-evil/evil-collection"
  :depends (evil))
(use-package evil-collection
  :after evil
  :config (evil-collection-init))

(defun evil-define-key-escape-spc (keymaps)
  (general-define-key :states 'normal
                      :keymaps keymaps
                      "SPC" nil
                      "S-SPC" nil
                      "C-SPC" nil))


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
; perspeen
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(el-get-bundle perspeen
  :type github
  :pkgname "seudut/perspeen"
  :depends (powerline))
(use-package perspeen
  :init
  (setq perspeen-use-tab t)
  :config
  (perspeen-mode)
  (general-define-key :keymaps '(normal)
                      :prefix "SPC"
                      "pC" 'perspeen-create-ws
                      "pN" 'perspeen-next-ws
                      "pn" 'perspeen-tab-next
                      "pP" 'perspeen-previous-ws
                      "pp" 'perspeen-tab-prev
                      "p'" 'perspeen-goto-last-ws
                      "pe" 'perspeen-ws-eshell
                      "pK" 'perspeen-delete-ws
                      "pk" 'perspeen-tab-del
                      "pR" 'perspeen-rename-ws
                      "pD" 'perspeen-change-root-dir
                      "p1" 'perspeen-ws-jump
                      "p2" 'perspeen-ws-jump
                      "p3" 'perspeen-ws-jump
                      "p4" 'perspeen-ws-jump
                      "p5" 'perspeen-ws-jump
                      "p6" 'perspeen-ws-jump
                      "p7" 'perspeen-ws-jump
                      "p8" 'perspeen-ws-jump
                      "p9" 'perspeen-ws-jumiousp
                      "pc" 'perspeen-tab-create-tab)
  ; By default,
  ; perspeen-tab--header-line-inactive <- mode-line-active
  ; perspeen-tab--powerline-inactive1 <- mode-line-active + grayscale
  (set-face-attribute 'perspeen-tab--header-line-inactive nil
                      :inherit 'mode-line-inactive)
  (set-face-attribute 'perspeen-tab--powerline-inactive1 nil
                      :background "gray11"
                      :inherit 'mode-line-inactive))

(el-get-bundle helm-perspeen
  :type github
  :pkgname "jimo1001/helm-perspeen"
  :depends (perspeen helm))
(use-package helm-perspeen)




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
  (skk-mode 1)
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
