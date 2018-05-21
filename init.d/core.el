;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; core.el
;;----------
;; write core and common configures in it.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; utils
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; utils/packages
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(el-get-bundle dash)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; utils/definitions
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; like Vim, insert after current point. (a/i)nsert
(defun insert-after (string)
  (forward-char)
  (insert string))

;; insert current time
(defun insert-current-time()
  (interactive)
  (let ((system-time-locale "C"))
    (insert (format-time-string "%Y-%m-%d(%a) %H:%M:%S" (current-time)))))
(defun append-current-time()
  (interactive)
  (let ((system-time-locale "C"))
    (insert-after (format-time-string "%Y-%m-%d(%a) %H:%M:%S" (current-time)))))




;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; settings
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; coding-system
(prefer-coding-system 'utf-8)

;; custom.el を作らせない
(setq custom-file (locate-user-emacs-file "custom.el"))

;; change yes/not into y/n
(defalias 'yes-or-no-p 'y-or-n-p)

;; disable BEEP
(setq ring-bell-function 'ignore)

;; 括弧の対応をハイライト
(show-paren-mode t)

;; ファイル末尾で必ず改行
(setq require-final-newline t)

;; インデントはハードタブを使わない
(setq-default indent-tabs-mode nil)

;; disable tool-bar
(tool-bar-mode 0)

;; disable menu-bar
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
(add-hook 'after-make-frame-functions #'set-my-font-config-atonce) ; systemd 経由だと適用されない．しょうがないので hook する

;; theme

(setq ns-use-srgb-colorspace nil) ; smooth powerline

(el-get-bundle spacemacs-theme)
(use-package spacemacs-common
  :config (load-theme 'spacemacs-dark t))

(el-get-bundle darkmine-theme)
(use-package darkmine-theme
  :config (load-theme 'darkmine t))

;; https://stackoverflow.com/questions/18904529/
(defun* reload-my-theme (&optional (frame (selected-frame)))
  (interactive)
  (with-selected-frame frame
    (load-theme 'spacemacs-dark t)
    (load-theme 'darkmine t)))
(reload-my-theme)
(defun reload-my-theme-in-gui-only-once (frame)
  (when (and (display-graphic-p frame) (not loaded-theme-p))
    (setq loaded-theme-p t)
    (reload-my-theme frame)))
(setq loaded-theme-p nil)
(add-hook 'after-make-frame-functions 'reload-my-theme-in-gui-only-once)
;; I don't know why but if a newer frame executes `load-theme`,
;; color of older frames is broken.

;;(el-get-bundle spacemacs-theme
;;  :type github
;;  :pkgname "nashamri/spacemacs-theme"
;;  :post-init (add-to-list 'custom-theme-load-path default-directory))
;;(use-package spacemacs-common
;;  :config (load-theme 'spacemacs-dark t))
;;
;;
;;(el-get-bundle darkmine-theme)
;;(use-package darkmine-theme
;;  :config (load-theme 'darkmine t))




;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; general
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(el-get-bundle general)
(use-package general
  :config
  (setq general-default-keymaps 'evil-normal-state-map)
  (general-define-key :keymaps '(normal)
                      :prefix "SPC"
                      ;; view
                      "vs" 'text-scale-adjust
                      "vR" 'reload-my-theme
                      "vl" 'load-theme
                      "ve" 'enable-theme
                      "vd" 'disable-theme
                      ;;buffer
                      "bd" 'kill-this-buffer
                      "bn" 'next-buffer
                      "bp" 'previous-buffer
                      "bk" 'kill-some-buffers))




;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; helper
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(el-get-bundle help-fns+) ; discribe-keymap
(use-package help-fns+)




;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; evil
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; evil / evil-mode
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(el-get-bundle evil)
(use-package evil
  :config
  (custom-set-variables '(search-invisible t)) ;https://github.com/syl20bnr/spacemacs/issues/3623
  (custom-set-variables '(evil-want-C-u-scroll t))
  (custom-set-variables  '(evil-want-visual-char-semi-exclusive t)) ; exclusive \n in visual state
  (custom-set-variables  '(evil-search-module 'isearch))
  (custom-set-variables '(evil-want-integration nil)) ; for evil-collection
  (custom-set-variables '(evil-move-cursor-back t)) ; goes back when reterning from insert and prevents going eol
  (evil-mode 1)

  ;;keymap
  ;; original `evil-execute-in-normal-state` changes the value `evil-move-cusor-back` into nil against our will.
  ;; `evil-execute-in-normal-state-natively` doesn't change it and executes "C-o" like Vim.
  (defun evil-execute-in-normal-state-natively ()
    "Execute the next command in Normal state, natively."
    (interactive)
    (evil-delay '(not (memq this-command
                            '(evil-execute-in-normal-state
                              evil-use-register
                              digit-argument
                              negative-argument
                              universal-argument
                              universal-argument-minus
                              universal-argument-more
                              universal-argument-other-key)))
        `(progn
           (with-current-buffer ,(current-buffer)
             (evil-change-state ',evil-state)))
             ;;(setq evil-move-cursor-back ',evil-move-cursor-back)))
      'post-command-hook)
    ;;(setq evil-move-cursor-back nil)
    (evil-normal-state)
    (evil-echo "Switched to Normal state for the next command ..."))
  (general-define-key :keymaps '(insert)
                      "C-k" 'auto-complete
                      "C-o" 'evil-execute-in-normal-state-natively)
  (general-define-key :keymaps '(normal)
                      :prefix "SPC"
                      "SPC" 'execute-extended-command))

(el-get-bundle evil-collection)
(use-package evil-collection
  :after evil
  :config
  (evil-collection-init)

  ;; https://github.com/emacs-evil/evil-collection/issues/79
  ;; commited code was wrong.
  ;; this is called by (evil-collection-paren-setup) 
  (defun evil-collection-paren-show-paren-function (f &rest args)
    "Integrate `show-paren-function' with `evil'."
    (if (not (bound-and-true-p evil-mode))
        (apply f args)
      (if (if (memq 'not evil-highlight-closing-paren-at-point-states)
              (memq evil-state evil-highlight-closing-paren-at-point-states)
            (not (memq evil-state evil-highlight-closing-paren-at-point-states)))
          (apply f args)
        (let ((pos (point)) syntax narrow)
          (setq pos
                (catch 'end
                  (dotimes (var (1+ (* 2 evil-show-paren-range)))
                    (if (zerop (mod var 2))
                        (setq pos (+ pos var))
                      (setq pos (- pos var)))
                    (setq syntax (syntax-class (syntax-after pos)))
                    (cond
                     ((eq syntax 4)
                      (setq narrow pos)
                      (throw 'end pos))
                     ((eq syntax 5)
  ;;                    (throw 'end (1+ pos))))))) ; wrong line
                      (throw 'end pos)))))) ; this is correct
          (if pos
              (save-excursion
                (goto-char pos)
                (save-restriction
                  (when narrow
                    (narrow-to-region narrow (point-max)))
                  (apply f args)))
            ;; prevent the preceding pair from being highlighted
            (dolist (ov '(show-paren--overlay
                          show-paren--overlay-1
                          show-paren-overlay
                          show-paren-overlay-1))
              (let ((ov (and (boundp ov) (symbol-value ov))))
                (when (overlayp ov) (delete-overlay ov))))))))))

(defun evil-define-key-escape-spc (keymaps)
  (general-define-key :states 'normal
                      :keymaps keymaps
                      "SPC" nil
                      "S-SPC" nil
                      "C-SPC" nil))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; evil / evil-leader
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;(el-get-bundle evil-leader)
;;(use-package evil-leader
;;  :config
;;  (global-evil-leader-mode 1)
;;  (evil-leader/set-leader "<SPC>")
;;  (evil-leader/set-key "<SPC>" #'execute-extended-command))




;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; perspeen
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(el-get-bundle perspeen)
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
  ;; By default,
  ;; perspeen-tab--header-line-inactive <- mode-line-active
  ;; perspeen-tab--powerline-inactive1 <- mode-line-active + grayscale
  (set-face-attribute 'perspeen-tab--header-line-inactive nil
                      :inherit 'mode-line-inactive)
  (set-face-attribute 'perspeen-tab--powerline-inactive1 nil
                      :background "gray11"
                      :inherit 'mode-line-inactive))

(el-get-bundle helm-perspeen)
(use-package helm-perspeen)




;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; recentf
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(use-package recentf
  :init
  (setq recentf-auto-cleanup 'never) ; disable before we start recentf!
  (setq recentf-max-saved-items 2000)
  (setq recentf-exclude '("~$" "recentf$"))
;;  (setq recentf-auto-save-timer (run-with-idle-timer 30 t 'recentf-save-list))
  :config
  (add-hook 'find-file-hook 'recentf-save-list)
  (recentf-mode 1))




;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; helm
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(el-get-bundle helm)
(use-package helm-config
  :config
  (general-define-key :keymaps '(normal)
                      :prefix "SPC"
                      "SPC" 'helm-M-x
                      ;; file
                      "ff" 'helm-find-files
                      "fr" 'helm-recentf
                      ;; buffer
                      "bl" 'helm-buffers-list)
  (helm-mode 1))




;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; which-key
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(el-get-bundle which-key)
(use-package which-key
  :config
  (which-key-mode)
  (general-define-key :keymaps '(normal)
                      :prefix "SPC"
                      "ww" 'which-key-show-top-level
                      "wk" 'describe-key
                      "wm" 'describe-mode
                      "wM" 'describe-keymap))




;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; auto-complete
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(el-get-bundle auto-complete)
(use-package auto-complete
  :init
  (use-package auto-complete-config)
  :config
  (ac-config-default)
  (setq ac-use-menu-map t) ; C-p/C-n move
  (setq ac-auto-start nil))




;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; skk
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(el-get-bundle ddskk)
(use-package skk
  :init
  (setq skk-sticky-key ";")
  (setq skk-kutouten-type 'en)
  ;;(setq skk-large-jisyo "/large/jisyo/path") ; setq in prefix.el

  :config

  ;; to make global skk minor mode and use it always
  ;; ref: https://stackoverflow.com/questions/16048231/
  (define-globalized-minor-mode global-skk-mode skk-mode
    (lambda () (skk-latin-mode 1)))
  (global-skk-mode)

  ;; to set C-j to evil-ex state
  ;; ref: https://emacs.stackexchange.com/questions/14163/
  (general-define-key :keymaps '(evil-ex-completion-map)
                      "C-j" 'skk-kakutei)

  ;; to return skk-latin-mode when entrying/exiting from insert-state
  (add-hook 'evil-insert-state-entry-hook 'skk-latin-mode-on)
  (add-hook 'evil-insert-state-exit-hook 'skk-latin-mode-on)

  ;; priority to skk-j-mode-map over any minor mode map
  ;; as default, haskell-mode binds ";" that I want to be 'skk-kakutei
  ;; ref: https://github.com/haskell/haskell-mode/issues/1320
  (defun skk-j-overrideing-minor ()
    (add-to-list 'minor-mode-overriding-map-alist
                 `(skk-j-mode . ,skk-j-mode-map)))
  (add-hook 'skk-mode-hook 'skk-j-overrideing-minor))




;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; shell
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(el-get-bundle shell-pop)
(use-package shell-pop
  :config
  (general-define-key :keymaps '(normal insert visual emacs)
                      "<f8>" 'shell-pop))

;;EOF
