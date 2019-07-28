;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; core.el
;;----------
;; write core and common configures in it.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; env
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(use-package exec-path-from-shell
  :el-get exec-path-from-shell
  :config
  (setq exec-path-from-shell-shell-name (getenv "ESHELL"))
  (setq exec-path-from-shell-variables
        '("PATH" "LD_LIBRARY_PATH"
          "SSH_AGENT_PID" "SSH_AUTH_SOCK")) ; ssh-agent
  (exec-path-from-shell-initialize)
  (setenv "LANG" "ja_JP.UTF-8")) ;; to pass "flycheck invalid multibyte char"




;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; utils
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; utils/packages
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(use-package dash
  :defer t
  :el-get dash)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; utils/definitions
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun insert-after (string)
  "Insert STRING after the current point."
  (if (not (eolp))
      (forward-char))
  (insert string))

(defun insert-current-time()
  "Insert CURRENT-TIME at the current point."
  (interactive)
  (let ((system-time-locale "C"))
    (insert (format-time-string "%Y-%m-%d(%a) %H:%M:%S" (current-time)))))

(defun append-current-time()
  "Insert CURRENT-TIME after the current point."
  (interactive)
  (let ((system-time-locale "C"))
    (insert-after (format-time-string "%Y-%m-%d(%a) %H:%M:%S" (current-time)))))



(defun is-asterisked (buffer)
  "Check if BUFFER's name is surrounded asterisks."
  (string-match "\\*.*\\*\\'" (buffer-name buffer)))

(defun kill-all-buffers-except-asterisked-buffers ()
  "Kill all asterisked buffers."
  (interactive)
  (mapc 'kill-buffer
        (remove-if 'is-asterisked (buffer-list))))

(defun is-dired (buffer)
  "Check if BUFFER is directory buffer."
  (eq 'dired-mode (buffer-local-value 'major-mode buffer)))

(defun kill-all-dired-buffers ()
  "Kill all directory buffers."
  (interactive)
  (mapc 'kill-buffer
        (remove-if-not 'is-dired (buffer-list))))


(defun eval-and-replace ()
  "Replace the preceding sexp with its value.
http://emacsredux.com/blog/2013/06/21/eval-and-replace/"
  (interactive)
  (backward-kill-sexp)
  (condition-case nil
      (prin1 (eval (read (current-kill 0)))
             (current-buffer))
    (error (message "Invalid expression")
           (insert (current-kill 0)))))




;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; settings
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; settings/encoding
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(prefer-coding-system 'utf-8)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; settings/edit
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; ファイル末尾で必ず改行
(setq require-final-newline t)

;; インデントはハードタブを使わない
(setq-default indent-tabs-mode nil)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; settings/files
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; custom.el を作らせない
(setq custom-file (locate-user-emacs-file "custom.el"))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; settings/view
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; disable tool-bar
(tool-bar-mode 0)

;; disable menu-bar
(menu-bar-mode 0)

;; 括弧の対応をハイライト
(show-paren-mode t)

;; highlights current line
(hl-line-mode t)

;; yes/not -> y/n
(defalias 'yes-or-no-p 'y-or-n-p)

;; disable BEEP
(setq ring-bell-function 'ignore)

;; smooth powerline
(setq ns-use-srgb-colorspace nil)


;; fonts
(defun set-my-font-with-size (size)
  "Set my font in the SIZE."
  (interactive "nsize: ")
  (let* ((fontfamily "Ricty"))
    (set-face-attribute 'default nil :family fontfamily :height (* size 10))
    (set-fontset-font t 'unicode (font-spec :family fontfamily))))

(set-my-font-with-size 12)

(defun set-my-font-atonce (&rest args)
  "Set my font only once with ARGS."
  (set-my-font-with-size 12)
  (remove-hook 'after-make-frame-functions #'set-my-font-atonce))
(add-hook 'after-make-frame-functions #'set-my-font-atonce) ; systemd 経由だと適用されない．しょうがないので hook する


;; theme
(use-package doom-themes
  :el-get doom-themes
  :init
  ;; Global settings (defaults)
  (setq doom-themes-enable-bold t    ; if nil, bold is universally disabled
        doom-themes-enable-italic t) ; if nil, italics is universally disabled
  :config
  ;; Load the theme (doom-one, doom-molokai, etc); keep in mind that each theme
  ;; may have their own settings.
  (load-theme 'doom-Iosvkem t)
  ;; Enable flashing mode-line on errors
  (doom-themes-visual-bell-config)
  ;; Enable custom neotree theme (all-the-icons must be installed!)
  (doom-themes-neotree-config)
  ;; Corrects (and improves) org-mode's native fontification.
  (doom-themes-org-config))




;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; general
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(use-package general
  :el-get general
  :config
  (setq general-default-keymaps 'evil-normal-state-map)
  (general-define-key :keymaps '(insert)
                      ;; eval
                      "C-x C-r" 'eval-and-replace)
  (general-define-key :keymaps '(normal)
                      :prefix "SPC"
                      ;; view
                      "vs" 'text-scale-adjust
                      "vS" 'set-my-font-with-size
                      "vR" 'reload-my-theme
                      "vl" 'load-theme
                      "ve" 'enable-theme
                      "vd" 'disable-theme
                      ;; buffer
                      "bd" 'kill-this-buffer
                      "bD" 'kill-all-dired-buffers
                      "bA" 'kill-all-buffers-except-asterisked-buffers
                      "bn" 'next-buffer
                      "bp" 'previous-buffer
                      "bk" 'kill-some-buffers)
  ;; remove dangerous binds by typing miss
  (general-define-key :keymaps '(global-map)
                      "C-x C-u" 'nil ; delete 'upcase-region insted of 'disable
                      "C-x C-l" 'nil ; delete 'downcase-region insted of 'disable
                      ))




;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; helper
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(use-package help-fns+
  :el-get help-fns+) ; discribe-keymap




;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; evil
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; evil / evil-mode
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(use-package evil
  :el-get evil
  :init
  (custom-set-variables '(evil-want-keybinding nil)) ; for evil-collection
  :config
  (custom-set-variables '(search-invisible t)) ;https://github.com/syl20bnr/spacemacs/issues/3623
  (custom-set-variables '(evil-want-C-u-scroll t))
  (custom-set-variables  '(evil-want-visual-char-semi-exclusive t)) ; exclusive \n in visual state
  (custom-set-variables '(evil-want-integration nil)) ; for evil-collection
  (custom-set-variables  '(evil-search-module 'isearch))
  (custom-set-variables '(evil-move-cursor-back t)) ; goes back when reterning from insert and prevents going eol
  (evil-mode 1)

  ;;keymap
  ;; original `evil-execute-in-normal-state` executes as "C-\ C-o" of Vim.
  ;; `evil-execute-in-normal-state-natively` executes as "C-o" of Vim.
  (defun evil-execute-in-normal-state-natively ()
    "Execute the next command in Normal state, natively."
    (interactive)
    (setq evil-move-cursor-back_ evil-move-cursor-back)
    (evil-execute-in-normal-state)
    ;; returns to original value without wating evil-delay
    (setq evil-move-cursor-back evil-move-cursor-back_))
  (general-define-key :keymaps '(insert)
                      "C-o" 'evil-execute-in-normal-state-natively)
  (general-define-key :keymaps '(normal)
                      :prefix "SPC"
                      "SPC" 'execute-extended-command))

(use-package evil-collection
  :el-get evil-collection
  :after evil
  :init
  (custom-set-variables '(evil-want-keybinding nil)) ; for evil-collection
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

(use-package helm-config
  :el-get helm
  :config
  (general-define-key :keymaps '(normal visual)
                      :prefix "SPC"
                      "SPC" 'helm-M-x)
  (general-define-key :keymaps '(normal)
                      :prefix "SPC"
                      ;; file
                      "ff" 'helm-find-files
                      "fr" 'helm-recentf
                      ;; buffer
                      "bb" 'helm-buffers-list)
  (helm-mode 1))




;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; which-key
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(use-package which-key
  :el-get which-key
  :config
  (which-key-mode)
  (general-define-key :keymaps '(normal)
                      :prefix "SPC"
                      "ww" 'which-key-show-top-level
                      "wk" 'describe-key
                      "wm" 'describe-mode
                      "wM" 'describe-keymap))




;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; company
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(use-package company
  :el-get company-mode

  :config
  (global-company-mode)


  :general
  (general-define-key :keymaps '(insert)
                      "C-k" 'company-complete))




;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; flycheck
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(use-package flycheck
  :el-get flycheck
  :config (global-flycheck-mode))




;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; skk
;;;;;;;;;;
;; doc: https://skk-dev.github.io/ddskk/skk.html
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(use-package skk
  :el-get ddskk
  :init
  (setq skk-sticky-key ";")
  (setq skk-kutouten-type 'jp)
  (setq skk-rom-kana-rule-list
                '(("!" nil "！")
                  ("?" nil "？")))
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
;; auto-spacing
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(use-package auto-spacing
  :el-get auto-spacing
  :config
    ;; where to insert a space
    (setq auto-spacing-english-regexp (rx (in "a-zA-Z0-9$.`")))
    (setq auto-spacing-non-english-regexp (rx (category japanese)))
    (setq auto-spacing-non-english-exception-regexp (rx (in "。，．！？；：「」（）、")))
    ;; add insert functions
    (setq auto-spacing-self-insert-command-list
          '(skk-insert
            TeX-insert-dollar)))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; shell
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(use-package shell-pop
  :el-get shell-pop
  :general
  (general-define-key :keymaps '(normal insert visual emacs)
                      "<f8>" 'shell-pop))

;;; core.el ends here
