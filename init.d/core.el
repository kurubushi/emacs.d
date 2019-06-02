;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; core.el
;;----------
;; write core and common configures in it.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; use-package
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;(defun insert-to-list (x y zs)
;;  (if (null zs)
;;      (error "insert-to-list: %s is not in the list." y)
;;    (let ((z (car zs))
;;          (zs (cdr zs)))
;;      (if (eq y z)
;;          `(,x ,y . ,zs)
;;        `(,z . ,(insert-to-list x y zs))))))
;;(defun append-to-list (x y zs)
;;  (if (null zs)
;;      (error "insert-to-list: %s is not in the list." y)
;;    (let ((z (car zs))
;;          (zs (cdr zs)))
;;      (if (eq y z)
;;          `(,y ,x . ,zs)
;;        `(,z . ,(append-to-list x y zs))))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; use-package/:logging
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;(setq use-package-inject-hooks 't)
;;(setq use-package-keywords (insert-to-list :logging :init use-package-keywords))
;;(defalias 'use-package-normalize/:logging 'use-package-nomalize-predicate)
;;;; if `use-package-inject-hooks' become obsoluted,
;;;; we create new keywords `:pre-init', `:pre-config' and their hooks
;;;; and make `:logging' add `(message ...)' to these hooks.
;;(defun use-package-handler/:logging (name keyword arg rest state)
;;  (let ((name-s (use-package-as-string name))) ; use-package-as-string == symbol-name
;;    (add-hook (intern (concat "use-package--" name-s "--pre-init-hook"))
;;              `(lambda () (message "[use-package] Loading :init in %s" ,name-s)))
;;    (add-hook (intern (concat "use-package--" name-s "--pre-config-hook"))
;;              `(lambda () (message "[use-package] Loading :config in %s" ,name-s)))
;;    (set (intern (concat "use-package--" name-s "--el-get-logging")) 't)
;;    (use-package-process-keywords name rest state)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; use-package/:el-get
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;(setq use-package-keywords (append-to-list :el-get :init use-package-keywords))
;;(defalias 'use-package-normalize/:el-get 'use-package-normalize-symlist)
;;(defun use-package-handler/:el-get (name keyword args rest state)
;;  (let ((name-s (use-package-as-string name)))
;;    (use-package-concat
;;     (if (not (boundp (intern (concat "use-package--" name-s "--el-get-logging"))))
;;         'nil
;;       `((message "[use-package] (el-get 'sync %s) in %s" ',args ,name-s)))
;;     `((el-get 'sync ',args))
;;     (use-package-process-keywords name rest state))))




;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; env
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; for flycheck "flycheck invalid multibyte char"
(setenv "LANG" "ja_JP.UTF-8")




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

;; like Vim, insert after current point. (a/i)nsert
(defun insert-after (string)
  (if (not (eolp))
      (forward-char))
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



;; kill all buffers except ... buffers

(defun is-asterisked (buffer)
  (string-match "\\*.*\\*\\'" (buffer-name buffer)))

(defun kill-all-buffers-except-asterisked-buffers ()
  (interactive)
  (mapc 'kill-buffer
        (remove-if 'is-asterisked (buffer-list))))

(defun is-dired (buffer)
  (eq 'dired-mode (buffer-local-value 'major-mode buffer)))

(defun kill-all-dired-buffers ()
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

;; highlights current line
(hl-line-mode t)

;; ファイル末尾で必ず改行
(setq require-final-newline t)

;; インデントはハードタブを使わない
(setq-default indent-tabs-mode nil)

;; disable tool-bar
(tool-bar-mode 0)

;; disable menu-bar
(menu-bar-mode 0)

(defun set-my-font-with-size (size)
  (interactive "nsize: ")
  (let* ((fontfamily "Ricty"))
    (set-face-attribute 'default nil :family fontfamily :height (* size 10))
    (set-fontset-font t 'unicode (font-spec :family fontfamily))))
(set-my-font-with-size 12)
(defun set-my-font-atonce (&rest args)
  (set-my-font-with-size 12)
  (remove-hook 'after-make-frame-functions #'set-my-font-atonce))
(add-hook 'after-make-frame-functions #'set-my-font-atonce) ; systemd 経由だと適用されない．しょうがないので hook する

;; theme

(add-to-list 'custom-theme-load-path (concat user-emacs-directory "theme.d/"))

(setq ns-use-srgb-colorspace nil) ; smooth powerline

(use-package omtose-phellack-theme
  :el-get omtose-phellack-theme)

;; https://stackoverflow.com/questions/18904529/
(defun* reload-my-theme (&optional (frame (selected-frame)))
  (interactive)
  (with-selected-frame frame
    (load-theme 'omtose-darker t)
    (load-theme 'omtose-darker2 t)))
(reload-my-theme)
(defun reload-my-theme-in-gui-only-once (frame)
  (when (and (display-graphic-p frame) (not loaded-theme-p))
    (setq loaded-theme-p t)
    (reload-my-theme frame)))
(setq loaded-theme-p nil)
(add-hook 'after-make-frame-functions 'reload-my-theme-in-gui-only-once)




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
  :config
  (custom-set-variables '(search-invisible t)) ;https://github.com/syl20bnr/spacemacs/issues/3623
  (custom-set-variables '(evil-want-C-u-scroll t))
  (custom-set-variables  '(evil-want-visual-char-semi-exclusive t)) ; exclusive \n in visual state
  (custom-set-variables  '(evil-search-module 'isearch))
  (custom-set-variables '(evil-want-integration nil)) ; for evil-collection
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
;; For some reason, the next function cannot restore evil-move-cursor-back.
;;  (defun evil-execute-in-normal-state-natively ()
;;    "Execute the next command in Normal state, natively."
;;    (interactive)
;;    (if (eolp)
;;        (evil-backward-char))
;;    (evil-execute-in-normal-state))
  (general-define-key :keymaps '(insert)
                      "C-o" 'evil-execute-in-normal-state-natively)
  (general-define-key :keymaps '(normal)
                      :prefix "SPC"
                      "SPC" 'execute-extended-command))

(use-package evil-collection
  :el-get evil-collection
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
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(use-package skk
  :el-get ddskk
  :init
  (setq skk-sticky-key ";")
  (setq skk-kutouten-type 'jp)
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


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; env
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(use-package exec-path-from-shell
  :el-get exec-path-from-shell
  :config
  ;; copy current universal env variables related ssh-agent in fish
  ;; with export ESHELL=$(which fish)
  (setq exec-path-from-shell-shell-name (getenv "ESHELL"))
  (defun resetenv ()
    (interactive)
    (exec-path-from-shell-copy-envs '("SSH_AGENT_PID" "SSH_AUTH_SOCK")))
  (resetenv))

;;EOF
