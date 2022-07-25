;;; config--doom-modeline.el --- Configuration of doom-modeline.

;;; Commentary:

;; conflict with `config--telephone-line'.

;;; Code:

(require 'use-package)
(require 'quelpa-use-package)

(use-package doom-modeline
  ; font が足りていなければ M-x all-the-icons-install-fonts
  :quelpa

  :after (config--evil
          config--nyan-mode
          config--all-the-icons)

  :custom
  (doom-modeline-buffer-file-name-style 'buffer-name)
  (doom-modeline-icon t)

  :init
  ;; Emacs 標準の mode-line に関する値を調整
  (setq column-number-mode t)
  (setq mode-line-misc-info nil) ; perspeen の情報等を出さない
  ;; disable SKK indicator at modeline
  (defadvice skk-setup-modeline (around disable--skk-setup-modeline activate)
    "Disable SKK indicator at modeline.")

  :config
  ;; define segments
  ;; https://github.com/seagle0128/doom-modeline/blob/master/doom-modeline-segments.el

  ;; override modals segment
  ;; doom-modeline-main のほか doom-modeline-vcs 等の modeline に利用されているので上書き
  ;; https://github.com/seagle0128/doom-modeline/pull/267
  (doom-modeline-def-segment modals
    "Display current state of evil."
    (when evil-mode
      (let* ((state-char (cond
                          ((evil-normal-state-p)   "N")
                          ((evil-insert-state-p)   "I")
                          ((evil-motion-state-p)   "M")
                          ((evil-visual-state-p)   "V")
                          ((evil-operator-state-p) "O")
                          ((evil-replace-state-p)  "R")
                          ((evil-emacs-state-p)    "E")
                          (t "?")))
             (state-face (if defining-kbd-macro
                              '(:weight bold :foreground "red")
                            '(:weight bold)))
             (state (propertize state-char 'face state-face)))
        (concat
         doom-modeline-spc
         state))))

  ;; override position segment
  (doom-modeline-def-segment buffer-position
    "The current position in the current buffer."
    (concat
     doom-modeline-spc
     "%l:%C"
     (when nyan-mode
       (concat
        doom-modeline-vspc
        (propertize (nyan-create) ; nyancat の現在地を強調する face
                    'face '(:background "#111111"))))
     doom-modeline-spc))

  (doom-modeline-def-segment perspeen
    "The current perspeen workspace name."
    (when perspeen-mode
      (concat
       doom-modeline-spc
       (all-the-icons-material "desktop_windows")
       doom-modeline-vspc
       (perspeen-ws-struct-name perspeen-current-ws)
       doom-modeline-spc)))

  (doom-modeline-def-segment persp-mode
    "The current persp-mode workspace name."
    (when (and persp-mode (get-current-persp))
      (concat
       doom-modeline-spc
       (all-the-icons-material "desktop_windows")
       doom-modeline-vspc
       (persp-name (get-current-persp))
       doom-modeline-spc)))

  ;; override main modeline
  (doom-modeline-def-modeline 'main
    '(bar modals buffer-info remote-host buffer-position)
    '(buffer-encoding vcs checker perspeen persp-mode))
  (doom-modeline-mode t))

(provide 'config--doom-modeline)

;;; config--doom-modeline.el ends here
