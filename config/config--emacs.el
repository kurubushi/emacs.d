;;; config--emacs.el --- Configuration of Emacs.

;;; Commentary:

;;; Code:

;;; coding

(prefer-coding-system 'utf-8)

;;; edit

(setq require-final-newline t) ; ファイル末尾で必ず改行
(setq-default indent-tabs-mode nil) ; インデントはハードタブを使わない

;;; view

(tool-bar-mode 0) ; disable tool-bar
(menu-bar-mode 0) ; disable menu-bar
(hl-line-mode t) ; highlights current line

(defalias 'yes-or-no-p 'y-or-n-p) ; yes/not -> y/n
(setq ring-bell-function 'ignore) ; disable BEEP

;;; fonts

;; depends on Ricty and Noto Color Emoji fonts.
;;   $ yay -S ttf-ricty noto-fonts-emoji
;; zenkaku-space 　 is displayed with Ricty and
;; :smile: is displayed with Noto.
(defconst default-font-family "Ricty")
(defconst default-emoji-font-family "Noto Color Emoji")
(defconst default-font-size 14)

(defun set-my-font-with-size (size)
  "Set my font in the SIZE."
  (interactive "nsize: ")
  (set-face-attribute 'default nil :family default-font-family :height (* size 10))
  (set-fontset-font "fontset-default" 'unicode (font-spec :family default-font-family))
  ;; 'symbol を選択しても :wrench: に Noto が使われなかったので 'unicode を選択する
  ;; 上の Ricty より優先されるように 'prepend を指定する
  (set-fontset-font "fontset-default" 'unicode (font-spec :family default-emoji-font-family) nil 'prepend))

(set-my-font-with-size 15)

(defun set-my-font-atonce (&rest args)
  "Set my font only once with ARGS."
  (set-my-font-with-size 15)
  (remove-hook 'after-make-frame-functions #'set-my-font-atonce))
(add-hook 'after-make-frame-functions #'set-my-font-atonce) ; systemd 経由だと適用されない．しょうがないので hook する

;;; theme

(setq custom-file (locate-user-emacs-file "custom.el")) ; custom.el を作らせない

;;; provide

(provide 'config--emacs)

;;; config--emacs.el ends here
