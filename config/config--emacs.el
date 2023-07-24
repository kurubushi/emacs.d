;;; config--emacs.el --- Configuration of Emacs.

;;; Commentary:

;;; Code:

(require 'utils--font)

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

;; depends on Ricty Diminished and Noto Color Emoji fonts.
;;   $ yay -S ttf-ricty-diminished noto-fonts-emoji
;; zenkaku-space 　 is displayed with Ricty Diminished and
;; :smile: is displayed with Noto.
(defconst default-font-family "Ricty Diminished")
(defconst default-emoji-font-family "Noto Color Emoji")
(defconst default-font-size 14)

(setq use-default-font-for-symbols nil)

(defun set-my-font-family (font-family)
  "Set FONT-FAMILY as default font family."
  (interactive (list (completing-read "font-family: " (font-family-list) nil t default-font-family)))
  (if (not (installed-font-family-p font-family))
      (warn "font-family \"%s\" is not installed" font-family)
    (set-fontset-font t 'ascii (font-spec :family font-family))
    (set-fontset-font t 'cp932 (font-spec :family font-family))
    (set-fontset-font t 'symbol (font-spec :family font-family)))
  (if (not (installed-font-family-p default-emoji-font-family))
      (warn "font-family \"%s\" is not installed" default-emoji-font-family)
    (set-fontset-font t 'unicode (font-spec :family default-emoji-font-family) nil 'prepend)))

(defun set-my-font-size (size)
  "Set SIZE as default font size."
  (interactive (list (read-number "size: " default-font-size)))
  (let ((height (* size 10)))
    (set-face-attribute 'default nil :height height)))

(set-my-font-family default-font-family)
(set-my-font-size default-font-size)
(set-language-environment "Japanese")

(defun set-my-font-atonce (&rest args)
  "Set my font only once with ARGS."
  (set-my-font-family default-font-family)
  (set-my-font-size default-font-size)
  (remove-hook 'after-make-frame-functions #'set-my-font-atonce))

;; systemd 経由だと適用されない．しょうがないので hook する
(add-hook 'after-make-frame-functions #'set-my-font-atonce)

;;; theme

(setq custom-file (locate-user-emacs-file "custom.el")) ; custom.el を作らせない

;;; provide

(provide 'config--emacs)

;;; config--emacs.el ends here
