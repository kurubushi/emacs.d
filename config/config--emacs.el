;;; config--emacs.el --- Configuration of Emacs.

;;; Commentary:

;;; Code:

;;; hooks

(defvar after-find-file-hooks nil)

(defun after-find-file-advice (&rest args)
  "Advice of `find-file' with ARGS."
  (apply 'run-hook-with-args 'after-find-file-hooks args))

(advice-add 'find-file :after 'after-find-file-advice)

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

(defun set-my-font-with-size (size)
  "Set my font in the SIZE."
  (interactive "nsize: ")
  (let* ((fontfamily "Ricty"))
    (set-face-attribute 'default nil :family fontfamily :height (* size 10))
    (set-fontset-font t 'unicode (font-spec :family fontfamily))))

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
