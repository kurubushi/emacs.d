;;; loader.el --- Loads emacs lisp files.

;;; Commentary:

;; A List of `require' and `load'.

;;; Code:

;;; require

(eval-when-compile
  (add-to-list 'load-path (concat user-emacs-directory "el-get/use-package"))
  (add-to-list 'load-path (concat user-emacs-directory "config")))

(require 'use-package)


;;; Emacs

(use-package config--emacs)
(use-package config--exec-path-from-shell)


;;; keymaps

(use-package config--general)
(use-package config--which-key)


;;; view

(use-package config--all-the-icons)
(use-package config--nyan-mode)
(use-package config--git-gutter)
(use-package config--doom-themes)
(use-package config--doom-modeline)


;;; file/buffer

(use-package config--helm)
(use-package config--recentf)
(use-package config--neotree)
(use-package config--perspeen)


;;; edit

(use-package config--evil)
(use-package config--skk)
(use-package config--auto-spacing)
(use-package config--company)


;;; etc

(use-package config--flycheck)
(use-package config--shell-pop)


;;; load

(defconst active-custom-elisp-files
  '("init.d/standard.el"))

(let ((load-file (lambda (file-name)
                   (load (concat user-emacs-directory file-name)))))
  (mapc load-file active-custom-elisp-files))


;;; loader.el ends here
