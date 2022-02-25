;;; config--tuareg.el --- Configuration of tuareg.

;;; Commentary:

;;; Code:

(eval-when-compile
  (add-to-list 'load-path (concat user-emacs-directory "el-get/use-package")))

(require 'use-package)


;;; use-package

(use-package tuareg
  :quelpa tuareg-mode
  :mode (("\\.ml[ily]?\\'" . tuareg-mode))
  :config
;;  (add-hook 'tuareg-mode-hook 'tuareg-imenu-set-imenu)
;;  (autoload 'utop-setup-ocaml-buffer "utop" "Toplevel for OCaml" t)
;;  (add-hook 'tuareg-mode-hook 'utop-setup-ocaml-buffer)
;; prettify-symbols-mode
  (setq tuareg-prettify-symbols-full t)
  (add-hook 'tuareg-mode-hook 'prettify-symbols-mode))


(provide 'config--tuareg)


;;; config--tuareg.el ends here
