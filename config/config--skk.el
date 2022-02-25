;;; config--skk.el --- Configuration of SKK.

;;; Commentary:

;;; Code:

(eval-when-compile
  (add-to-list 'load-path (concat user-emacs-directory "el-get/use-package")))

(require 'use-package)


;;; SKK

;; doc: https://skk-dev.github.io/ddskk/skk.html

(use-package skk
  :quelpa ddskk
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


(provide 'config--skk)


;;; config--skk.el ends here
