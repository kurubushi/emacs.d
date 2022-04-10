;;; config--skk.el --- Configuration of SKK.

;;; Commentary:

;;; Code:

(require 'use-package)
(require 'quelpa-use-package)
(require 'general)

;; https://skk-dev.github.io/ddskk/skk.html

(use-package skk
  :quelpa ddskk
  :custom
  (skk-sticky-key ";")
  (skk-kutouten-type 'jp)
  (skk-rom-kana-rule-list '(("!" nil "！")
                            ("?" nil "？")))
  ;;(skk-large-jisyo "/large/jisyo/path") ; in prefix.el

  :config
  ;; priority to skk-j-mode-map over any minor mode map
  ;; as default, haskell-mode binds ";" that I want to be 'skk-kakutei
  ;; ref: https://github.com/haskell/haskell-mode/issues/1320
  (defun skk-j-overrideing-minor ()
    (add-to-list 'minor-mode-overriding-map-alist
                 `(skk-j-mode . ,skk-j-mode-map)))

  ;; to make global skk minor mode and use it always
  ;; ref: https://stackoverflow.com/questions/16048231/
  (define-globalized-minor-mode global-skk-mode skk-mode
    (lambda () (skk-latin-mode 1)))

  (global-skk-mode)

  :general
  ;; to set C-j to evil-ex state
  ;; ref: https://emacs.stackexchange.com/questions/14163/
  (general-define-key :keymaps '(evil-ex-completion-map)
                      "C-j" 'skk-kakutei)

  :hook
  ((skk-mode . skk-j-overrideing-minor)
   ;; to return skk-latin-mode when entrying/exiting from insert-state
   (evil-insert-state-entry . skk-latin-mode-on)
   (evil-insert-state-exit  . skk-latin-mode-on)))

(provide 'config--skk)

;;; config--skk.el ends here
