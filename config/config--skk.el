;;; config--skk.el --- Configuration of SKK.

;;; Commentary:

;;; Code:

(require 'use-package)
(require 'quelpa-use-package)
(require 'general)

(defun get-jisyo-path (jisyo)
  "Get SKK-JISYO file path of JISYO."
  (if (stringp jisyo)
      jisyo
    (car jisyo)))

(defun available-jisyo-p (jisyo)
  "Check if JISYO is available."
  (file-readable-p (get-jisyo-path jisyo)))

(defun skk/ (path)
  "Prepend `$HOME/.local/share' to PATH."
  (format "%s/.local/share/skk/%s" (getenv "HOME") path))

;; https://skk-dev.github.io/ddskk/skk.html

;; install JISYO
;; - ~/.local/share/skk/SKK-JISYO.L (symbolic link is OK)
;; - ~/.local/share/skk/SKK-JISYO.*.utf8

(use-package skk
  :quelpa ddskk
  :custom
  (skk-large-jisyo (skk/ "SKK-JISYO.L"))
  (skk-jisyo `(,(concat user-emacs-directory "skk-jisyo.utf8") . utf-8))
  ;; Each file name (excluding directory path) must not conflict with others.
  ;; For example, let `skk-jisyo' be "~/.emacs.d/skk-jisyo";
  ;; even if `skk-extra-jisyo-file-list' contains "/other/directory/path/skk-jisyo",
  ;; the jisyo is not a search target.
  (skk-extra-jisyo-file-list
   (seq-filter #'available-jisyo-p
               `((,(skk/ "SKK-JISYO.user.utf8") . utf-8)
                 (,(skk/ "SKK-JISYO.emoji.utf8") . utf-8)
                 (,(skk/ "SKK-JISYO.jawiki.utf8") . utf-8))))
  (skk-sticky-key ";")
  (skk-kutouten-type 'jp)
  (skk-rom-kana-rule-list '(("!" nil "！")
                            ("?" nil "？")))

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

(use-package skk-jisyo-edit
  :quelpa ddskk
  :mode ("\\.skk\\'" . skk-jisyo-edit-mode))

(provide 'config--skk)

;;; config--skk.el ends here
