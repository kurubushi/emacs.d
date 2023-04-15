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
  (skk-jisyo (concat user-emacs-directory "skk-jisyo"))
  ;; Each file name (excluding directory path) must not conflict with others.
  ;; For example, let `skk-jisyo' be "~/.emacs.d/skk-jisyo";
  ;; even if `skk-extra-jisyo-file-list' contains "/other/directory/path/skk-jisyo",
  ;; the jisyo is not a search target.
  ;; Extra jisyo pathes are defined at post-config.el.
  ;;(setq skk-large-jisyo "/large/jisyo/path")
  ;;(setq skk-extra-jisyo-file-list '("/jisyo/path" ("/jisyo/path.utf8" . utf-8)))
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
  :config
  (defun current-buffer-coding ()
    "Get coding in the current buffer."
    (plist-get (coding-system-plist buffer-file-coding-system) :mime-charset))

  (defun revert-buffer-with-euc-jp ()
    "Revert the current buffer with EUC-jp."
    (unless (eq (current-buffer-coding) 'euc-jp)
      (revert-buffer-with-coding-system 'euc-jp)))

  :mode ("\\.skk\\'" . skk-jisyo-edit-mode)
  :hook (skk-jisyo-edit-mode . revert-buffer-with-euc-jp))

(provide 'config--skk)

;;; config--skk.el ends here
