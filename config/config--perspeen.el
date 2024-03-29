;;; config--perspeen.el --- Configuration of perspeen.

;;; Commentary:

;; conflict with `config--persp-mode'

;;; Code:

(require 'use-package)
(require 'quelpa-use-package)
(require 'general)

;;; perspeen

(use-package perspeen
  :quelpa perspeen

  :init
  (setq perspeen-use-tab nil)

  :config
  (perspeen-mode)

  ;; Use the old definition as a workaround.
  ;; https://github.com/emacs-helm/helm/commit/f7fa3a9e0ef1f69c42e0c513d02c9f76ea9a4344
  (defun perspeen-helm-buffer-list (orig-fun &rest args)
    "Advice of `helm-buffer-list'. Use ido."
    (require 'ido)
    (let ((ido-process-ignore-lists t)
          ido-ignored-list
          ido-ignore-buffers
          ido-use-virtual-buffers)
      (ido-make-buffer-list nil)))
  (advice-add 'helm-buffer-list :around #'perspeen-helm-buffer-list)

  :general
  (general-define-key :keymaps '(normal)
                      :prefix "SPC"
                      "pC" 'perspeen-create-ws
                      "pN" 'perspeen-next-ws
                      "pP" 'perspeen-previous-ws
                      "pK" 'perspeen-delete-ws
                      "pR" 'perspeen-rename-ws
                      "pD" 'perspeen-change-root-dir
                      "p'" 'perspeen-goto-last-ws
                      "p1" 'perspeen-ws-jump
                      "p2" 'perspeen-ws-jump
                      "p3" 'perspeen-ws-jump
                      "p4" 'perspeen-ws-jump
                      "p5" 'perspeen-ws-jump
                      "p6" 'perspeen-ws-jump
                      "p7" 'perspeen-ws-jump
                      "p8" 'perspeen-ws-jump
                      "p9" 'perspeen-ws-jump
                      "pe" 'perspeen-ws-eshell
                      "pc" 'perspeen-tab-create-tab
                      "pk" 'perspeen-tab-del
                      "pn" 'perspeen-tab-next
                      "pp" 'perspeen-tab-prev)
  ; By default,
  ; perspeen-tab--header-line-inactive <- mode-line-active
  ; perspeen-tab--powerline-inactive1 <- mode-line-active + grayscale
  (set-face-attribute 'perspeen-tab--header-line-inactive nil
                      :background "gray10"
                      :inherit 'mode-line-inactive)
  (set-face-attribute 'perspeen-tab--powerline-inactive1 nil
                      :background "gray10"
                      :inherit 'mode-line-inactive))

;;; helm-perspeen

(use-package helm-perspeen
  :quelpa helm-perspeen
  :after (helm perspeen)
  :general
  (general-define-key :keymaps '(normal)
                      :prefix "SPC"
                      "pp" 'helm-perspeen))

(provide 'config--perspeen)

;;; config--perspeen.el ends here
