;;; config--perspeen.el --- Configuration of perspeen.

;;; Commentary:

;; conflict with `config--persp-mode'

;;; Code:

(eval-when-compile
  (add-to-list 'load-path (concat user-emacs-directory "el-get/use-package")))

(require 'use-package)


;;; perspeen

(use-package perspeen
  :el-get perspeen
  :init
  (setq perspeen-use-tab nil)
  :config
  (perspeen-mode)
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
  :el-get helm-perspeen
  :after (helm perspeen)
  :general
  (general-define-key :keymaps '(normal)
                      :prefix "SPC"
                      "pp" 'helm-perspeen))


(provide 'config--perspeen)


;;; config--perspeen.el ends here
