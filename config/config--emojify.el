;;; config--emojify.el --- Configuration of emojify.

;;; Commentary:

;;; Code:

(require 'use-package)
(require 'quelpa-use-package)

;; https://github.com/iqbalansari/emacs-emojify
(use-package emojify
  :quelpa
  :hook (after-init . global-emojify-mode)
  :custom
  (emojify-display-style 'unicode)
  (emojify-user-emojis `((":memo:" .
                          (("name"    . "Memo")
                           ;; ("image"   . "1f4dd.png")
                           ("unicode" . "📝")
                           ("style"   . "github")))
                         (":camera_flash:" .
                          (("name"    . "Camera Flash")
                           ;; ("image"   . "1f4f8.png")
                           ("unicode" . "📸")
                           ("style"   . "github")))
                         (":adhesive_bandage:" .
                          (("name"    . "Adhesive Bandage")
                           ;; ("image"   . "1fa79.png")
                           ("unicode" . "🩹")
                           ("style"   . "github")))
                         (":monocle_face:" .
                          (("name"    . "Monocle Face")
                           ;; ("image"   . "1f9d0.png")
                           ("unicode" . "🧐")
                           ("style"   . "github")))
                         (":test_tube:" .
                          (("name"    . "Test Tube")
                           ;; ("image"   . "1f9ea.png")
                           ("unicode" . "🧪")
                           ("style"   . "github")))
                         (":stethoscope:" .
                          (("name"    . "Stethoscope")
                           ;; ("image"   . "1fa7a.png")
                           ("unicode" . "🩺")
                           ("style"   . "github")))
                         (":bricks:" .
                          (("name"    . "Bricks")
                           ;; ("image"   . "1f9f1.png")
                           ("unicode" . "🧱")
                           ("style"   . "github")))
                         (":technologist:" .
                          (("name"    . "Technologist")
                           ;; ("image"   . "1f9d1-200d-1f4bb.png")
                           ("unicode" . "🧑‍💻") ; FIXME: Emoji ZWJ Sequences has a blank.
                           ("style"   . "github")))
                         (":thread:" .
                          (("name"    . "Thread")
                           ;; ("image"   . "1f9f5.png")
                           ("unicode" . "🧵")
                           ("style"   . "github")))
                         (":safety_vest:" .
                          (("name"    . "Safety Vest")
                           ;; ("image"   . "1f9ba.png")
                           ("unicode" . "🦺")
                           ("style"   . "github")))))
  :config
  (emojify-set-emoji-styles '(github))
  (global-emojify-mode))

(provide 'config--emojify)

;;; config--emojify.el ends here
