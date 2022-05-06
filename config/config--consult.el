;;; config--consult.el --- Configuration of consult.

;;; Commentary:

;;; Code:

(require 'cl-lib)
(require 'use-package)
(require 'quelpa-use-package)
(require 'general)

(use-package consult
  :quelpa
  ;; require embark to kill buffers in `consult-buffer'.
  :after (vertico embark persp-mode)
  :custom
  (consult-buffer-sources '(consult--source-persp-buffer
                            consult--source-recent-file
                            consult--source-bookmark
                            consult--source-project-buffer
                            consult--source-project-recent-file))

  :config

  ;;; Sources

  (defvar consult--source-persp-buffer
    `(:name     "Buffers"
      :narrow   ?b
      :category buffer
      :face     consult-buffer
      :history  buffer-name-history
      :state    ,#'consult--buffer-state
      :default  t
      :items    ,(lambda ()
                   (let ((buffers   (if persp-mode
                                        (persp-buffer-list)
                                      (buffer-list)))
                         (is-hidden (lambda (buf)
                                      (string-match-p
                                       (consult--regexp-filter consult-buffer-filter)
                                       (buffer-name buf)))))
                     (mapcar #'buffer-name (cl-remove-if is-hidden buffers)))))
    "Buffers at the current perspective.")

  ;; Keymaps

  (defvar consult-buffer-map (make-sparse-keymap)
    "Keymap for 'consult-buffer'.")

  (consult-customize consult-buffer :keymap consult-buffer-map)

  ;; Commands

  (defun consult-selected-kill-buffer ()
    "Kill the selected buffer at 'consult-buffer'."
    (interactive)
    (let* ((target (plist-get (car (embark--targets)) :target))
           (buffer (get-buffer target)))
      (when buffer
        (kill-buffer buffer)
        ;; Use `embark--reset' instead of `consult-vertico--refresh'
        ;; becase `consult-vertico--refresh' cannot refresh the candidates.
        ;; ref. https://github.com/minad/consult/issues/474
        (embark--restart))))

  (defun vertico-exit-with-killing-mrufb ()
    "Open the selected file with killing the most recently used file buffer."
    (interactive)
    (when (minibufferp)
      (with-killing-mru-file-buffer
       (vertico-exit))))

  :general
  (general-define-key :keymaps 'normal
                      :prefix "SPC"
                      "SPC" #'execute-extended-command
                      "bb"  #'consult-buffer
                      "fe"  #'consult-find
                      "ff"  #'find-file
                      "fr"  #'consult-recent-file
                      "gg"  #'consult-git-grep)
  (general-define-key :keymaps 'consult-buffer-map
                      "C-k" #'consult-selected-kill-buffer)
  (general-define-key :keymaps 'vertico-map
                      "C-<return>" #'vertico-exit-with-killing-mrufb))

(provide 'config--consult)

;;; config--consult.el ends here
