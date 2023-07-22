;;; config--ivy.el --- Configuration of ivy/counsel.

;;; Commentary:

;;; Code:

(require 'use-package)
(require 'quelpa-use-package)
(require 'general)

;; https://takaxp.github.io/articles/qiita-helm2ivy.html

(use-package swiper
  :quelpa)

(use-package counsel ; requires ivy and swiper
  :quelpa
  :after utils--buffer
  :functions (with-killing-mru-file-buffer
              ivy-more-chars
              counsel--async-command
              counsel--project-current)

  :config
  (setf (alist-get t ivy-re-builders-alist) 'ivy--regex-ignore-order) ; 絞り込み方法
  (setf (alist-get 'counsel-M-x ivy-initial-inputs-alist) "") ; 絞り込み文字プリセット

  ;; counsel-explore-file

  (defun counsel-explore-file-args (strings)
    "Argument to find files for STRINGS."
    (mapconcat (lambda (str)
                 (format "-iregex '.*%s.*'" str))
               strings
               " -and "))

  (defun counsel-explore-file-except (strings)
    "Argument to avoid finding for STRINGS."
    (mapconcat (lambda (str)
                 (format "-not \\( -wholename '%s' -prune \\)" str))
               strings
               " "))

  (defun counsel-explore-file-cmd (string)
    "Command to find files for STRING."
    (let ((keywords (split-string string))
          (ng-words '("*/.*" "*~" "*#")))
      (format "find . %s %s"
              (counsel-explore-file-except ng-words)
              (counsel-explore-file-args keywords))))

  (defun counsel-explore-file-function (string &optional)
    "Find fine in the current directory for STRING."
    (or
     (ivy-more-chars)
     (progn
       (counsel--async-command (counsel-explore-file-cmd string))
       nil)))

  (defun counsel-explore-file (&optional initial-input directory)
    "Explore files at the current directory.
INITIAL-INPUT can be given as the initial minibuffer input.
DIRECTORY, if non-nil, is used as the root directory for search."
    (interactive)
    (let ((dir (or directory "."))
          (collection-function 'counsel-explore-file-function))
      (with-cd dir
        (ivy-read "Explore file: " collection-function
                  :initial-input initial-input
                  :dynamic-collection t
                  :action #'find-file
                  :require-match t
                  :caller 'counsel-explore-file))))

  (defun counsel-explore-file-on-project-root ()
    "Explore files in the project root."
    (interactive)
    (counsel-explore-file nil (counsel--project-current)))

  ;; hooks

  (defvar after-ivy-switch-buffe-hook nil)

  (defun after-ivy-switch-buffer-advice (&rest args)
    "Advice of `ivy-switch-buffer' with ARGS."
    (apply 'run-hook-with-args 'after-ivy-switch-buffer-hook args))

  (advice-add 'ivy-switch-buffer :after 'after-ivy-switch-buffer-advice)

  (defun ivy-done-with-killing-mru-file-buffer (&rest args)
    "Execute 'ivy-done' with killing the most recently buffer for file.
ARGS are parameters for 'ivy-done'."
    (interactive)
    (with-killing-mru-file-buffer (apply 'ivy-done args)))

  ;; Ignore *-ed buffers (i.e. *Messages*).
  ;; `ivy-toggle-ignore' (C-c C-a) shows ignored buffers.
  ;; (add-to-list 'ivy-ignore-buffers "\\*.*\\*\\'")

  ;; switch-mode
  (ivy-mode 1)
  (counsel-mode 1)

  :general (general-define-key :keymaps '(normal visual)
                               :prefix "SPC"
                               "SPC" 'counsel-M-x)
           (general-define-key :keymaps 'normal
                               :prefix "SPC b"
                               "b" 'counsel-switch-buffer)
           (general-define-key :keymaps 'normal
                               :prefix "SPC f"
                               "f" 'counsel-find-file
                               "r" 'counsel-recentf
                               "e" 'counsel-explore-file-on-project-root)
           (general-define-key :keymaps 'normal
                               :prefix "SPC g"
                               "g" 'counsel-git-grep)
           (general-define-key :keymaps 'ivy-minibuffer-map
                               "RET" 'ivy-done
                               "C-<return>" 'ivy-done-with-killing-mru-file-buffer))

(provide 'config--ivy)

;;; config--ivy.el ends here
