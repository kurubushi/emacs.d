;;; config--org-mode.el --- Configuration of org-mode.

;;; Commentary:

;;; Code:

(require 'general)

(defvar org-directory (concat (getenv "HOME") "/org")
  "Directory for Org-mode.")

(defvar org-notes-file (concat org-directory "/notes.org")
  "File path to save Notes.")

(defvar org-todo-file (concat org-directory "/todo.org")
  "File path to save TODOs.")

(use-package org-mode
  :custom
  ;; org-todo
  (org-todo-keywords '((sequence "TODO(t)" "WIP(w)" "|" "DONE(d)" "CANCELLED(c)")))
  (org-log-done 'time)

  ;; org-capture
  (org-capture-templates '(("n" "Note" entry (file+headline org-notes-file "Notes")
                            "* %?\nEntered on %U\n %i\n %a")
                           ("t" "Todo" entry (file+headline org-todo-file "InBox")
                            "* TODO %?\n %i\n %a")))
  ;; org-refile
  (org-refile-targets `(((,org-directory) :maxlevel . 3)))

  ;; org-agenda
  (org-agenda-files `(,org-directory))

  :config
  (defun show-org-notes ()
    "Show org-notes-file."
    (interactive)
    (if (get-buffer org-notes-file)
        (let ((buffer (get-buffer org-notes-file)))
          (switch-to-buffer buffer))
      (find-file org-notes-file)))

  :general (general-define-key :keymaps 'normal
                               :prefix "SPC o"
                               "a" 'org-agenda
                               "c" 'org-capture
                               "r" 'org-refile
                               "m" 'show-org-notes))

(provide 'config--org-mode)

;;; config--org-mode.el ends here
