;;; utils--buffer.el --- Utilities about buffers.

;;; Commentary:

;;; Code:

(require 'cl-lib)
(require 'utils--find-file)

;;; Buffers for file

(defun kill-file-buffers ()
  "Close buffers which file visited in."
  (interactive)
  (mapc 'kill-buffer
        (cl-remove-if-not 'buffer-file-name (buffer-list))))

(defun is-dired (buffer)
  "Check if BUFFER is directory buffer."
  (eq 'dired-mode (buffer-local-value 'major-mode buffer)))

(defun is-asterisked (buffer)
  "Check if BUFFER's name is surrounded asterisks."
  (string-match "\\*.*\\*\\'" (buffer-name buffer)))

;;; Most recently used buffers

(defun mru-buffer ()
  "Return the most recently used buffer."
  (window-buffer (get-mru-window)))

(defun kill-mru-file-buffer ()
  "Close the most recently used buffer for file."
  (interactive)
  (let* ((buffer (mru-buffer))
         (path (buffer-file-name buffer)))
    (when path
      (kill-buffer buffer)
      (message (concat "Closed " path)))))

(defvar kill-mru-file-buffer-before-find-file-p nil
  "If non-nil, the most recently used buffer is killed before 'find-file'.")

(defun kill-mru-file-buffer-before-find-file (&rest args)
  "Close the most recently used buffer before 'find-file'.
ARGS are parameters for 'find-file'."
  (when kill-mru-file-buffer-before-find-file-p
    (kill-mru-file-buffer)))

(defun turn-off-kill-mru-file-buffer-before-find-file-p (&rest args)
  "Trun off 'kill-mru-file-buffer-before-find-file'.
For example, 'ivy-done' jumps through 'with-killing-mru-file-buffer'
without turning off the flag.
Therefore, the flag needs to be turned off after 'find-file'.
ARGS are parameters for 'find-file'."
  (setq kill-mru-file-buffer-before-find-file-p nil))

(add-hook 'before-find-file-hook 'kill-mru-file-buffer-before-find-file)
(add-hook 'after-find-file-hook 'turn-off-kill-mru-file-buffer-before-find-file-p)

(defmacro with-killing-mru-file-buffer (&rest body)
  "Execute BODY with killing the mru file buffer."
  `(progn
    (setq kill-mru-file-buffer-before-find-file-p t)
    ,@body
    ;; For example, 'ivy-done' jumps through the following line.
    (setq kill-mru-file-buffer-before-find-file-p nil)))

;;; Encoding

(defun current-buffer-coding ()
  "Get coding in the current buffer."
  (plist-get (coding-system-plist buffer-file-coding-system) :mime-charset))

(defun revert-buffer-with-euc-jp ()
  "Revert the current buffer with EUC-jp."
  (unless (eq (current-buffer-coding) 'euc-jp)
    (revert-buffer-with-coding-system 'euc-jp)))

;;; provide

(provide 'utils--buffer)

;;; utils--buffer.el ends here
