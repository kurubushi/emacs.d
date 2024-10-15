;;; confide--copilot.el --- Configuration of copilot.

;;; Commentary:

;;; Code:

(require 'use-package)
(require 'quelpa-use-package)
(require 'general)

(use-package copilot
  :quelpa (copilot :fetcher github
                   :repo "zerolfx/copilot.el"
                   :branch "main"
                   :files ("dist" "*.el"))

  :demand

  :general
  (general-define-key :keymaps 'copilot-mode-map
                      :states 'insert
                      :prefix "C-l"
                      "C-l" 'copilot-accept-completion
                      "C-n" 'copilot-next-completion
                      "C-p" 'copilot-previous-completion)
  
  :config
  (defun copilot-login-p ()
    "Check if copilot is logged in."
    (if (not (locate-file copilot-node-executable exec-path))
        (warn "%s is not found." copilot-node-executable)
      (copilot--dbind
          (:status :user :userCode user-code :verificationUri verification-uri)
          (copilot--request 'signInInitiate '(:dummy "signInInitiate"))
        (s-equals-p status "AlreadySignedIn"))))

  (defun copilot-turn-on ()
    "Turn on copilot."
    (when (copilot-login-p)
      (copilot-mode 1)))

  :hook (prog-mode . copilot-turn-on))

(use-package copilot-chat
  :quelpa
  :after (request org markdown-mode))

(provide 'config--copilot)

;;; config--copilot.el ends here
