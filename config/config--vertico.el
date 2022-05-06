;;; config--vertico.el --- Configuration of vertico.

;;; Commentary:

;;; Code:

(require 'use-package)
(require 'quelpa-use-package)
(require 'general)

(use-package vertico
  :quelpa (vertico :fetcher git
                   :url "git@github.com:minad/vertico.git")
  :custom
  (vertico-count 20)

  :config
  (vertico-mode))

;; Persist history over Emacs restarts. Vertico sorts by history position.
(use-package savehist
  :config
  (savehist-mode))

;; Optionally use the `orderless' completion style.
(use-package orderless
  :quelpa (orderless :fetcher git
                     :url "git@github.com:oantolin/orderless.git")
  :config
  (setq completion-styles '(orderless basic)))

(provide 'config--vertico)

;;; config--vertico.el ends here
