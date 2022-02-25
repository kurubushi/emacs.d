;;; config--telephone-line.el --- Configuration of telephone-line.

;;; Commentary:

;; conflict with `config--doom-modeline'.

;;; Code:

(require 'use-package)
(require 'quelpa-use-package)

(use-package telephone-line
  :quelpa
  :after (config--evil config--nyan-mode config--perspeen)
  :config
  ;; https://github.com/dbordak/telephone-line/blob/master/examples.org
  (setq telephone-line-evil-use-short-tag t)
  (telephone-line-defsegment telephone-line-perspeen-segment ()
    (when perspeen-mode
      (format "[%s]" (perspeen-ws-struct-name perspeen-current-ws))))
  (telephone-line-defsegment telephone-line-mule-info-segment ()
    '("" mode-line-mule-info "%*"))
  (telephone-line-defsegment* telephone-line-position-info-segment ()
    '("" "%l.%C"))

  (setq telephone-line-lhs
        '((evil   . (telephone-line-evil-tag-segment))
          (accent . (telephone-line-major-mode-segment
                     telephone-line-mule-info-segment))
          (nil    . (telephone-line-buffer-name-segment))
          (accent . (telephone-line-position-info-segment))
          (nil    . (telephone-line-nyan-segment))))
  (setq telephone-line-rhs
        '((accent . (telephone-line-vc-segment
                     telephone-line-perspeen-segment))))

  (telephone-line-mode t))

(provide 'config--telephone-line)

;;; config--telephone-line.el ends here
