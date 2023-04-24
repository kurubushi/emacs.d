;;; config--evil.el --- Configuration of Evil.

;;; Commentary:

;;; Code:

(require 'use-package)
(require 'quelpa-use-package)
(require 'general)

(use-package evil
  :quelpa
  :after config--undo-tree

  :custom
  (evil-want-C-u-scroll t)
  (evil-search-module 'isearch)

  ;; Enable only evil-v$-excludes-newline temporarily.
  ;; (evil-want-visual-char-semi-exclusive nil)
  (evil-v$-excludes-newline t)

  ;; evil-collection assumes the following values.
  ;; ref. https://github.com/emacs-evil/evil-collection
  (evil-want-keybinding nil)
  (evil-want-integration t)

  ;; https://github.com/syl20bnr/spacemacs/issues/3623
  (search-invisible t)

  :config
  ;; https://github.com/emacs-evil/evil/pull/1360
  (evil-set-undo-system 'undo-tree)

  ;; FIXME: Neither `evil-want-visual-char-semi-exclusive' nor
  ;; `evil-v$-excludes-newline' can simulate the visual mode of vi.
  ;;
  ;; For example, consider the following text:
  ;;
  ;; -----
  ;; ab
  ;;
  ;; cd
  ;; -----
  ;;
  ;; When `evil-want-visual-char-semi-exclusive' is not nil,
  ;; `lvjD' is expected to delete `ab' and the empty line;
  ;; however, the command cannot delete `a' and
  ;; the result is:
  ;;
  ;; -----
  ;; a
  ;; cd
  ;; -----
  ;;
  ;; When `evil-v$-excludes-newline' is not nil,
  ;; `vjD' is expected to delete only `ab' and the empty line;
  ;; however, the command deletes also `cd' line and
  ;; the result is:
  ;;
  ;; -----
  ;; -----
  ;;
  ;; A workaround: both `Vjd' and `VjD' work well.
  ;;
  ;; Rewrite `evil-*-line commands':
  (defun evil-*-line-with-1-end (args)
    (if (not (evil-visual-state-p)) args
      (pcase args
        (`(,beg ,end . ,rest) (append `(,beg ,(1- end)) rest)))))

  (mapc (lambda (f)
          (advice-add f :filter-args 'evil-*-line-with-1-end))
        '(evil-delete-line
          evil-yank-line
          evil-change-line
          evil-indent-line))

  (evil-mode 1))

(provide 'config--evil)

;;; config--evil.el ends here
