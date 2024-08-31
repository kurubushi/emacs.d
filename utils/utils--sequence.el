;;; utils--sequence.el --- Utilities about sequences.

;;; Commentary:

;;; Code:

(require 'cl-lib)

(defun random-in-range (from to)
  "Return a random non-negative integer in [FROM, TO).
FROM is inclusive and TO is exclusive."
  (+ from (random (- to from))))

(defun shuffle (seq)
  "Shuffle SEQ."
  (let ((result (copy-sequence seq)))
    (dotimes (i (length result) result)
      (cl-rotatef (elt result i)
                  (elt result (random-in-range i (length result)))))))

;;; provide

(provide 'utils--sequence)

;;; utils--sequence.el ends here
