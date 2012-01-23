;;;; nnlp.lisp

(in-package #:nnlp)

;;; "nnlp" goes here. Hacks and glory await!

;; P01 (*) Find the last box of a list.
;; Example:
;; * (my-last '(a b c d))
;; (D)

(defun p01 (lst)
  (my-last lst))
(defun mylast (lst)
  (if (null (rest lst))
      lst
      (mylast (rest lst))))