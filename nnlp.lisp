;;;; nnlp.lisp

(in-package #:nnlp)

;;; "nnlp" goes here. Hacks and glory await!

;; P01 (*) Find the last box of a list.
;; Example:
;; * (my-last '(a b c d))
;; (D)

(defun p01 (lst)
  (mylast lst))
(defun mylast (lst)
  (if (null (rest lst))
      lst
      (mylast (rest lst))))

(defun p02 (lst)
  (my-butlast lst))
(defun my-butlast (lst)
  (when (consp (rest lst))
    (if (null (rest (rest lst)))
        lst
        (my-butlast (rest lst)))))

(defun p03 (lst at)
  (my-nth lst at))
(defun my-nth (lst at)
  ;; (when (< at 1)
  ;;   (error "At should be atleast 1"))
  (unless (< at 1)
    (if (or (= at 1) (null lst))
        (car lst)
        (my-nth (cdr lst) (- at 1)))))

(defun p04 (lst)
  (my-length lst 0))

(defun my-length (lst len)
  (if (null lst)
      len
      (my-length (rest lst) (1+ len))))

(defun p05 (lst)
  (my-reverse lst))

(defun my-reverse (lst)
  (labels (
           (rev (acc lst)
             (if (null lst)
                 acc
                 (rev (push (first lst) acc) (rest lst)))))
    (rev '() lst )))
(defun p06 (lst)
  (my-palindrome lst))

(defun my-palindrome (lst)
  (cond ((null lst) t)
        ((my-single? lst) t)
        ((eql (first lst) (car (last lst))) (my-palindrome (rest (butlast lst))))
        (t nil)))

(defun my-single? (lst)
  (and (consp lst) (null (cdr lst))))

(defun p07 (lst)
  (flatten lst))

(defun flatten (lst)
  (labels (
           (flat (acc lst)
             (let ((first (first lst)))
              (cond ((null lst) acc)
                    ((consp first) (flat (append acc (flat '()  first)) (rest lst)))
                    (t (flat (append acc (list first)) (rest lst)))))))
    (flat '() lst)))
(defun p08 (lst)
  (compress lst))

(defun compress (lst)
  (labels
      ((comp (acc lst)
         (let ((elt (first lst)))
           (cond ((null lst) acc)
                 ((eql elt (second lst)) (comp acc (rest lst)) )
                 (t (comp (append acc (list elt)) (rest lst)))))))
    (comp '() lst)))