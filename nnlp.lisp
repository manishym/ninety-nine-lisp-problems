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

;; P09 (**) Pack consecutive duplicates of list elements into sublists.
;; If a list contains repeated elements they should be placed in separate sublists.

;; Example:
;; * (pack '(a a a a b c c a a d e e e e))
;; ((A A A A) (B) (C C) (A A) (D) (E E E E))

(defun p09 (lst)
  (pack-duplicates lst))

(defun single? (lst)
  (and (consp lst) (null (cdr lst))))
;; (defun pack-duplicates (lst)
;;   (labels
;;       ((pack-list (acc lst)
;;          (cond ((null lst) nil)
;;                ((eql (first acc) (first lst))
;;                 (pack-list (append acc (list (first lst))) (rest lst)))
;;                (t (if (single? acc)
;;                       (list (car acc) lst)
;;                       (list acc lst)))))
;;        (dup (acc lst)
;;          (if (null lst)
;;              acc
;;              (let ((packed  (pack-list (list (first lst)) (rest lst))))
;;                (dup (append acc (list (first packed))) (rest packed))))))
;;     (dup nil lst)))

(defun pack-duplicates-in (lst)
  (duplicate-compress nil lst))

(defun duplicate-compress (acc lst)
  (if (null lst)
      acc
      (let ((packed (pack-duplicates (list (first lst)) (rest lst))))
        (duplicate-compress (append acc (list (first packed))) (first (rest packed))))))

(defun pack-duplicates (acc lst)
  (cond ((null lst) (list acc))
        ((eql (first acc) (first lst))
         (pack-duplicates (append acc (list (first lst))) (rest lst)))
        (t (list acc lst))))

(defun p10 (lst)
  (run-length-encode (lst)))

(defun run-length-encode (lst)
  (compress-list (pack-duplicates-in lst)))

(defun compress-list-loop (lst)
  (let (acc)
    (dolist (elt lst)
      (push  (list (length elt) (first elt)) acc))
    (nreverse acc)))

(defun compress-list (lst)
  (compress-list-loop lst))

(defun p11 (lst)
  (compress-list-loop-modified lst))
(defun modified-run-length-encode (lst)
  (compress-list-loop-modified (pack-duplicates-in lst)))
(defun compress-list-loop-modified (lst)
  (let (acc)
   (dolist (elt lst)
     (let ((length (length elt)))
       (if (> length 1)
           (push (list length (first elt)) acc)
           (push (first elt) acc))))
   (nreverse acc)))

(defun p12 (lst)
  (decode-run-length lst) )
(defun decode-run-length (lst)
  (flatten (mapcar #'decode-r-l lst)))
(defun decode-r-l (lst)
  (if (consp lst)
      (mklst lst)
      lst))

(defun mklst (lst)
  (make-n-elt-list (first lst) (second lst)))

(defun make-n-elt-list (n elt)
  (let (lst) (dotimes (i n lst)
      (push elt lst))))

(defun list-elt (elt)
  (if (consp elt)
      elt
      (list elt)))

(defun p13 (lst)
  (run-length-encode-direct (lst)))

(defun run-length-encode-direct (lst)
  (labels
      ((enc (acc count elt lst)
         (cond ((null lst)
                (if (>= count 2)
                    (push (list count elt) acc)
                    (push elt acc)))
               ((eql elt (first lst))
                (enc acc (1+ count) elt (rest lst)))
               (t (enc (push (if (> count 1)
                                 (list count elt)
                                 elt) acc)
                       1
                       (first lst)
                       (rest lst))))))
    (nreverse (enc nil 1 (first lst) (rest lst)))))

(defun enc-direct (acc count elt lst)
  (cond ((null lst)
                (if (> count 1)
                    (push (list count elt) acc)
                    (push elt acc)))
               ((eql elt (first lst))
                (enc-direct acc (1+ count) elt (rest lst)))
               (t (enc-direct (push (list count elt) acc)
                       1
                       (first lst)
                       (rest (rest lst))))))
(defun p13 (lst)
  (duplicate lst 2))

(defun duplicate (lst num)
  (let (acc)
    (dolist (elt lst)
      (dotimes (i num)
        (push elt acc)))
    (nreverse acc)))
(defun p14 (lst n)
  (duplicate lst n))

(defun p15 (lst n)
  "Drop every nth element of a list"
  (nreverse (drop-nth nil 1 lst n)))
(defun p16 (lst n)
  (drop-nth nil 1 lst n))
(defun drop-nth (acc count lst n)
  (if (null lst)
      acc
      (if (= 0 (mod count n))
          (drop-nth acc (1+ count) (rest lst) n)
          (drop-nth (push (car lst) acc) (1+ count) (rest lst) n ))))

(defun our-subseq (lst start &optional (end 0))
  (labels
      ((sq (lst start num-elts)
         (if (or (null lst) (= 0 num-elts))
             nil
             (if  (= start 0)
                 (cons (car lst) (sq (cdr lst) start (1- num-elts)))
                 (sq (rest lst) (1- start) num-elts)))))
    (sq lst start (- (if (= 0 end) (length lst) end) start))))

(defun p17 (lst len)
  (split lst len))
(defun p18 (lst start end)
  (our-subseq lst start end))
(defun split (lst len)
  (list (our-subseq lst 0 len)
        (our-subseq lst len)))
(defun p19 (lst num)
  (rotate lst num))

(defun rotate (lst num)
  (let ((sub (split lst num)))
    (append (second sub) (first sub))))
(defun p20 (lst num)
  (remove-at lst num))
(defun remove-at (lst num)
  (assert (>= num 1))
  (if (= num 1)
      (rest lst)
      (cons (first lst) (remove-at (rest lst) (- num 1) ))))