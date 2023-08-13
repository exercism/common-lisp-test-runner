(cl:defpackage :all-fail
  (:use :common-lisp)
  (:export :leap-year-p))

(in-package :all-fail)

(defun divisible-by-p (n d) (= 0 (rem n d)))

(defun leap-year-p (year)
  (not
    (and (divisible-by-p year 4)
        (or (not (divisible-by-p year 100))
      (divisible-by-p year 400)))))
  
