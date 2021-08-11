(defpackage :mat-ops
  (:use :cl)
  (:export
   ;; Utilities
   #:nrows #:ncols
   #:transpose
   #:make-matvec
   ;; Predicates
   #:squarep
   ;; Arithmetic
   #:mul
   #:scal-mul))
