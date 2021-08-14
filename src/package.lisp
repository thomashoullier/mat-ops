(defpackage :mat-ops
  (:use :cl)
  (:export
   ;; Utilities
   #:nrows #:ncols
   #:transpose
   #:make-matvec
   #:ipiv-to-p
   ;; Predicates
   #:squarep
   ;; Arithmetic
   #:mul
   #:scal-mul))
