(defpackage :mat-ops
  (:use :cl)
  (:export
   ;; Utilities
   #:nrows #:ncols
   #:transpose
   #:make-matvec
   ;; Arithmetic
   #:mul
   #:scal-mul))
