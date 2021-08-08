(defpackage :mat-ops
  (:use :cl)
  (:export
   ;; Utilities
   #:nrows #:ncols
   #:transpose
   ;; Arithmetic
   #:mul
   #:scal-mul))
