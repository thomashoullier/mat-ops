;;;; Simple matrix operations
(in-package :mat-ops)

;;; Utilities
(defun nrows (A)
  "Number of rows in matrix A."
  (array-dimension A 0))

(defun ncols (A)
  "Number of columns in matrix A."
  (array-dimension A 1))

;;; Arithmetic
(defun mul (A B)
  "Matrix multiplication A.B"
  (when (/= (ncols A) (nrows B)) (error "mul: matrix dimension mismatch."))
  (let* ((nrows-c (nrows A)) (ncols-c (ncols B))
         (nsums (nrows B))
         (C (make-array (list nrows-c ncols-c))))
    (loop for i from 0 below nrows-c do
      (loop for j from 0 below ncols-c do
        (setf (aref C i j)
              (loop for k from 0 below nsums
                    sum (* (aref A i k) (aref B k j))))))
    C))

;; (defun add (A B)
;;   "Matrix element-wise addition A+B. The matrices must have the same size."
;;   (when (not (equal (array-dimensions A) (array-dimensions B)))
;;     (error "add: matrices must be of identical size."))

;;   )
