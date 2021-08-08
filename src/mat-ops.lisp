;;;; Simple matrix operations
(in-package :mat-ops)

;;; Utilities
(defun nrows (A)
  "Number of rows in matrix A."
  (array-dimension A 0))

(defun ncols (A)
  "Number of columns in matrix A."
  (array-dimension A 1))

(defun transpose (A)
  "Transpose the matrix A."
  (let ((B (make-array (list (ncols A) (nrows A)))))
    (loop for i from 0 below (nrows B) do
      (loop for j from 0 below (ncols B) do
        (setf (aref B i j) (aref A j i))))
    B))

;; (defun make-vec )

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

(defun scal-mul (scal A)
  "Scalar-matrix multiplication."
  (let ((B (make-array (list (nrows A) (ncols A)))))
    (loop for i from 0 below (nrows B) do
      (loop for j from 0 below (ncols B) do
        (setf (aref B i j) (* scal (aref A i j)))))
    B))
