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

(defun make-matvec (vec dim)
  "Create a 2D matrix containing the vector in arr.
   dim is 0 for a column vector, 1 for a row vector."
  (let ((matvec))
    (ecase dim
      (0 (setf matvec (make-array (list (length vec) 1)))
       (loop for i from 0 below (nrows matvec) do
         (setf (aref matvec i 0) (aref vec i))))
      (1 (setf matvec (make-array (list 1 (length vec))))
       (loop for j from 0 below (ncols matvec) do
         (setf (aref matvec 0 j) (aref vec j)))))
    matvec))

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

(defun scal-mul (scal A)
  "Scalar-matrix multiplication."
  (let ((B (make-array (list (nrows A) (ncols A)))))
    (loop for i from 0 below (nrows B) do
      (loop for j from 0 below (ncols B) do
        (setf (aref B i j) (* scal (aref A i j)))))
    B))
