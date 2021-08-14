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

(defun ipiv-to-p (ipiv)
  "Compute a permutation vector from a sequential swap vector ipiv.
   ipiv are the swap vectors given by eg LAPACK.
   Also ipiv indices start from 1 and we want p to start from 0.
   Exemple: ipiv = [3 3 3] means:
     * Swap row 1 with row 3
     * Then swap row 2 with row 3.
     * Do nothing on row 3.
   The result permutation vector is p = [2 0 1]"
  (let ((p (make-array (length ipiv))))
    ;; Initialize p
    (loop for i from 0 below (length p) do (setf (aref p i) i))
    ;; Swap p elements according to ipiv.
    (loop for swap across ipiv
          for i from 0 below (length p) do
            (rotatef (aref p i) (aref p (1- swap))))
    p))

;;; Predicates
(defun squarep (A)
  "Is A square?"
  (= (nrows A) (ncols A)))

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
