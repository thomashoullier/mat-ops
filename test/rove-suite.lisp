;;;; Rove test suite for mat-ops.
(defpackage :mat-ops/test
  (:use :cl :rove))
(in-package :mat-ops/test)

(let ((a #2A((1 2) (3 4) (-2 3)))
      (b #2A((3 3) (1 9)))
      (c #2A((5 21) (13 45) (-3 21)))
      (d #2A((4 8) (12 16) (-8 12)))
      (e #2A((1 3 -2) (2 4 3)))
      (vec #(1 2 3 4))
      (colvec #2A((1) (2) (3) (4)))
      (rowvec #2A((1 2 3 4))))
  (deftest utilities
    (testing "nrows"
      (ok (= 3 (mat-ops:nrows a)) ""))
    (testing "ncols"
      (ok (= 2 (mat-ops:ncols a)) ""))
    (testing "transpose"
      (ok (equalp e (mat-ops:transpose a)) ""))
    (testing "matvec"
      (ok (and (equalp colvec (mat-ops:make-matvec vec 0))
               (equalp rowvec (mat-ops:make-matvec vec 1))) ""))
    (testing "ipiv-to-p"
      (ok (equalp #(2 0 1) (mat-ops:ipiv-to-p #(3 3 3))) "")))
  (deftest predicates
    (testing "squarep"
      (ok (and (mat-ops:squarep b)
               (not (mat-ops:squarep a))) "")))
  (deftest arithmetic
    (testing "mul"
      (ok (equalp c (mat-ops:mul a b)) ""))
    (testing "scal-mul"
      (ok (equalp d (mat-ops:scal-mul 4 a)) ""))))
