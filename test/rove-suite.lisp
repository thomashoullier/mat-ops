;;;; Rove test suite for mat-ops.
(defpackage :mat-ops/test
  (:use :cl :rove))
(in-package :mat-ops/test)

(let ((a #2A((1 2) (3 4) (-2 3)))
      (b #2A((3 3) (1 9)))
      (c #2A((5 21) (13 45) (-3 21))))
  (deftest utilities
    (testing "nrows"
      (ok (= 3 (mat-ops:nrows a)) ""))
    (testing "ncols"
      (ok (= 2 (mat-ops:ncols a)) "")))
  (deftest arithmetic
    (testing "mul"
      (ok (equalp c (mat-ops:mul a b)) ""))))
