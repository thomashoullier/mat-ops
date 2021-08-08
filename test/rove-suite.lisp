;;;; Rove test suite for mat-ops.
(defpackage :mat-ops/test
  (:use :cl :rove))
(in-package :mat-ops/test)

(let ((a #2A((1 2) (3 4) (-2 3))))
  (deftest utilities
    (testing "nrows"
      (ok (= 3 (mat-ops:nrows a)) ""))
    (testing "ncols"
      (ok (= 2 (mat-ops:ncols a)) ""))))
