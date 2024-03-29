# Simple matrix operations
`mat-ops` implements simple matrix operations in Common Lisp.

## Initial scope
* The matrix elements are `number` (on which we can operate with *eg* `#'+`
  etc.).
* The matrices are 2D.
* The matrices are dense.
* We implement only simple operations. 'Simple' means anything which is
  unambiguous in result and for which there is no algorithmic tradeoff to
  perform between methods. Addition, multiplication, transposition are
  simple operations, matrix inversion is not.

## Implementation choices
* Matrices are represented with native 2D arrays. The first dimension is rows,
  the second dimension is columns.
* Vectors must be cast as matrices, this is for the sake of uniformity and to be
  able to distinguish between row and column vectors.
* Operations are functional by default.

## Usage
### Utilities
**nrows** mat => nrows

Returns the number of rows in the matrix.

**ncols** mat => ncols

Returns the number of columns in the matrix.

**transpose** mat => trans-mat

Transpose the matrix.

**make-matvec** vec dim => 2dvec

Generate a row or column 2D matrix from a 1D vector.
dim is 0 for a column vector, 1 for a row vector.

**ipiv-to-p** ipiv => p

Convert a swap vector ipiv (as given by LAPACK, indices also start from 1)
to a permutation vector, zero-indexed.

```common-lisp
(defparameter *a* #2A((1 2) (3 4) (-2 3)))
(nrows *a*)
;; => 3
(ncols *a*)
;; => 2
(transpose *a*)
;; => #2A((1 3 -2) (2 4 3))

(defparameter *vec* #(1 2 3 4))
(make-matvec *vec* 0)
;; => #2A((1) (2) (3) (4))
(make-matvec *vec* 1)
;; => #2A((1 2 3 4))

(ipiv-to-p #(3 3 3))
;; => #(2 0 1)
```

### Predicates
**squarep** a => T or nil

Is a matrix square? Taken as meaning same number of
rows and columns.

### Arithmetic
**mul** a b => c

Compute the matrix multiplication C = A.B
Error is signal upon dimension mismatch.

**scal-mul** scal a => b

Compute the scalar/matrix multiplication B = scal\*A.

```common-lisp
(defparameter *a* #2A((1 2) (3 4) (-2 3)))
(defparameter *b* #2A((3 3) (1 9)))
(mul *a* *b*)
;; => #2A((5 21) (13 45) (-3 21))
(scal-mul 4 *a*)
;; => #2A((4 8) (12 16) (-8 12))
```

## Tests
Launch tests with:

```common-lisp
(asdf:test-system "mat-ops")
```

## Dependencies
* `mat-ops`: None.
* `mat-ops/test`:
  * [rove](https://github.com/fukamachi/rove)

## References

## See also
* [array-operations](https://github.com/bendudson/array-operations)
