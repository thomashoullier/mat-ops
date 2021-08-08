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
* Vectors must be cast as matrices, this is for sake of uniformity and to be
  able to distinguish between row and column vectors.

## Usage
### Utilities
**nrows** mat => nrows

Returns the number of rows in the matrix.

**ncols** mat => ncols

Returns the number of columns in the matrix.

```common-lisp
(defparameter *a* #2A((1 2) (3 4) (-2 3)))
(nrows *a*)
;; => 3
(ncols *a*)
;; => 2
```

### Arithmetic
**mul** a b => c

Compute the matrix multiplication C = A.B
Error is signal upon dimension mismatch.

```common-lisp
(defparameter *a* #2A((1 2) (3 4) (-2 3)))
(defparameter *b* #2A((3 3) (1 9)))
(mul *a* *b*)
;; => #2A((5 21) (13 45) (-3 21))
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
