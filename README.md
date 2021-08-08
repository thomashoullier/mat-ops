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

## Tests

## Dependencies

## References