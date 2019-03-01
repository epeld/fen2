

(defun make-square (row column)
  "Construct a square from a row and a column"
  (assert (< row 8))
  (assert (< column 8))
  (+ column (* row 8)))

(defun square-row (square)
  "Return the row-coordinate of a square"
  (floor square 8))


(defun square-column (square)
  "Return the column-coordinate of a square"
  (mod square 8))

