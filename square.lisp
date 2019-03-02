
(defun make-square (row column)
  "Construct a square from a row and a column"
  (assert (< row 8))
  (assert (< column 8))
  (the square
       (+ column (* row 8))))

(defun square-row (square)
  "Return the row-coordinate of a square"
  (the coord
       (floor square 8)))


(defun square-column (square)
  "Return the column-coordinate of a square"
  (the coord
       (mod square 8)))

