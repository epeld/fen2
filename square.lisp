
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



(defun parse-square (string)
  (let ((col (- (char-code (aref string 0)) (char-code #\a)))
        (row (+ (char-code #\1) (- 7 (char-code (aref string 1))))))
    (unless (<= 0 col 7)
      (error "Invalid file for '~a'" string))
    (unless (<= 0 row 7)
      (error "Invalid rank for '~a'" string))
    (make-square col row)))
