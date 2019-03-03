
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
  "Parse out a chess square from a string like \"e4\" or similar"
  (unless (eql (length string) 2)
    (error "Not a valid square-string '~a'" string))
  (let ((col (read-file (aref string 0) nil))
        (row (read-rank (aref string 1) nil)))
    (unless col
      (error "Invalid file for '~a'" string))
    (unless row
      (error "Invalid rank for '~a'" string))
    (make-square col row)))


(defun read-rank (character &optional (error t))
  "Returns the ROW-coordinate that a chess rank signifies"
  (let ((row (+ (char-code #\1) (- 7 (char-code character)))))
    (if (<= 0 row 7)
        row
        (when error
          (error "Invalid rank for '~a'" character)))))


(defun read-file (character &optional (error t))
  "Returns the COLUMN-coordinate that a chess file signifies"
  (let ((col (- (char-code character) (char-code #\a))))
    (if (<= 0 col 7)
        col
        (when error
          (error "Invalid file for '~a'" character)))))
