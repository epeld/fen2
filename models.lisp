

(deftype color ()
  '(or (eql :white) (eql :black)))

(deftype piece-type ()
  '(member :pawn :bishop :rook :queen :king :knight))

(deftype board ()
  '(array (or null piece) (64)))

(defun make-board ()
  (make-array '(64)
              :initial-element nil
              :element-type '(or null piece)))

(defstruct (piece)
  "Represents a chess piece"
  (type :pawn :type piece-type)
  (color :white :type color))


(defstruct (chess-position)
  "Represents a specific chess position"
  (board nil :type board)
  (turn :white :type color)
  (rights nil :type list)
  (passant nil)
  (half-move 0 :type number)
  (full-move 1 :type number))
