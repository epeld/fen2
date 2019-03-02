

(defparameter example "rnbqkbnr/pppppppp/8/8/4P3/8/PPPP1PPP/RNBQKBNR b KQkq e3 0 1")


(defun color-from-character (ch)
  "Uses a character's case to determine if representing a black or white piece"
  (the character ch)
  (the color
       (if (char= (char-upcase ch) ch) :white :black)))



(defun piece-type-from-character (ch)
  "Determine which FEN-piece type this character represents"
  (the character ch)
  (the piece-type
       (ecase (char-downcase ch)
         (#\p :pawn)
         (#\b :bishop)
         (#\r :rook)
         (#\q :queen)
         (#\k :king)
         (#\n :knight))))


(defun piece-from-character (ch)
  "Construct a piece from a character, e.g #\P -> (:pawn :white)"
  (the character ch)
  (make-piece (piece-type-from-character ch) (color-from-character ch)))


(defun fen-blank-p (ch)
  "Return how many blanks a given character represents (or NIL if none)"
  (let ((ix (search `(,ch) "12345678")))
    (when ix
      (1+ ix))))



(defun board (fen)
  "Read out all pieces into a 2d array"
  (the string fen)
  (loop for ch across fen
     with pieces = (the board (make-board))
     with rows = 0
     with cols = 0

     until (or (and (eql 7 rows) (<= 7 cols))
	       (< 7 rows))
       
     if (char= ch #\/)
     do (progn (assert (eql 8 cols))
	       (setf cols 0)
	       (incf rows))
       
     else 
     do (let ((blanks (fen-blank-p ch)))
	  (unless blanks
	    (setf (aref pieces (make-square rows cols)) (piece-from-character ch)))
	  (incf cols (or blanks 1)))
      
     finally (progn (assert (eql 7 cols))
		    (assert (eql 7 rows))
		    (return pieces))))


(defun string-part (num string &optional (separator #\Space))
  "Given a separator, extract the part of the string that comes after num separators"
  (loop for ch across string
     with counter = 0
     with ix = 0

     when (eql counter num)
     do (return (subseq string ix (or (position separator string :start ix)
				      (length string))))
       
     do (incf ix)

     if (char= ch separator)
     do (progn (incf counter))))


(defun turn (fen)
  "Extract whose turn it is from a fen string"
  (let ((part (string-part 1 fen)))
    
    (unless (< 0 (length part))
      (error "Invalid turn-part in string ~s" fen))
    
    (ecase (aref part 0)
      (#\b :black)
      (#\w :white))))


(defun castling-side (ch)
  "Determine which castlingside a given character represents"
  (ecase (char-downcase ch)
    (#\k :kingside)
    (#\q :queenside)))


(defun right (ch)
  "Convert a character to the castling right it represents"
  (the character ch)
  (list (castling-side ch) (color ch)))


(defun rights (fen) 
  "Extract the castling rights from a FEN string"
  (mapcar #'right (coerce (string-part 2 fen) 'list)))


(defun file-coordinate (ch)
  "Character to rank coordiante conversion"
  (the character ch)
  (or (position ch "abcdefgh")
      (error "Invalid file ~a" ch)))


(defun rank-coordinate (ch)
  "Character to rank coordiante conversion"
  (the character ch)
  (or (position ch "12345678")
      (error "Invalid rank ~a" ch)))


;; TODO move to some square module later on
(defun square-coordinates (square)
  "String to square coordiante conversion"
  (assert (eql 2 (length square)))
  (list (file-coordinate (aref square 0))
	(rank-coordinate (aref square 1))))


(defun coordinates-to-square (coords)
  "Convert a list of two coordinates into a square string"
  (let ((f (first coords))
	(r (second coords)))
    (assert (<= 0 f 7))
    (assert (<= 0 r 7))
    (coerce (list (aref "abcdefgh" f)
		  (aref "12345678" r))
	    'string)))


(defun verify-square (str)
  "Sanity check for squares"
  (assert (string= str (coordinates-to-square (square-coordinates str))))
  t)


(defun passant-square (fen) 
  "Extract the passant square from a FEN string"
  (let ((square (string-part 3 fen)))
    (unless (string= "-" square)
      (square-coordinates square))))


(defun half-move (fen)
  "Extract the half-move count from a FEN string"
  (parse-integer (string-part 4 fen)))


(defun full-move (fen)
  "Extract the full-move count from a FEN string"
  (parse-integer (string-part 5 fen)))


(defun parse (fen)
  "Parse a fen string into a plist containing all its information"
  (make-chess-position :board (board fen)
	               :turn (turn fen)
	               :rights (rights fen)
	               :passant (passant-square fen)
	               :half-move (half-move fen)
	               :full-move (full-move fen)))


;;
;; Encoding routines
;; 

(defun half-move-string (pos)
  "Encode the half-move of a position as a string"
  (write-to-string (getf pos :half-move)))


(defun full-move-string (pos)
  "Encode the half-move of a position as a string"
  (write-to-string (getf pos :full-move)))


(defun passant-square-string (pos)
  "Encode the passant square of a position as a string"
  (let ((coords (getf pos :passant)))
    (if coords
	(coordinates-to-square coords)
	"-")))


(defun turn-string (pos)
  "Encode the turn part of a position as a string"
  (ecase (getf pos :turn)
    (:white "w")
    (:black "b")))


(defun right-char (right)
  (let ((ch (ecase (first right) (:kingside #\k) (:queenside #\q))))
    (if (eq :white (second right))
	(char-upcase ch)
	ch)))


(defun rights-string (pos)
  "Encode the castling rights of a position as a string"
  (coerce (mapcar #'right-char (getf pos :rights))
	  'string))





