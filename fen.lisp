

(defparameter example "rnbqkbnr/pppppppp/8/8/4P3/8/PPPP1PPP/RNBQKBNR b KQkq e3 0 1")


(defun color (ch)
  "Uses a character's case to determine if representing a black or white piece"
  (the character ch)
  (if (char= (char-upcase ch) ch) :white :black))


(defun piece-type (ch)
  "Determine which FEN-piece type this character represents"
  (the character ch)
  (ecase (char-downcase ch)
    (#\p :pawn)
    (#\b :bishop)
    (#\r :rook)
    (#\q :queen)
    (#\k :king)
    (#\n :knight)))


(defun piece (ch)
  "Construct a piece from a character, e.g #\P -> (:pawn :white)"
  (the character ch)
  (list (piece-type ch) (color ch)))


(let ((digits (coerce "12345678" 'list)))
  
  (defun fen-blank-p (ch)
    "Return whether the argument represents a blank sequence of characters"
    (the character ch)
    (let ((match (member ch digits)))
      (when match
	(+ 1 (- 8 (length match)))))))


(defun pieces (fen)
  "Read out all pieces into a 2d array"
  (the sequence fen)
  (loop for ch across fen
     with pieces = (make-array '(8 8) :initial-element nil)
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
	    (setf (aref pieces rows cols) (piece ch)))
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


(defun passant-square (fen) 
  "Extract the passant square from a FEN string"
  (square-coordinates (string-part 3 fen)))


(defun half-move (fen)
  "Extract the half-move count from a FEN string"
  (parse-integer (string-part 4 fen)))


(defun full-move (fen)
  "Extract the full-move count from a FEN string"
  (parse-integer (string-part 5 fen)))


(defun parse (fen)
  "Parse a fen string into a plist containing all its information"
  (list :pieces (pieces fen)
	:turn (turn fen)
	:rights (rights fen)
	:passant (passant-square fen)
	:half-move (half-move fen)
	:full-move (full-move fen)))

