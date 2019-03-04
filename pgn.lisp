
(deftype move-type ()
  '(member :moves :takes))


(deftype partial-square ()
  '(cons (or null coord) (or null coord)))


(defun row-source (row)
  (cons nil row))

(defun column-source (column)
  (cons column nil))

(defun column-source-p (source)
  (and (consp source)
       (not (null (car source)))))

(defun row-source-p (source)
  (and (consp source)
       (not (null (cdr source)))))


(defstruct (pgn-move)
  (piece-type :pawn :type piece-type)
  (move-type :moves :type move-type)
  (destination nil :type square)
  (source nil :type (or null partial-square square)))


(defun parse-move (string)
  (cond
    ((string= string "O-O")
     :kingside)

    ((string= string "O-O-O")
     :queenside)

    (t
     (parse-normal-move string))))


(defun parse-normal-move (string)
  (if (member (aref string 0) (coerce "abcdefgh" 'list))
      (parse-pawn-move string)
      (let ((piece-type (piece-type-from-character (aref string 0))))
        (cond
          ((<= (length string) 3)
           (make-pgn-move :piece-type piece-type
                          :move-type :moves
                          :destination (parse-square (subseq string 1 3))
                          :source nil))
          
          ((eql #\x (aref string 1))
           (make-pgn-move :piece-type piece-type
                          :move-type :takes
                          :destination (parse-square (subseq string 2 4))
                          :source nil))
          
           (t
            (multiple-value-bind (source pos) (parse-move-source string 1)

              ;; An 'x' signifies :takes (and takes up a character position)
              (let (move-type)
                (if (eql #\x (aref string pos))
                    ;; Increase pos to make the rest of the parsing skip #\x
                    (progn (incf pos 1)
                           (setf move-type :takes))

                    ;; No character to skip
                    (setf move-type :moves))
                
                (make-pgn-move :piece-type piece-type
                             :move-type move-type
                             :destination (parse-square (subseq string pos (+ 2 pos)))
                             :source source))))))))


(defun parse-move-source (string &optional (offset 0))
  ;; If the second char isn't a chess rank..
  (if (null (read-rank (aref string (1+ offset)) nil))
      ;; .. then the source is only given as a file/rank
      (cond ((read-file (aref string offset) nil)
             (values (column-source (read-file (aref string offset))) (1+ offset)))

            ((read-rank (aref string offset) nil)
             (values (row-source (read-rank (aref string offset))) (1+ offset)))

            (t
             (error "Invalid source in move '~a'" string)))

      ;; otherwise, the source is given as a square
      (values (parse-square (subseq string offset (+ 2 offset))) (+ 2 offset))))


(defun parse-pawn-move (string)
  (let ((ch (aref string 0))
        (ch2 (aref string 1)))
    (cond ((eql #\x ch2)
           (make-pgn-move :piece-type :pawn
                          :move-type :takes
                          :destination (parse-square (subseq string 2 4))
                          :source (column-source (read-file ch))))

          ((read-rank ch2 nil)
           (cond ((eql 2 (length string))
                  (make-pgn-move :piece-type :pawn
                                 :move-type :moves
                                 :destination (make-square (read-file ch) (read-rank ch2))
                                 :source nil))

                 ((and (eql #\x (aref string 2))
                       (eql 5 (length string)))
                  (make-pgn-move :piece-type :pawn
                                 :move-type :takes
                                 :destination (parse-square (subseq string 3 5))
                                 :source (make-square (read-file ch) (read-rank ch2))))

                 ((eql 4 (length string))
                  (make-pgn-move :piece-type :pawn
                                 :move-type :moves
                                 :destination (parse-square (subseq string 2 4))
                                 :source (make-square (read-file ch) (read-rank ch))))

                 (t
                  (error "Invalid pawn move '~s'" string)))))))

