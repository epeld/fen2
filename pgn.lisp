
(deftype move-type ()
  '(member :moves :takes))

(defstruct (pgn-move)
  (piece-type :pawn :type piece-type)
  (move-type :moves :type move-type)
  (destination nil :type square)
  (source nil :type (or null coord square)))
