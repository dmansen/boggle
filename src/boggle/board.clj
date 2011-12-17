(ns boggle.board)

(defn make-board
  [vec]
  "Given a vector, constructs a board out of it"
  vec)

(defn letters
  "Gets the letters on the board, as a vector"
  [board]
  board)

(defn length
  "Length of one side of the board"
  [board]
  (int (Math/sqrt (count board))))

(defn letter-at
  "Given a board and an [x y] coordinate, returns the character in that position"
  [board [x y]]
  (board (+ x (* y (length board)))))