(ns boggle.board)

(defn make-board
  [vec]
  "given a vector, constructs a board out of it"
  vec)

(defn letters
  "gets the letters on the board, as a vector"
  [board]
  board)

(defn length
  "one side of the board"
  [board]
  (int (Math/sqrt (count board))))

(defn letter-at
  [board [x y]]
  (board (+ x (* y (length board)))))