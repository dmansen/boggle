(ns boggle.board)

(defn make-board
  [vec]
  "given a vector, constructs a board out of it"
  vec)

(defn letters
  "gets the letters on the board, as a vector"
  [board]
  board)

(defn- board-length
  "one side of the board"
  [board]
  (int (Math/sqrt (count board))))

(defn letter-at
  [board [x y]]
  (board (+ x (* y (board-length board)))))

(defn neighbor-positions
  "Ugly function, so it's here to not look at again. Given a board and a position, returns a vector of all valid neighbor positions."
  [board [x y]]
  (filter (fn [[x y]]
            (and (> x -1)
                 (> y -1)
                 (< x (board-length board))
                 (< y (board-length board))))
          [[(dec x) y]
           [(dec x) (dec y)]
           [x (dec y)]
           [(inc x) (dec y)]
           [(inc x) y]
           [(inc x) (inc y)]
           [x (inc y)]
           [(dec x) (inc y)]]))

(defn- all-positions
  "returns a vector of all positions on the board"
  [board]
  (vec (for [y (range 0 (board-length board))
             x (range 0 (board-length board))]
         [x y])))

(defn- neighbors
  [board pos]
  (map
   (fn [pos] [(letter-at board pos) pos])
   (neighbor-positions board pos)))

(defn next-letter-options
  "returns a vector of possible positions to continue the trace"
  [board trace current-pos word]
  (vec (map second
            (let [ns (neighbors board current-pos)]
              (filter
               (fn [[letter pos]]
                 (and
                  (= (.charAt word 0) letter)
                  (not (some #{pos} trace))))
               ns)))))

(defn- letters-to-positions
  [board]
  (map (fn [c pos] [c pos])
       board
       (all-positions board)))

(defn trace-word-inner
  "Given a starting trace, finds all valid traces of that word on the board"
  [board trace word]
  (let [current-pos (last trace)]
   (if (empty? word)
     trace
     (apply concat
            (for [next-pos
                  (next-letter-options board trace current-pos word)]
              (trace-word-inner board
                                (conj trace next-pos)
                                (.substring word 1)))))))

(defn trace-word
  "Given a boggle board and a word, finds all ways of creating that word on the board"
  [board word]
  (for [[c pos] (letters-to-positions board)
        :when (= c (.charAt word 0))]
    (trace-word-inner board
                      [pos]
                      (.substring word 1))))