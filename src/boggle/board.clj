(ns boggle.board)

(defn- letters
  "gets the letters on the board"
  [board]
  board)

(defn- board-length
  "one side of the board"
  [board]
  (int (Math/sqrt (count board))))

(defn- letter-at
  [board [x y]]
  (board (+ x (* y (board-length board)))))

(defn- neighbor-positions
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

(defn- next-letter-options
  "returns a vector of possible positions to continue the trace"
  [board trace current-pos word]
  (vec (map second
            (let [ns (neighbors sample-board current-pos)]
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

(defn- comp-n
  "composes f with the argument n times"
  [f n x]
  (if (= 0 n)
    x
    (recur f (dec n) (f x))))

(defn- trace-word-inner
  [board trace current-pos word]
  (if (empty? word)
    (conj trace current-pos)
    (for [next-pos
          (next-letter-options board trace current-pos word)]
      (trace-word-inner board
                        (conj trace current-pos)
                        next-pos
                        (.substring word 1)))))

(defn- trace-word-unwrapper
  [board trace current-pos word]
  "need to do this because the inner wrapper keeps wrapping the results in more and more lists...this is probably a dumb way of doing this"
  (vec (comp-n first (.length word)
               (trace-word-inner board trace current-pos word))))

(defn trace-word
  [board word]
  (vec (for [[c pos] (letters-to-positions board)
             :when (= c (.charAt word 0))
             :let [trace (trace-word-unwrapper board
                                               []
                                               pos
                                               (.substring word 1))]
             :when (not (empty? trace))]
         trace)))