(ns boggle.board)

(def sample-board
  [\E \H \C \A
   \G \D \R \I
   \E \I \M \O
   \T \O \A \E])

(defn board-length
  "one side of the board"
  [board]
  (int (Math/sqrt (count board))))

(defn letter-at
  [board [x y]]
  (board (+ x (* y (board-length board)))))

(def letter-and-neighbors-at letter-at)

(defn neighbor-positions
  "Ugly function, so it's here to not look at again. Given a board and a position, returns a vector of all valid neighbor positions."
  [board [x y]]
  (filter (fn [[x y]]
            (and (> x -1)
                 (> y -1)
                 (< x (board-length board))
                 (< y (board-length board))))
          [[y (dec x)]
           [(dec y) (dec x)]
           [(dec y) x]
           [(dec y) (inc x)]
           [y (inc x)]
           [(inc y) (inc x)]
           [(inc y) x]
           [(inc y) (dec x)]]))

(defn neighbors
  "Given a board and a position, builds the neighbor set for the letter at that position on the board."
  [board pos]
  (vec (map (fn [p]
              [(letter-at board p) p])
            (neighbor-positions board pos))))

(defn all-positions
  "returns a vector of all positions on the board"
  [board]
  (vec (for [x (range 0 (board-length board))
             y (range 0 (board-length board))]
         [x y])))

(defn neighbor-map
  "builds the neighbor map for this board"
  [board]
  (vec (map (fn [pos]
              [(letter-at board pos) (neighbors board pos)])
            (all-positions board))))

(defn neighbors-at
  [board pos]
  ((letter-and-neighbors-at (neighbor-map board) pos) 1))

(defn next-letter-options
  "returns a vector of possible positions to continue the trace"
  [board current-pos word]
  (vec (map #(% 1)
            (let [ns (neighbors-at sample-board current-pos)]
              (filter
               (fn [[letter pos]]
                 (= (.charAt word 0) letter))
               ns)))))

(defn trace-word-inner
  [board trace current-pos word]
  (if (empty? word)
    trace
    (let [opts (next-letter-options board current-pos word)]
      (if (empty? opts)
        nil
        (recur board
               (conj trace (opts 0))
               (opts 0)
               (.substring word 1))))))

(defn letters-to-positions
  [board]
  (map (fn [x y] [x y])
       board
       (all-positions board)))

(defn trace-word
  [board word]
  (let [starting-options
        (filter (fn [[c pos]]
                  (= c (.charAt word 0))) (letters-to-positions board))]
    (map
     (fn [[c pos]]
       (trace-word-inner board [pos] pos (.substring word 1)))
     starting-options)))