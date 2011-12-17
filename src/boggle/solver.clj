(ns boggle.solver
  (:use [boggle.board]
        [boggle.dictionary]))

(defn- neighbor-positions
  "Ugly function, so it's here to not look at again. Given a board and a position, returns a vector of all valid neighbor positions."
  [board [x y]]
  (filter (fn [[x y]]
            (and (> x -1)
                 (> y -1)
                 (< x (length board))
                 (< y (length board))))
          [[(dec x) y]
           [(dec x) (dec y)]
           [x (dec y)]
           [(inc x) (dec y)]
           [(inc x) y]
           [(inc x) (inc y)]
           [x (inc y)]
           [(dec x) (inc y)]]))

(defn- positions
  "returns a seq of all positions on the board"
  [board]
  (for [y (range 0 (length board))
        x (range 0 (length board))]
    [x y]))

(defn- neighbors
  [board pos]
  (map
   #(vector (letter-at board %) %)
   (neighbor-positions board pos)))

(defn- valid-choices
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

(defn- letter-positions
  [board]
  (map #(vector %1 %2)
       board
       (positions board)))

(defn- trace-word-from
  "Given a starting trace, finds all valid traces of that word on the board"
  [board trace word]
  (if (empty? word)
    [trace]
    (apply
     concat
     (for [next-pos
           (valid-choices board
                          trace
                          (last trace)
                          word)]
       (trace-word-from board
                        (conj trace next-pos)
                        (.substring word 1))))))

(defn trace-word
  "Given a boggle board and a word, finds all ways of creating that word on the board"
  [board word]
  (apply concat (for [[c pos] (letter-positions board)
                      :when (= c (.charAt word 0))]
                  (trace-word-from board
                                   [pos]
                                   (.substring word 1)))))

(defn find-words
  "Given a dictionary and a word, returns a map from each valid word to a list of ways to find that word"
  [dict board]
  (let [word-vec (for [c (keys dict)
                       :when (some #{c} (letters board))
                       word (words-starting-with dict c)
                       :let [traces (trace-word board word)]
                       :when (not (empty? (flatten traces)))]
                   [word traces])]
    ; This part creates the map from the [[word traces]..] vector
    (zipmap (map first word-vec) (map second word-vec))))