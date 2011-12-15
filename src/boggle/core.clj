(ns boggle.core)

(defn good-word?
  "checks if the string only has A-Z"
  [s]
  (and (java.util.regex.Pattern/matches "[A-Z]*" s)
       (> (.length s) 2)
       (< (.length s) 6)))

(defn add-word
  "adds a word to the dictionary"
  [dict word]
  (let [first-char (.charAt word 0)]
    (let [current-word-set
          (if (nil? (get dict first-char))
            #{}
            (get dict first-char))]
      (assoc dict first-char (conj current-word-set word)))))

(defn load-dictionary
  "Loads the dictionary from the specified file"
  [file]
  (let [file-buf (java.io.BufferedReader. (java.io.FileReader. file))]
    (loop [dict {}]
      (if (not (.ready file-buf))
        dict
        (let [new-word
              (.replace (.toUpperCase (.readLine file-buf)) "'" "")]
          (if (good-word? new-word)
            (recur (add-word dict new-word))
            (recur dict)))))))

(defn words-starting-with
  "List of words that start with the specified letter, from the dictionary provided."
  [dict char]
  (get dict char))

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

(defn neighbors
  "Given a board and a position, builds the neighbor set for the letter at that position on the board."
  [board [x y]]
  (map (fn [pos]
         {(letter-at board pos) pos})
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
                [(inc y) (dec x)]])))

(defn neighbor-map
  "builds the neighbor map for this board"
  [board]
  (vec (map (fn [pos]
              [(letter-at board pos) (neighbors board pos)])
            (all-positions board))))

(defn all-positions
  "returns a vector of all positions on the board"
  [board]
  (for [x (range 0 (board-length board))
        y (range 0 (board-length board))]
    [x y]))

(defn trace-word-inner
  [board trace word]
  (let [current-pos (last trace)
        first-char (.charAt word 0)]
    (let [cur-neighbors (neighbors board current-pos)]
      "not implemented")))