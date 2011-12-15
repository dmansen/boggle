(ns boggle.dictionary)

(defn good-word?
  "checks if the string only has A-Z"
  [s]
  (and (java.util.regex.Pattern/matches "[A-Z]*" s)
       (> (.length s) 2)
       (< (.length s) 6)))

(defn words-starting-with
  "List of words that start with the specified letter, from the dictionary provided."
  [dict char]
  (get dict char))

(defn empty-word-set
  []
  #{})

(defn add-word
  "adds a word to the dictionary"
  [dict word]
  (let [first-char (.charAt word 0)]
    (let [current-word-set
          (if (nil? (words-starting-with dict first-char))
            (empty-word-set)
            (words-starting-with dict first-char))]
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