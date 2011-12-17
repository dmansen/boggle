(ns boggle.main
  (:use [boggle.board]
        [boggle.dictionary]))

(defn elem
  [x list]
  (not (nil? (some #{x} list))))

(defn find-words
  [dict board]
  (for [c (keys dict)
        :when (elem c (letters board))
        word (words-starting-with dict c)
        :let [traces (trace-word board word)]
        :when (not (empty? (flatten traces)))]
    [word traces]))