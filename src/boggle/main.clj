(ns boggle.main
  (:use [boggle.board]
        [boggle.dictionary]))

(defn find-words
  [dict board]
  (for [c (keys dict)
        :when (some #{c} (letters board))
        word (words-starting-with dict c)
        :let [traces (trace-word board word)]
        :when (not (empty? (flatten traces)))]
    [word traces]))