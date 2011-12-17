(ns boggle.test
  (:use [boggle.main]
        [boggle.dictionary]
        [boggle.board]))

(defn create-sample-board
  []
  (make-board [\E \H \C \A
               \G \D \R \I
               \E \I \M \O
               \T \O \A \E]))

(defn create-dict
  []
  (load-dictionary "/usr/share/dict/words"))

(defn trace-accurate?
  [board trace word]
  (cond
   (empty? word) true
   (empty? trace) false
   (not= (letter-at board (first trace)) (.charAt word 0)) false
   true (trace-accurate? board (rest trace) (.substring word 1))))

(defn audit-results
  [board results]
  (for [[word traces] results
        trace traces
        :when (not (trace-accurate? board trace word))]
    [:fail trace word]))