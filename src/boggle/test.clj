(ns boggle.test
  (:use [boggle.main]
        [boggle.dictionary]))

(def sample-board
  [\E \H \C \A
   \G \D \R \I
   \E \I \M \O
   \T \O \A \E])

(def the-dict
  (load-dictionary "/usr/share/dict/words"))

(defn results
  []
  (find-words the-dict sample-board))