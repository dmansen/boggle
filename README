# boggle

A solver for boggle which uses persistent data structures and other cool crap.

## Usage

Make a board:

  (def sample-board
    (boggle.board/make-board 
      [\E \H \C \A
       \G \D \R \I
       \E \I \M \O
       \T \O \A \E]))

Load the dictionary:

  (def dict
    (boggle.dictionary/load-dictionary "/usr/share/dict/words"))

Solve it!

  (boggle.solver/find-words dict sample-board)

## License

Copyright (C) 2011 Derek Mansen

Distributed under the Eclipse Public License, the same as Clojure.
