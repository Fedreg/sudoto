(ns sudoto.state
  (:require
   [reagent.core :as r]
   [sudoto.audio :as audio]))

(def board
  [0 9 0  1 5 0  0 0 0
   5 0 0  0 0 0  3 8 9
   4 0 0  0 0 0  0 0 0

   1 0 2  0 0 0  0 9 0
   0 0 4  0 0 0  1 0 0
   7 0 9  0 3 0  4 5 6

   2 3 0  5 6 4  8 0 7
   0 6 0  0 0 9  0 3 4
   0 8 0  2 0 1  0 0 0])

;; (def board
;;   [
;;    1 2 3 4 5 6 7 8 9
;;    4 5 6 7 8 9 1 2 3
;;    1 2 3 4 5 6 7 8 9
;;    2 3 4 1 2 3 5 6 7
;;    4 5 6 7 8 9 1 2 3
;;    1 2 3 4 5 6 7 8 9
;;    4 5 6 7 8 9 1 2 3
;;    2 3 4 1 2 3 5 6 7
;;    4 5 6 7 8 9 1 2 3
;;    ])

(def state
  (r/atom {:board board
           :bpm 120
           :mode (:phrygian audio/modes)}))

