(ns sudoto.matrix
  (:require
   [sudoto.state :as state]))

(defn match-nums [n nums]
  "Returns a bool determining whether n is in nums"
  (->> nums
       (filter #(= n %))
       first
       some?))

(defn get-box
  "Determines which box number in the matrix"
  [[x y]]
  (cond 
    (match-nums x [1 2 3]) (cond
                               (match-nums y [1 2 3]) 1
                               (match-nums y [4 5 6]) 2
                               (match-nums y [7 8 9]) 3)
    (match-nums x [4 5 6]) (cond
                               (match-nums y [1 2 3]) 4
                               (match-nums y [4 5 6]) 5
                               (match-nums y [7 8 9]) 6)
    (match-nums x [7 8 9]) (cond
                               (match-nums y [1 2 3]) 7
                               (match-nums y [4 5 6]) 8
                               (match-nums y [7 8 9]) 9)))

(defn get-cell
  "Determines which number in the box"
  [[x y]]
  (cond 
    (match-nums x [1 4 7]) (cond
                             (match-nums y [1 4 7]) 1
                             (match-nums y [2 5 8]) 2
                             (match-nums y [3 6 9]) 3)
    (match-nums x [2 5 8]) (cond
                             (match-nums y [1 4 7]) 4
                             (match-nums y [2 5 8]) 5
                             (match-nums y [3 6 9]) 6)
    (match-nums x [3 6 9]) (cond
                             (match-nums y [1 4 7]) 7
                             (match-nums y [2 5 8]) 8
                             (match-nums y [3 6 9]) 9)))

(defn get-row 
  "Determines nths in row"
  [box cell]
  (cond 
    (match-nums box [1 2 3]) (cond
                               (match-nums cell [1 2 3]) (range 0 9) 
                               (match-nums cell [4 5 6]) (range 9 18) 
                               (match-nums cell [7 8 9]) (range 18 27))
    (match-nums box [4 5 6]) (cond
                               (match-nums cell [1 2 3]) (range 27 36) 
                               (match-nums cell [4 5 6]) (range 36 45)
                               (match-nums cell [7 8 9]) (range 45 54))
    (match-nums box [7 8 9]) (cond
                               (match-nums cell [1 2 3]) (range 54 63) 
                               (match-nums cell [4 5 6]) (range 63 72)
                               (match-nums cell [7 8 9]) (range 72 81))))

(defn get-col 
  "Determines nths in col"
  [box cell]
  (cond 
    (match-nums box [1 4 7]) (cond
                               (match-nums cell [1 4 7]) (range 0 73 9) 
                               (match-nums cell [2 5 8]) (range 1 74 9) 
                               (match-nums cell [3 6 9]) (range 2 75 9))
    (match-nums box [2 5 8]) (cond
                               (match-nums cell [1 4 7]) (range 3 76 9)
                               (match-nums cell [2 5 8]) (range 4 77 9) 
                               (match-nums cell [3 6 9]) (range 5 78 9))
    (match-nums box [3 6 9]) (cond
                               (match-nums cell [1 4 7]) (range 6 79 9)
                               (match-nums cell [2 5 8]) (range 7 80 9) 
                               (match-nums cell [3 6 9]) (range 8 81 9))))

(defn nums [n]
  (map #(+ n %)  [0 1 2]))

(defn get-box-nths [n]
  "Returns a list of nths found in each box in the matrix array"
  (flatten (map nums [(+ n 0) (+ n 9) (+ n 18)])))

(defn box-cells [box]
  "Returns a list of all cells (nths) located in each box"
  (case box
    1 (get-box-nths 0)
    2 (get-box-nths 3)
    3 (get-box-nths 6)
    4 (get-box-nths 27)
    5 (get-box-nths 30)
    6 (get-box-nths 33)
    7 (get-box-nths 54)
    8 (get-box-nths 57)
    9 (get-box-nths 60)))

(defn get-matrix-nth
  "Determines nth position in matrix for a given box and cell"
  [box cell]
  (nth (box-cells box) (- cell 1)))

(defn duplicate?
  "Given x y coords, checks box, row, and column to make sure val is not duplicated in any"
  [x y val]
  (let [box       (get-box [x y])
        cell      (get-cell [x y])
        cells     (box-cells box)
        row       (get-row box cell)
        col       (get-col box cell)
        box-state (->> cells
                       (map #(nth (:board @state/state) %))
                       (remove zero?))
        row-state (->> row 
                       (map #(nth (:board @state/state) %))
                       (remove zero?))
        col-state (->> col 
                       (map #(nth (:board @state/state) %))
                       (remove zero?))
        res       (or (< 1 (count (filter #(= val %) box-state)))
                      (< 1 (count (filter #(= val %) row-state)))
                      (< 1 (count (filter #(= val %) col-state))))]
    res))
