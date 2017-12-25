(ns sudoto.core
  (:require
   [reagent.core :as r]
   [clojure.set  :as set]
   ;; [sudoto.audio :refer [play]]
   ))

(def board
  [1 2 3  4 5 6  7 8 9
   1 2 3  4 5 6  7 8 9
   1 2 3  4 5 6  7 8 9

   1 2 3  4 5 6  7 8 9
   1 2 3  4 5 6  7 8 9
   1 2 3  4 5 6  7 8 9

   1 2 3  4 5 6  7 8 9
   1 2 3  4 5 6  7 8 9
   1 2 3  4 5 6  7 8 9])

(def board-state (r/atom board))

(def possible-nums (set (range 1 10)))

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
    (match-nums x [1 2 3]) (cond
                             (match-nums y [1 4 7]) 1
                             (match-nums y [2 5 8]) 2
                             (match-nums y [3 6 9]) 3)
    (match-nums x [4 5 6]) (cond
                             (match-nums y [1 4 7]) 4
                             (match-nums y [2 5 8]) 5
                             (match-nums y [3 6 9]) 6)
    (match-nums x [7 8 9]) (cond
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
  (let [box   (get-box [x y])
        cell  (get-cell [x y])
        cells (box-cells box)
        row   (get-row box cell)
        col   (get-col box cell)
        pos   (get-matrix-nth box cell)
        state (nth @board-state pos)]

    (if (or (contains? (set cells) val)
            (contains? (set row)   val)
            (contains? (set col)   val))
      true
      false)))

; Test funcs
(defn initialize-board []
  (let [val (for [x (range 1 10)
                  y (range 1 10)]
              (if (= 2 (rand-int 3)) y 0))
        res (into [] val)]
    (reset! board-state val)))

;; -------------------------
;; Styles 
;; -------------------------

(defn container-style []
  {:style
   {:display "flex"
    :flex-wrap "wrap"
    :width "318px"
    :height "312px"
    :border "1px solid black"}})

(defn box-style []
  {:style
   {:display "flex"
    :flex-wrap "wrap"
    :width "102px"
    :height "100px"
    :border "2px solid black"}})

(defn cell-style []
  ;; {:style
   {:height "30px"
    :width "30px"
    ;; :background-color (cell-color box num)
    :text-align "center"
    :border "1px solid black"})

;; --------------------------
;; Views 
;; --------------------------

(defn cell [val]
  [:input {:style (cell-style)
           :value val }])
           ;; :on-change #(swap! box-state update-in [box] assoc num (js/parseInt (-> % .-target .-value)))}])

;; (defn box [y]
;;   [:div (box-style)
;;    (cell (get-in @box-state [y :1]) y :1)
;;    (cell (get-in @box-state [y :2]) y :2)
;;    (cell (get-in @box-state [y :3]) y :3)
;;    (cell (get-in @box-state [y :4]) y :4)
;;    (cell (get-in @box-state [y :5]) y :5)
;;    (cell (get-in @box-state [y :6]) y :6)
;;    (cell (get-in @box-state [y :7]) y :7)
;;    (cell (get-in @box-state [y :8]) y :8)
;;    (cell (get-in @box-state [y :9]) y :9)])

(defn container []
  [:div (container-style)
   (map #(cell %) board-state)
   ;; [:button {:on-click (play {:frequency 220})}]
   [:div (str @board-state)]
   ])

;; -------------------------
;; Initialize app

(defn mount-root []
  (r/render [container] (.getElementById js/document "app")))

(defn init! []
  (initialize-board)
  (mount-root))

(init!)
