(ns sudoto.core
  (:require
   [reagent.core :as r]
   [clojure.set :as set]))

;; -------------------------
;; model  
;; -------------------------

(def init-nums (atom #{1 2 3 4 5 6 7 8 9}))
(def init-used (atom #{}))
(def box-state (atom {}))
;; X axis
(def xa (atom #{}))
(def xb (atom #{}))
(def xc (atom #{}))
;; Y axis
(def ya (atom #{}))
(def yb (atom #{}))
(def yc (atom #{}))

(defn reset-state []
  "Resets all atoms to empty sets"
  (map #(reset! % #{}) [init-used xa xb xc ya yb yc]))

(defn get-row-atoms [key]
  "Returns atoms needed to track state in each row or column depending on box key"
  (case key
    :1 [xa ya]
    :2 [xa yb]
    :3 [xa yc]
    :4 [xb ya]
    :5 [xb yb]
    :6 [xb yc]
    :7 [xc ya]
    :8 [xc yb]
    :9 [xc yc]))

(defn set-row-state [key val]
  "Adds number used to appropriate row/column atoms to ensure no duplication of numbers."
  (map #(swap! % conj val) (get-row-atoms key)))

(defn randomizer [key]
  "Returns blank or 1-9"
  (let [all-nums  @init-nums
        used-nums @init-used
        row-nums  @(first (get-row-atoms key))
        col-nums  @(last  (get-row-atoms key))
        possible (shuffle (set/difference all-nums used-nums row-nums col-nums))
        num (first possible)
        rand (rand-int 3)
        val (if (= 2 rand) num "")]
    (swap! init-used conj num)
    (when (integer? val) 
      (swap! (first (get-row-atoms key)) conj num)
      (swap! (last  (get-row-atoms key)) conj num))
    val))

(defn gen-init-values [key]
  "Generates initial value (num or blank) for each box"
  (reset! init-used #{})
  (swap! box-state assoc
         key
         {:1 (randomizer :1)
          :2 (randomizer :2)
          :3 (randomizer :3)
          :4 (randomizer :4)
          :5 (randomizer :5)
          :6 (randomizer :6)
          :7 (randomizer :7)
          :8 (randomizer :8)
          :9 (randomizer :9)}))

(defn initialize []
  "Sets state values for a new game"
  (reset-state)
  (gen-init-values :1)
  (gen-init-values :2)
  (gen-init-values :3)
  (reset! xa #{})
  (reset! xb #{})
  (reset! xc #{})
  (gen-init-values :4)
  (gen-init-values :5)
  (gen-init-values :6)
  (reset! xa #{})
  (reset! xb #{})
  (reset! xc #{})
  (gen-init-values :7)
  (gen-init-values :8)
  (gen-init-values :9))

;; -------------------------
;; Util 
;; -------------------------


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
  {:style
   {:height "32px"
    :width "32px"
    :text-align "center"
    :border "1px solid black"}})

;; --------------------------
;; Views 
;; --------------------------

(defn cell [a]
  [:div (cell-style) a])

(defn box [y]
  [:div (box-style)
   (cell (get-in @box-state [y :1]))
   (cell (get-in @box-state [y :2]))
   (cell (get-in @box-state [y :3]))
   (cell (get-in @box-state [y :4]))
   (cell (get-in @box-state [y :5]))
   (cell (get-in @box-state [y :6]))
   (cell (get-in @box-state [y :7]))
   (cell (get-in @box-state [y :8]))
   (cell (get-in @box-state [y :9]))])

(defn container []
  [:div (container-style)
   (box :1)
   (box :2)
   (box :3)
   (box :4)
   (box :5)
   (box :6)
   (box :7)
   (box :8)
   (box :9)])

;; -------------------------
;; Initialize app

(defn mount-root []
  (r/render [container] (.getElementById js/document "app")))

(defn init! []
  (initialize)
  (mount-root))
