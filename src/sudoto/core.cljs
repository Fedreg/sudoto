(ns sudoto.core
  (:require
   [reagent.core  :as r]
   [sudoto.state  :as state]
   [sudoto.matrix :as m]
   [sudoto.audio  :as audio]))

(defn initialize-board []
  (let [val (for [x (range 1 10)
                  y (range 1 10)]
              (if (= 2 (rand-int 3)) y 0))
        res (into [] val)]
    (swap! state/state assoc-in [:board] state/board)))

;; -------------------------
;; Styles 
;; -------------------------

(defn container-style []
  {:style
   {:box-sizing "border-box"
    :width      "456px"
    :height     "456px"
    :border     "3px solid black"}})

(defn cell-style [x y val]
  ;; {:style
  {:box-sizing        "border-box"
   :height            "50px"
    :width            "50px"
    :background-color (if (m/duplicate? x y val) "red" "white")
    :font-size        "18px"
    :text-align       "center"
    :border-top       "1px solid grey"
    :border-left      "1px solid grey"
    :border-right     (if (m/match-nums y [3 6 ]) "3px solid black")
    :border-bottom    (if (m/match-nums x [3 6 ]) "3px solid black")
    })

;; --------------------------
;; Views 
;; --------------------------

(defn cell [x y val]
  (let [box  (m/get-box  [x y])
        cell (m/get-cell [x y])
        value (if (= 0 val) "" val)
        n    (m/get-matrix-nth box cell)]

    [:input {:style     (cell-style x y value)
             :key       (str box "-" cell)
             :value     value 
             :on-change #(swap! state/state assoc-in [:board n]
                                (if (and (not= ""  (-> % .-target .-value))
                                         (not= " " (-> % .-target .-value)))
                                  (js/parseInt (-> % .-target .-value))
                                  0))}]))

(defn container []
  [:div (container-style)
   (map
    #(cell %1 %2 %3)
    (mapcat #(repeat 9 %) (range 1 10))
    (flatten (repeat 9 (range 1 10)))
    (:board @state/state))
   [:button {:on-click (fn [] (audio/play-all))} "play-all"]
   ])

;; -------------------------
;; Initialize app
;; -------------------------

(defn mount-root []
  (r/render [container] (.getElementById js/document "app")))

(defn init! []
  (initialize-board)
  (mount-root))

