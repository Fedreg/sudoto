(ns sudoto.audio
  (:require
   [sudoto.matrix :as m]
   [sudoto.state  :as state]))


(defn half-steps
  "Determines how many half-steps in each number from 1-9"
  [n]
  (case n
    1 (get-in @state/state [:mode :1])
    2 (get-in @state/state [:mode :2])
    3 (get-in @state/state [:mode :3])
    4 (get-in @state/state [:mode :4])
    5 (get-in @state/state [:mode :5])
    6 (get-in @state/state [:mode :6])
    7 (get-in @state/state [:mode :7])
    8 (get-in @state/state [:mode :8])
    9 (get-in @state/state [:mode :9])))

(defn frequency
  "Determines frequency in hz from half-steps above given frequency"
  [n]
  (if (= 0 n)
    0
    (* 110 (Math/pow 1.059463 (half-steps n)))))

(defn octave
  "Returns octave based on sudoku number"
  [n]
  (case n
    0 4
    1 8
    2 4
    3 8
    4 2
    5 2
    6 8
    7 2
    8 4
    9 0.5))

(defn duration
  "Note duration based on box number"
  [box]
  (case box
    1 0.125
    2 0.25
    3 0.25
    4 0.5
    5 0.5 
    6 1
    7 1
    8 2
    9 2))

(defn repetitions
  "Determines how many times each box's notes should repeat"
  [box]
  (case box
    1 16
    2 8
    3 8
    4 4
    5 4
    6 2
    7 2
    8 1
    9 1))

(defn -build-notes
  "Prepares note map to be sent to oscillators"
  [box hz]
  {:frequency hz
   :octave    (octave box)
   :duration  (duration box)})

(defn build-notes
  "Calculates notes to send based on box number"
  [box]
  (let [cells (m/box-cells box)
        nums  (map #(nth (:board @state/state) %) cells)
        freqs (map #(frequency %) nums)
        reps  (repetitions box)
        notes (map #(-build-notes box %1) freqs)]
    (->> notes
         (repeat reps)
         flatten)))

(def ctx 
  (let [constructor (or js/window.AudioContext
                        js/window.webkitAudioContext)]
    (constructor.)))

(defn play-note
  "Creates a synthesizer that connects web audio parts and generates frequency"
  [octave hz sustain]
  (let [osc     (.createOscillator ctx)
        vol     (.createGain ctx)
        wave    "square"]
    (.connect osc vol)
    (.connect vol (.-destination ctx))
    
    (set! (.-value (.-gain vol)) 0)
    (.setTargetAtTime (.-gain vol) 0.25 (.-currentTime ctx) 0.01)
    (.setTargetAtTime (.-gain vol) 0.00 (+ (.-currentTime ctx) sustain) 0.05)

    (set! (.-value  (.-frequency osc)) (* hz octave))
    (set! (.-type osc) wave)

    (.start osc (.-currentTime ctx))
    (.stop osc (+ (.-currentTime ctx) sustain 0.1))))

(defn play-sequence
  "Schedules a sequence of notes to be sent to play-note"
  [notes bpm]
  (let [xs       (first notes)
        ys       (rest notes)
        hz       (:frequency xs)
        octave   (:octave xs)
        duration (:duration xs)
        sustain  (* (/ 60 bpm) duration 2)]
    (play-note octave hz sustain)
    (when-not (empty? ys)
      (js/setTimeout #(play-sequence ys bpm) (* 1000 sustain)))))

(defn play-all []
  (play-sequence (build-notes 1) (:bpm @state/state))
  (play-sequence (build-notes 2) (:bpm @state/state))
  (play-sequence (build-notes 3) (:bpm @state/state))
  (play-sequence (build-notes 4) (:bpm @state/state))
  (play-sequence (build-notes 5) (:bpm @state/state))
  (play-sequence (build-notes 6) (:bpm @state/state))
  (play-sequence (build-notes 7) (:bpm @state/state))
  (play-sequence (build-notes 8) (:bpm @state/state))
  (play-sequence (build-notes 9) (:bpm @state/state)))

(def modes
  {:ionaian    {:1 0 :2 2 :3 4 :4 5 :5 7 :6 9 :7 11 :8 12 :9 14}
   :dorian     {:1 0 :2 2 :3 3 :4 5 :5 7 :6 9 :7 11 :8 12 :9 14}
   :phrygian   {:1 0 :2 1 :3 3 :4 5 :5 7 :6 8 :7 10 :8 12 :9 13}
   :lydian     {:1 0 :2 2 :3 4 :4 6 :5 7 :6 9 :7 11 :8 12 :9 14}
   :mixolydian {:1 0 :2 2 :3 3 :4 5 :5 7 :6 8 :7 11 :8 12 :9 14}
   :aeolian    {:1 0 :2 2 :3 3 :4 5 :5 7 :6 8 :7 10 :8 12 :9 14}
   })

