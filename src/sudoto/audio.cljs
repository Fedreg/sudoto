(ns sudoto.audio)

;; ---------------------------
;; Oscillator
;; ---------------------------

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
    (.setTargetAtTime (.-gain vol) 0.75 (.-currentTime ctx) 0.1)
    (.setTargetAtTime (.-gain vol) 0.00 (+ (.-currentTime ctx) sustain) 0.1)

    (set! (.-value  (.-frequency osc)) (* hz octave))
    (set! (.-type osc) wave)

    (.start osc (.-currentTime ctx))
    (.stop osc (+ (.-currentTime ctx) sustain 0.1))))

(defn frequency
  "Determines frequency in hz from half-steps above given frequency"
  [half-steps]
  (* 110 (Math/pow 1.059463 half-steps)))

(defn play-sequence
  "Schedules a sequence of notes to be sent to play-note"
  [notes bpm]
  (let [xs       (first notes)
        ys       (rest notes)
        hz       (:frequency xs)
        octave   (:octave xs)
        duration (:duration xs)
        sustain  (* (/ 60 bpm) duration 2)]
    (prn "SUS" sustain bpm)
    (play-note octave hz 0.5)
    (when-not (empty? ys)
      (js/setTimeout #(play-sequence ys bpm) (* 1000 sustain)))))



