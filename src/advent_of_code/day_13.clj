(ns advent-of-code.day-13
  (:require [clojure.string :as str]
            [clojure.java.io :as io]
            [clojure.core.async :as async]
            [quil.core :as q]))

(defonce screen-state (atom {:score 0 :screen [[]]}))

(defn intcode-program
  "Reads all the instructions from the Intcode program."
  []
  (-> (io/resource "day_13.txt")
      slurp
      (str/split #",")
      (->> (mapv #(Long/parseLong %)))))

(defn parse-instruction
  "Parses an instruction and returns a tuple of op code and param modes."
  [instruction]
  [(rem instruction 100)
   (mapv #(Integer/valueOf (str %)) (reverse (str (quot instruction 100))))])

(defn assoc-safely
  "Associates a value safely into the program inserting zeros where it was not
  previously defined."
  [program idx value]
  (assoc (if (> idx (dec (count program)))
           (apply conj program (repeat (- idx (dec (count program))) 0))
           program)
    idx
    value))

(defn execute-intcode-program
  "Executes the Intcode program. Closes output-chan when done. Op code 3/4
  consume and output to input-chan and output-chan respectfully."
  [initial-program input-chan output-chan]
  (async/go-loop [program initial-program
                  instruction-pointer 0
                  relative-base 0
                  snapshots []]
    (let [val-at-position (fn [offset]
                            (nth program (+ instruction-pointer offset) 0))
          [op-code param-modes] (parse-instruction (val-at-position 0))
          get-address (fn [index]
                        (let [value (val-at-position (inc index))
                              mode (nth param-modes index 0)]
                          (case mode
                            0 value
                            1 value
                            2 (+ relative-base value))))
          get-param (fn [index]
                      (let [value-at-address (val-at-position (inc index))]
                        (case (nth param-modes index 0)
                          0 (nth program value-at-address 0)
                          1 value-at-address
                          2 (nth program (+ relative-base value-at-address) 0))))]
      (case op-code
        ;; Add
        1 (recur (assoc-safely program
                               (get-address 2)
                               (+ (get-param 0) (get-param 1)))
                 (+ instruction-pointer 4)
                 relative-base
                 snapshots)

        ;; Multiply
        2 (recur (assoc-safely program
                               (get-address 2)
                               (* (get-param 0) (get-param 1)))
                 (+ instruction-pointer 4)
                 relative-base
                 snapshots)

        ;; Read
        3 (let [v (async/<! input-chan)]
            (case v
              :take-snapshot (let [next-snapshots (concat [[program instruction-pointer relative-base]] snapshots)]
                               (println "Program taking snapshot" (count next-snapshots))
                               (async/>! output-chan :take-snapshot)
                               (recur program instruction-pointer relative-base next-snapshots))
              :restore-snapshot (let [[last-snapshot & remaining-snapshots] snapshots]
                                  (if-let [[program instruction-pointer relative-base] last-snapshot]
                                    (do (println "Program restoring snapshot" (count remaining-snapshots))
                                        (async/>! output-chan :restore-snapshot)
                                        (recur program instruction-pointer relative-base remaining-snapshots))
                                    (do (println "No snapshot to restore")
                                        (recur program instruction-pointer relative-base remaining-snapshots))))
              (recur (assoc-safely program (get-address 0) v)
                     (+ instruction-pointer 2)
                     relative-base
                     snapshots)))

        ;; Print
        4 (recur (do (async/>! output-chan (get-param 0))
                     program)
                 (+ instruction-pointer 2)
                 relative-base
                 snapshots)

        ;; Jump If True
        5 (recur program
                 (if (zero? (get-param 0))
                   (+ instruction-pointer 3)
                   (get-param 1))
                 relative-base
                 snapshots)

        ;; Jump If False
        6 (recur program
                 (if (zero? (get-param 0))
                   (get-param 1)
                   (+ instruction-pointer 3))
                 relative-base
                 snapshots)

        ;; Less Than
        7 (recur (assoc-safely program
                               (get-address 2)
                               (if (< (get-param 0) (get-param 1)) 1 0))
                 (+ instruction-pointer 4)
                 relative-base
                 snapshots)

        ;; Equals
        8 (recur (assoc-safely program
                               (get-address 2)
                               (if (= (get-param 0) (get-param 1)) 1 0))
                 (+ instruction-pointer 4)
                 relative-base
                 snapshots)

        ;; Adjust Relative Base
        9 (recur program
                 (+ instruction-pointer 2)
                 (+ relative-base (get-param 0))
                 snapshots)

        ;; Halt
        99 (async/close! output-chan)))))

(defn get-initial-screen
  "Executes the Intcode program and returns the initial screen w/o input for part 1."
  [program]
  (let [input-chan (async/chan)
        output-chan (async/chan)]
    (execute-intcode-program program input-chan output-chan)
    (async/go-loop [screen []]
      (let [x (async/<! output-chan)
            y (async/<! output-chan)
            tile-id (async/<! output-chan)]
        (if (and (some? x) (some? y) (some? tile-id))
          (recur (conj screen [x y tile-id]))
          screen)))))

(defn count-tiles
  "Counts the tiles on the screen matching the given tile id."
  [screen tile-id]
  (count (filter #(= tile-id (nth % 2)) screen)))

(def scale-factor 20)

(defn screen-bounds
  "Gets the max x,y the screen occupies"
  [screen scaled?]
  (if scaled?
    [(* scale-factor (inc (apply max (map #(nth % 0) screen))))
     (* scale-factor (inc (apply max (map #(nth % 1) screen))))]
    [(apply max (map #(nth % 0) screen))
     (apply max (map #(nth % 1) screen))]))

(defn setup
  "Sets up Quil"
  []
  (q/frame-rate 30)
  (q/color-mode :rgb)
  (apply q/background [200 200 200]))

(defn draw
  "Draw the game state using Quil."
  [bounds]
  (let [{:keys [score screen]} @screen-state
        [max-x max-y] bounds]
    (q/clear)
    (doall
      (for [x (range max-x)
            y (range max-y)]
        (let [tile-id (get-in screen [y x])]
          (case tile-id
            ;; Empty Tile - Draw Nothing
            0 (do (q/fill 200 200 200)
                  (q/rect (* scale-factor x) (* scale-factor y) scale-factor scale-factor))

            ;; Wall Tile - Draw a Dark Gray Box
            1 (do (q/fill 40 40 40)
                  (q/rect (* scale-factor x) (* scale-factor y) scale-factor scale-factor))

            ;; Block Tile - Draw a Bright Purple Box
            2 (do (q/fill 200 100 255)
                  (q/rect (* scale-factor x) (* scale-factor y) scale-factor scale-factor))

            ;; Paddle - Draw a Dark Green Box
            3 (do (q/fill 0 255 0)
                  (q/rect (* scale-factor x) (* scale-factor y) scale-factor scale-factor))

            ;; Ball - Draw a Bright Red Circle
            4 (do (q/fill 255 0 0)
                  (q/ellipse (* scale-factor x) (* scale-factor y) scale-factor scale-factor))))))
    (q/fill 255 255 255)
    (q/text-font (q/create-font "Courier New" 22) 22)
    (q/text-align :right)
    (q/text (str score) (* max-x scale-factor) (dec scale-factor))))

(defn key-pressed
  "Handle different key presses and send them to the Intcode computer."
  [input-chan]
  (case (q/key-as-keyword)
    :left (async/>!! input-chan -1)
    :right (async/>!! input-chan 1)
    :shift (async/>!! input-chan :take-snapshot)
    :command (async/>!! input-chan :restore-snapshot)
    (async/>!! input-chan 0)))

(defn screen->state
  "Given an initial screen state, build a 2d vector representing the tiles."
  [screen [max-x max-y]]
  (let [blank-screen (mapv (fn [_]
                             (mapv (constantly 0) (range max-x)))
                           (range max-y))]
    (reduce (fn [acc [x y tile-id]]
              (assoc-in acc [y x] tile-id))
            blank-screen
            screen)))

(defn draw-game
  "Plays the game using Quil."
  [program initial-screen]
  (let [input-chan (async/chan)
        output-chan (async/chan)
        free-play-program (assoc program 0 2)               ;; Insert Quarters into program
        bounds (screen-bounds initial-screen false)]
    (reset! screen-state {:screen (screen->state initial-screen bounds) :score 0})
    (execute-intcode-program free-play-program input-chan output-chan)
    (async/go-loop [snapshots []]
      (if-let [x (async/<! output-chan)]
        (case x
          :take-snapshot (let [next-snapshots (concat [@screen-state] snapshots)]
                           (println "Taking game snapshot" (count next-snapshots))
                           (recur next-snapshots))
          :restore-snapshot (let [[last-snapshot & remaining-snapshots] snapshots]
                              (if last-snapshot
                                (do (println "Restoring game snapshot" (count remaining-snapshots))
                                    (reset! screen-state last-snapshot)
                                    (recur remaining-snapshots))
                                (do (println "No game snapshot to restore.")
                                    (recur snapshots))))
          (let [y (async/<! output-chan)
                tile-id (async/<! output-chan)]
            (if (and (some? x) (some? y) (some? tile-id))
              (do (if (and (= x -1) (= y 0))
                    (swap! screen-state #(assoc % :score tile-id))
                    (swap! screen-state #(assoc-in % [:screen y x] tile-id)))
                  (recur snapshots)))))
        (println "Game Over")))
    (q/defsketch breakout-sketch
      :title "Breakout"
      :size (screen-bounds initial-screen true)
      :host "host"
      :setup setup
      :draw (partial draw bounds)
      :key-pressed (partial key-pressed input-chan))))

(defn run
  "Runs part 1 and 2 of day 13."
  []
  (let [program (intcode-program)
        initial-screen (async/<!! (get-initial-screen program))]
    (println "Part 1 - Number of blocks on the screen:"
             (count-tiles initial-screen 2))
    (draw-game program initial-screen)))