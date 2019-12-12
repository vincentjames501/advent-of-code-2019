(ns advent-of-code.day-11
  (:require [clojure.string :as str]
            [clojure.java.io :as io]
            [clojure.core.async :as async]))

(defn intcode-program
  "Reads all the instructions from the Intcode program."
  []
  (-> (io/resource "day_11.txt")
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
                  relative-base 0]
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
                 relative-base)

        ;; Multiply
        2 (recur (assoc-safely program
                               (get-address 2)
                               (* (get-param 0) (get-param 1)))
                 (+ instruction-pointer 4)
                 relative-base)

        ;; Read
        3 (recur (do #_(println "awaiting input")
                   (assoc-safely program
                                 (get-address 0)
                                 (async/<! input-chan)))
                 (+ instruction-pointer 2)
                 relative-base)

        ;; Print
        4 (recur (do
                   #_(println "output" (get-param 0))
                   (async/>! output-chan (get-param 0))
                   program)
                 (+ instruction-pointer 2)
                 relative-base)

        ;; Jump If True
        5 (recur program
                 (if (zero? (get-param 0))
                   (+ instruction-pointer 3)
                   (get-param 1))
                 relative-base)

        ;; Jump If False
        6 (recur program
                 (if (zero? (get-param 0))
                   (get-param 1)
                   (+ instruction-pointer 3))
                 relative-base)

        ;; Less Than
        7 (recur (assoc-safely program
                               (get-address 2)
                               (if (< (get-param 0) (get-param 1)) 1 0))
                 (+ instruction-pointer 4)
                 relative-base)

        ;; Equals
        8 (recur (assoc-safely program
                               (get-address 2)
                               (if (= (get-param 0) (get-param 1)) 1 0))
                 (+ instruction-pointer 4)
                 relative-base)

        ;; Adjust Relative Base
        9 (recur program
                 (+ instruction-pointer 2)
                 (+ relative-base (get-param 0)))

        ;; Halt
        99 (async/close! output-chan)))))

(def initial-panel
  "Each panel is represented with this map of current color and paint count."
  {:color 0 :count 0})

(defn get-next-position
  "Given a current position and facing and a direction to turn, return a new
  facing and position."
  [[x y facing] direction-to-turn]
  (if (zero? direction-to-turn)
    ;; Turn Left
    (case facing
      "^" [(- x 1) y "<"]
      "<" [x (+ y 1) "V"]
      "V" [(+ x 1) y ">"]
      ">" [x (- y 1) "^"])
    ;; Turn Right
    (case facing
      "^" [(+ x 1) y ">"]
      ">" [x (+ y 1) "V"]
      "V" [(- x 1) y "<"]
      "<" [x (- y 1) "^"])))

(defn normalize-position-and-drawing
  "After get-next-position is called, it could result in us going out of bounds
  on the drawing. Instead of going out of bounds, expand the drawing's vectors
  accordingly and then shift the x,y position."
  [position drawing]
  (let [[x y facing] position]
    (cond
      (neg? x)
      [[(inc x) y facing]
       (mapv #(apply conj [initial-panel] %) drawing)]

      (neg? y)
      [[x (inc y) facing]
       (apply conj [(vec (repeat (count (first drawing)) initial-panel))]
              drawing)]

      (= :not-found (nth drawing y :not-found))
      [position
       (conj drawing (vec (repeat (count (first drawing)) initial-panel)))]

      (= :not-found (nth (nth drawing y) x :not-found))
      [position (mapv #(conj % initial-panel) drawing)]

      :else
      [position drawing])))

(defn paint-drawing
  "Paints the drawing to stdout so a human could read it."
  [drawing]
  (doseq [row drawing]
    (doseq [{:keys [color]} row]
      (print (if (zero? color) "." "#")))
    (println)))

(defn build-drawing
  "Part 1 - Builds the drawing by executing the Intcode program. Starts
  off with an initial drawing of a single space and loops painting spaces
  along the way."
  [program initial-color]
  (let [in-chan (async/chan 1)
        out-chan (async/chan 1)]
    (execute-intcode-program program in-chan out-chan)
    (async/go-loop [current-position [0 0 "^"]
                    drawing [[(assoc initial-panel :color initial-color)]]]
      (let [[x y] current-position]
        (async/>! in-chan (:color (get-in drawing [y x])))
        (if-some [color-to-paint (async/<! out-chan)]
          (if-some [direction-to-turn (async/<! out-chan)]
            (let [[next-position next-drawing] (normalize-position-and-drawing
                                                 (get-next-position current-position direction-to-turn)
                                                 (-> drawing
                                                     (assoc-in [y x :color] color-to-paint)
                                                     (update-in [y x :count] inc)))]
              (recur next-position next-drawing))
            drawing)
          drawing)))))

(defn num-panels-painted
  "Computes the number of panels painted by checking all panels and counting the
  ones whose :count is > 0."
  [drawing]
  (->> drawing
       flatten
       (filter #(pos? (:count %)))
       count))

(defn run
  "Runs part 1 and 2 of day 11."
  []
  (let [program (intcode-program)
        drawing-1 (async/<!! (build-drawing program 0))
        drawing-2 (async/<!! (build-drawing program 1))]
    (println "Part 1 - Number of panels painted at least once:"
             (num-panels-painted drawing-1))
    (paint-drawing drawing-1)
    (println "Part 2 - Registration identifier:")
    (paint-drawing drawing-2)))