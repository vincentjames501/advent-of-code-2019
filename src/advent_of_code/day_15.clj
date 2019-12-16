(ns advent-of-code.day-15
  (:require [clojure.string :as str]
            [clojure.java.io :as io]
            [clojure.core.async :as async]
            [clojure.set :as set]))


(defn intcode-program
  "Reads all the instructions from the Intcode program."
  []
  (-> (io/resource "day_15.txt")
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

;;north (1), south (2), west (3), and east (4)

(defn apply-direction
  "Given a position and a direction, return the next position with the direction applied."
  [[x y] direction]
  (case direction
    1 [x (inc y)]
    2 [x (dec y)]
    3 [(dec x) y]
    4 [(inc x) y]))

(def directions
  "Next direction attempts when using the left or right follow side"
  {:left  {1 [3 1 4 2]
           2 [4 2 3 1]
           3 [2 3 1 4]
           4 [1 4 2 3]}
   :right {1 [4 1 3 2]
           2 [3 2 4 1]
           3 [1 3 2 4]
           4 [2 4 1 3]}})

(defn find-o2-sensor-for-direction
  "Finds the o2 sensor, walls, by following a specific follow side."
  [program follow-side]
  (let [input-chan (async/chan)
        output-chan (async/chan)]
    (execute-intcode-program program input-chan output-chan)
    (async/go-loop [visited {[0 0] 0}
                    sensor-position nil
                    walls #{}
                    steps 0
                    current-position [0 0]
                    facing 1]
      (let [directions-without-walls (->> (get-in directions [follow-side facing])
                                          (filter (fn [direction]
                                                    (not (contains? walls (apply-direction current-position direction))))))
            next-direction (or (->> directions-without-walls
                                    (filter (fn [direction]
                                              (not (contains? visited (apply-direction current-position direction)))))
                                    first)
                               (first directions-without-walls))
            next-position (apply-direction current-position next-direction)]
        (async/>! input-chan next-direction)
        (let [code (async/<! output-chan)]
          (case code
            ;; Wall
            0 (recur visited sensor-position (conj walls next-position) steps current-position facing)
            ;; Open
            1 (let [next-visited (update visited next-position #(min (or % Integer/MAX_VALUE) (inc steps)))]
                (recur next-visited sensor-position walls (get next-visited next-position) next-position next-direction))
            ;; O2 Sensor
            2 {:sensor-position next-position
               :steps-to-sensor (inc steps)
               :walls           walls}))))))

(defn find-o2-sensor
  "Finds the o2 sensor by following the left and right walls. Returns the minimum
  number of steps to get to the sensor, all the walls if found, and the sensor position."
  [program]
  (async/go
    (let [left-result (async/<! (find-o2-sensor-for-direction program :left))
          right-result (async/<! (find-o2-sensor-for-direction program :right))]
      {:sensor-position (:sensor-position left-result)
       :steps-to-sensor (:steps-to-sensor left-result)
       :walls           (set/union (:walls left-result) (:walls right-result))})))

(defn fill-oxygen
  "Fills all the next positions that haven't already been filled with oxygen and returns the
  new locations that were filled."
  [walls oxygen-locations]
  (loop [result #{}
         [current & remaining] oxygen-locations]
    (if current
      (let [next-locations (->> (range 1 5)
                                (keep (fn [direction]
                                        (let [next-position (apply-direction current direction)]
                                          (when-not (or (contains? oxygen-locations next-position)
                                                        (contains? walls next-position))
                                            next-position))))
                                set)]
        (recur (set/union result next-locations) remaining))
      result)))

(defn minutes-to-fill-with-oxygen
  "Recursively fills the system with oxygen and stops when there is nothing left to spread."
  [sensor-position walls]
  (loop [mins 0
         oxygen-locations #{sensor-position}]
    (let [new-oxygen-locations (fill-oxygen walls oxygen-locations)]
      (if (empty? new-oxygen-locations)
        mins
        (recur (inc mins)
               (set/union oxygen-locations new-oxygen-locations))))))

(defn run
  "Runs part 1 and 2 of day 15."
  []
  (let [program (intcode-program)
        {:keys [walls steps-to-sensor sensor-position]} (async/<!! (find-o2-sensor program))]
    (println "Part 1 - Minimum number of movements to find oxygen sensor:"
             steps-to-sensor)
    (println "Part 2 - Minutes to fill with oxygen:"
             (minutes-to-fill-with-oxygen sensor-position walls))))