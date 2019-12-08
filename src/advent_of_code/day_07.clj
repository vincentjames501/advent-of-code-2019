(ns advent-of-code.day-07
  (:require [clojure.string :as str]
            [clojure.java.io :as io]
            [clojure.core.async :as async]
            [clojure.core.reducers :as r]))

(defn intcode-program
  "Reads all the instructions from the Intcode program."
  []
  (-> (io/resource "day_07.txt")
      slurp
      (str/split #",")
      (->> (mapv #(Integer/parseInt %)))))

(defn permutations
  "Generates all permutations for 5 values starting with the start value."
  [start]
  (let [values (range start (+ 5 start))]
    (for [a values
          b values
          c values
          d values
          e values
          :when (distinct? a b c d e)]
      [a b c d e])))

(defn parse-instruction
  "Parses an instruction and returns a tuple of op code and param modes."
  [instruction]
  [(rem instruction 100)
   (mapv #(Integer/valueOf (str %)) (reverse (str (quot instruction 100))))])

(defn execute-intcode-program
  "Executes the Intcode program. Closes output-chan when done. Op code 3/4
  consume and output to input-chan and output-chan respectfully."
  [initial-program input-chan output-chan]
  (async/go-loop [program initial-program
                  instruction-pointer 0]
    (let [val-at-position (fn [offset]
                            (nth program (+ instruction-pointer offset)))
          [op-code param-modes] (parse-instruction (val-at-position 0))
          get-param (fn [index]
                      (let [value-at-address (val-at-position (inc index))]
                        (case (nth param-modes index 0)
                          0 (nth program value-at-address)
                          1 value-at-address)))]
      (case op-code
        ;; Add
        1 (recur (assoc program (val-at-position 3)
                                (+ (get-param 0) (get-param 1)))
                 (+ instruction-pointer 4))

        ;; Multiply
        2 (recur (assoc program (val-at-position 3)
                                (* (get-param 0) (get-param 1)))
                 (+ instruction-pointer 4))

        ;; Read
        3 (recur (assoc program (val-at-position 1)
                                (async/<! input-chan))
                 (+ instruction-pointer 2))

        ;; Print
        4 (recur (do
                   (async/>! output-chan (get-param 0))
                   program)
                 (+ instruction-pointer 2))

        ;; Jump If True
        5 (recur program
                 (if (zero? (get-param 0))
                   (+ instruction-pointer 3)
                   (get-param 1)))

        ;; Jump If False
        6 (recur program
                 (if (zero? (get-param 0))
                   (get-param 1)
                   (+ instruction-pointer 3)))

        ;; Less Than
        7 (recur (assoc program (val-at-position 3)
                                (if (< (get-param 0) (get-param 1)) 1 0))
                 (+ instruction-pointer 4))

        ;; Equals
        8 (recur (assoc program (val-at-position 3)
                                (if (= (get-param 0) (get-param 1)) 1 0))
                 (+ instruction-pointer 4))

        ;; Halt
        99 (async/close! output-chan)))))

(defn try-permutation-for-feedback-amplifier
  "Tries the permutation for the feedback loop amplifier returning the signal."
  [program permutation]
  (let [output-chan (async/chan)
        input-channels (mapv (fn [phase]
                               (let [result (async/chan)]
                                 (async/put! result phase)
                                 result))
                             permutation)]
    (doseq [[first-input-chan next-input-chan] (partition 2 1 (conj input-channels output-chan))]
      (execute-intcode-program program first-input-chan next-input-chan))
    (async/go
      (let [first-input (first input-channels)]
        (async/>! first-input 0)
        (loop [result nil]
          (if-let [latest (async/<! output-chan)]
            (do (async/put! first-input latest)
                (recur latest))
            result))))))

(defn find-highest-signal-for-feedback-amplifier
  "Try all permutations of inputs for the of the feedback amplifier returning
  the one resulting in the highest signal."
  [program]
  (loop [[next-combination & remaining-combinations] (permutations 5)
         acc 0]
    (if next-combination
      (recur remaining-combinations
             (let [output (async/<!! (try-permutation-for-feedback-amplifier program next-combination))]
               (if (> output acc) output acc)))
      acc)))

(defn try-permutation-for-normal-amplifier
  "Tries a given permutation for the normal amplifier returning the final output."
  [program combination]
  (async/go-loop [[current-input & next-inputs] combination
                  previous-output 0]
    (let [input-chan (async/chan)
          output-chan (async/chan)]
      (if current-input
        (do (execute-intcode-program program input-chan output-chan)
            (async/>! input-chan current-input)
            (async/>! input-chan previous-output)
            (recur next-inputs (async/<! output-chan)))
        previous-output))))

(defn find-highest-signal-for-normal-amplifier
  "Finds the highest signal for the normal amplifier."
  [program]
  (->> (permutations 0)
       (r/map #(async/<!! (try-permutation-for-normal-amplifier program %)))
       (into [])
       (apply max)))

(defn run
  "Runs part 1 and 2 of day 7."
  []
  (let [program (intcode-program)]
    (println "Part 1 - Result from Intcode program:"
             (find-highest-signal-for-normal-amplifier program))
    (println "Part 2 - Result from Intcode program:"
             (find-highest-signal-for-feedback-amplifier program))))