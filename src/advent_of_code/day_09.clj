(ns advent-of-code.day-09
  (:require [clojure.string :as str]
            [clojure.java.io :as io]
            [clojure.core.async :as async]))

(defn intcode-program
  "Reads all the instructions from the Intcode program."
  []
  (-> (io/resource "day_09.txt")
      slurp
      (str/split #",")
      (->> (mapv #(Integer/parseInt %)))))

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
        3 (recur (assoc-safely program
                               (get-address 0)
                               (async/<! input-chan))
                 (+ instruction-pointer 2)
                 relative-base)

        ;; Print
        4 (recur (do (async/>! output-chan (get-param 0))
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

(defn find-boost-keycode
  "Part 1 - Returns the BOOST keycode."
  [program inputs]
  (let [in-chan (async/chan)
        out-chan (async/chan)]
    (execute-intcode-program program in-chan out-chan)
    (async/onto-chan in-chan inputs)
    (last (async/<!! (async/into [] out-chan)))))

(defn run
  "Runs part 1 and 2 of day 9."
  []
  (let [program (intcode-program)]
    (println "Part 1 - Result from Intcode program:"
             (find-boost-keycode program [1]))
    (println "Part 2 - Result from Intcode program:"
             (find-boost-keycode program [2]))))