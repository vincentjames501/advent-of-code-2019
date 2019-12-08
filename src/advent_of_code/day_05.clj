(ns advent-of-code.day-05
  (:require [clojure.java.io :as io]
            [clojure.string :as str]))

(defn intcode-program
  "Reads all the instructions from the Intcode program."
  []
  (-> (io/resource "day_05.txt")
      slurp
      (str/split #",")
      (->> (mapv #(Integer/parseInt %)))))

(defmulti process-op
  "Handles each op code and accepts the op-code, program, inputs, current instruction pointer,
  and parameter modes. Returns a tuple of the new program, new inputs, and new instruction pointer.
  If the instruction pointer is the same as it was before, stop the program."
  (fn [op-code program inputs instruction-pointer parameter-modes]
    op-code))

(defn params
  "Returns up to num params after the instruction pointer to the op code."
  [program instruction-pointer num]
  (subvec program (inc instruction-pointer) (+ num (inc instruction-pointer))))

(defn val-for-param-mode
  "If the param mode is 0, get the value at the program's address, otherwise, just use the
  value as-is."
  [program param-mode param]
  (if (= param-mode 0)
    (nth program param)
    param))

;; Add
(defmethod process-op 1
  [_ program inputs instruction-pointer [param-mode-1 param-mode-2]]
  (let [[param-1 param-2 address] (params program instruction-pointer 3)
        value-1 (val-for-param-mode program param-mode-1 param-1)
        value-2 (val-for-param-mode program param-mode-2 param-2)]
    [(assoc program address (+ value-1 value-2)) inputs (+ 4 instruction-pointer)]))

;; Multiply
(defmethod process-op 2
  [_ program inputs instruction-pointer [param-mode-1 param-mode-2]]
  (let [[param-1 param-2 address] (params program instruction-pointer 3)
        value-1 (val-for-param-mode program param-mode-1 param-1)
        value-2 (val-for-param-mode program param-mode-2 param-2)]
    [(assoc program address (* value-1 value-2)) inputs (+ 4 instruction-pointer)]))

;; Input
(defmethod process-op 3
  [_ program inputs instruction-pointer _]
  (let [address (first (params program instruction-pointer 1))
        [input-value & new-inputs] inputs]
    [(assoc program address input-value) new-inputs (+ 2 instruction-pointer)]))

;; Output
(defmethod process-op 4
  [_ program inputs instruction-pointer [param-mode-1]]
  (let [address (first (params program instruction-pointer 1))]
    (println "Output" (if (= param-mode-1 0)
                        (nth program address)
                        address))
    [program inputs (+ 2 instruction-pointer)]))

;; Jump If True
(defmethod process-op 5
  [_ program inputs instruction-pointer [param-mode-1 param-mode-2]]
  (let [[param-1 param-2] (params program instruction-pointer 2)
        value-1 (val-for-param-mode program param-mode-1 param-1)
        value-2 (val-for-param-mode program param-mode-2 param-2)]
    (if (zero? value-1)
      [program inputs (+ 3 instruction-pointer)]
      [program inputs value-2])))

;; Jump If False
(defmethod process-op 6
  [_ program inputs instruction-pointer [param-mode-1 param-mode-2]]
  (let [[param-1 param-2] (params program instruction-pointer 2)
        value-1 (val-for-param-mode program param-mode-1 param-1)
        value-2 (val-for-param-mode program param-mode-2 param-2)]
    (if (zero? value-1)
      [program inputs value-2]
      [program inputs (+ 3 instruction-pointer)])))

;; Less Than
(defmethod process-op 7
  [_ program inputs instruction-pointer [param-mode-1 param-mode-2]]
  (let [[param-1 param-2 address] (params program instruction-pointer 3)
        value-1 (val-for-param-mode program param-mode-1 param-1)
        value-2 (val-for-param-mode program param-mode-2 param-2)]
    [(assoc program address (if (< value-1 value-2) 1 0)) inputs (+ 4 instruction-pointer)]))

;; Equals
(defmethod process-op 8
  [_ program inputs instruction-pointer [param-mode-1 param-mode-2]]
  (let [[param-1 param-2 address] (params program instruction-pointer 3)
        value-1 (val-for-param-mode program param-mode-1 param-1)
        value-2 (val-for-param-mode program param-mode-2 param-2)]
    [(assoc program address (if (= value-1 value-2) 1 0)) inputs (+ 4 instruction-pointer)]))

;; Quit
(defmethod process-op 99
  [_ program inputs instruction-pointer _]
  [program inputs instruction-pointer])

;; Other
(defmethod process-op :default
  [_ program inputs instruction-pointer _]
  (println "Something went wrong!")
  [program inputs instruction-pointer])

(defn parse-op-code
  "Parses an op-code such as 1002 and produces an op code followed by the param modes:
  [2 [0 1]]"
  [op-code]
  (let [formatted-op-code (format "%04d" op-code)]
    [(Integer/valueOf (subs formatted-op-code 2))
     (mapv #(Integer/valueOf (str %)) (reverse (subs formatted-op-code 0 (- (count formatted-op-code) 2))))]))

(defn execute-intcode-program
  "Executes an Intcode program one operation at a time. Once complete, return the
  first value in the program."
  [program inputs]
  (loop [current-program program
         current-inputs inputs
         instruction-pointer 0]
    (let [[op-code parameter-modes] (parse-op-code (nth current-program instruction-pointer))
          [new-program new-inputs new-instruction-pointer] (process-op op-code current-program current-inputs instruction-pointer parameter-modes)]
      (if (not= new-instruction-pointer instruction-pointer)
        (recur new-program new-inputs new-instruction-pointer)
        (first new-program)))))

(defn run
  "Runs part 1 and 2 of day 5."
  []
  (let [program (intcode-program)]
    (println "Part 1 - Result from Intcode program:"
             (execute-intcode-program program [1]))
    (println "Part 2 - Result from Intcode program:"
             (execute-intcode-program program [5]))))

