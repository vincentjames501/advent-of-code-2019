(ns advent-of-code.day-02
  (:require [clojure.java.io :as io]
            [clojure.string :as str]))

(defn intcode-program
  "Reads all the instructions from the Intcode program."
  []
  (-> (io/resource "day_02.txt")
      slurp
      (str/split #",")
      (->> (mapv #(Integer/parseInt %)))))

(defmulti process-op
  "Handles each op code and accepts the op-code, program, and current instruction pointer.
  Returns a tuple of the new program and the instruction pointer offset. If the offset is
  0, stop the program."
  (fn [op-code program instruction-pointer] op-code))

(defmethod process-op 1
  [_ program instruction-pointer]
  (let [[x y pos] (subvec program
                          (inc instruction-pointer)
                          (+ 3 (inc instruction-pointer)))
        v1 (nth program x)
        v2 (nth program y)]
    [(assoc program pos (+ v1 v2)) 4]))

(defmethod process-op 2
  [_ program instruction-pointer]
  (let [[x y pos] (subvec program
                          (inc instruction-pointer)
                          (+ 3 (inc instruction-pointer)))
        v1 (nth program x)
        v2 (nth program y)]
    [(assoc program pos (* v1 v2)) 4]))

(defmethod process-op 99
  [_ program _]
  [program 0])

(defmethod process-op :default
  [_ program _]
  (println "Something went wrong!")
  [program 0])

(defn execute-intcode-program
  "Executes an Intcode program one operation at a time. Once complete, return the
  first value in the program."
  [program noun verb]
  (loop [current-program (assoc program 1 noun 2 verb)
         instruction-pointer 0]
    (let [op-code (nth current-program instruction-pointer)
          [new-program advance-pos-by] (process-op op-code current-program instruction-pointer)]
      (if (pos? advance-pos-by)
        (recur new-program (+ instruction-pointer advance-pos-by))
        (first new-program)))))

(defn discover-magic-number
  "Loops through all permutations of nouns and verbs and do (100 * noun) + verb where
  the output of the Intcode program is equal to the magic number."
  [program magic-number]
  (some (fn [[noun verb]]
          (when (= (execute-intcode-program program noun verb)
                   magic-number)
            (+ (* 100 noun) verb)))
        (for [noun (range 100)
              verb (range 100)]
          [noun verb])))

(defn run
  "Runs part 1 and 2 of day 2."
  []
  (let [program (intcode-program)]
    (println "Part 1 - Result from Intcode program:"
             (execute-intcode-program program 12 2))
    (println "Part 2 - Result from discovering magic number:"
             (discover-magic-number program 19690720))))

