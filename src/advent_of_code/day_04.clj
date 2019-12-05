(ns advent-of-code.day-04
  (:require [clojure.java.io :as io]
            [clojure.string :as str]))

(defn read-puzzle-input-range
  "Reads the puzzle input for day 4"
  []
  (-> (io/resource "day_04.txt")
      slurp
      (str/split #"-")
      (->> (map #(Integer/parseInt %))
           (apply range))))

(defn input-to-digits
  "Converts an input like 1234 to a sequence of digits like [1 2 3 4]"
  [input]
  (mapv #(Integer/valueOf (str %)) (str input)))

(defn matches-criteria-p1?
  "Checks the password to see if it meets the criteria for part 1."
  [input]
  (let [digits (input-to-digits input)]
    (and (some #(apply = %) (partition 2 1 digits))
         (apply <= digits))))

(defn matches-criteria-p2?
  "Checks the password to see if it meets the criteria for part 2."
  [input]
  (let [digits (input-to-digits input)]
    (and (some #(= 2 %) (vals (frequencies digits)))
         (apply <= digits))))

(defn run
  "Runs part 1 and 2 of day 4."
  []
  (let [input-range (read-puzzle-input-range)]
    (println "Part 1 - Number matching criteria:"
             (count (filter matches-criteria-p1? input-range)))
    (println "Part 2 - Number matching criteria:"
             (count (filter matches-criteria-p2? input-range)))))
