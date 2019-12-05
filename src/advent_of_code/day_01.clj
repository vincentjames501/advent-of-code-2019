(ns advent-of-code.day-01
  (:require [clojure.java.io :as io]
            [clojure.string :as str]))

(defn module-masses
  "Reads all the module masses from a file and returns a collection of integers
  representing the masses."
  []
  (->> (io/resource "day_01.txt")
       slurp
       str/split-lines
       (map #(Integer/parseInt %))))

(defn direct-fuel-required-for-module
  "Computes the fuel required for a module which is:
  - Mass divided by 3
  - Rounded down
  - Subtract 2
  - Negative numbers are 0"
  [module-mass]
  (max (- (int (double (/ module-mass 3))) 2) 0))

(defn total-fuel-required-for-module
  "Recursively computes the mass for the module plus mass for the fuel required."
  [module-mass]
  (reduce + (take-while pos? (rest (iterate direct-fuel-required-for-module module-mass)))))

(defn total-fuel-required-without-considering-fuel
  "Part 1 - Computes the total fuel required without considering the weight of the fuel."
  [module-masses]
  (->> module-masses
       (map direct-fuel-required-for-module)
       (reduce +)))

(defn total-fuel-considering-fuel
  "Part 2 - Computes the total fuel required while considering the weight of the fuel."
  [module-masses]
  (->> module-masses
       (map total-fuel-required-for-module)
       (reduce +)))

(defn run
  "Runs part 1 and 2 of day 1."
  []
  (let [masses (module-masses)]
    (println "Part 1 - Total fuel required without considering fuel:"
             (total-fuel-required-without-considering-fuel masses))
    (println "Part 2 - Total fuel required considering fuel:"
             (total-fuel-considering-fuel masses))))

