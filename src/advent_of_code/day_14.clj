(ns advent-of-code.day-14
  (:require [clojure.string :as str]
            [clojure.java.io :as io])
  (:import (java.util Collections ArrayList)))

(defn format-segment
  "Given a segment, convert the quantity to a int and return a vector."
  [[quantity chemical]]
  [(Integer/valueOf quantity) chemical])

(defn reactions
  "Reads all reactions from the file and builds map of output => {quantity,inputs}."
  []
  (->> (io/resource "day_14.txt")
       slurp
       str/split-lines
       (reduce
         (fn [acc segment-str]
           (let [segments (as-> segment-str $
                                (str/replace $ #",|=|>" "")
                                (str/split $ #"\s+")
                                (partition-all 2 $))
                 inputs (mapv format-segment (butlast segments))
                 output (format-segment (last segments))]
             (assoc acc (nth output 1) {:quantity (nth output 0) :inputs inputs})))
         {})))

(defn min-ore-to-produce-single-fuel
  "Returns the minimum ORE and overflow required to make a single FUEL."
  ([reactions]
   (min-ore-to-produce-single-fuel reactions "FUEL" {:required 1 :overflow {}}))
  ([reactions chemical {:keys [required overflow]}]
   (let [overflow-available (get overflow chemical 0)
         updated-overflow (assoc overflow chemical (- overflow-available (min required overflow-available)))
         new-requirement (- required (min required overflow-available))]
     (if (or (= "ORE" chemical) (zero? new-requirement))
       {:required new-requirement
        :overflow updated-overflow}
       (let [{:keys [quantity inputs]} (get reactions chemical)
             min-take-factor (int (Math/ceil (/ new-requirement quantity)))
             overflow-to-remove (- (* quantity min-take-factor) new-requirement)
             next-overflow (update updated-overflow chemical #(+ % overflow-to-remove))]
         (reduce
           (fn [acc [input-quantity input-chemical]]
             (let [next-required (* min-take-factor input-quantity)
                   chain-requirement (min-ore-to-produce-single-fuel reactions input-chemical (assoc acc :required next-required))]
               (update chain-requirement :required #(+ % (:required acc)))))
           {:required 0
            :overflow next-overflow}
           inputs))))))

(defn max-fuel-given-ore
  "Finds the max fuel one can use given ORE. Using a hack binary search method :)."
  [reactions max-ore]
  (let [coll (proxy [ArrayList] []
               (size [] 10000000)
               (get [index]
                 (:required (min-ore-to-produce-single-fuel reactions
                                                            "FUEL"
                                                            {:required (inc index)
                                                             :overflow {}}))))
        idx (Collections/binarySearch coll max-ore)]
    (if (neg? idx)
      (Math/abs (inc idx))
      (inc idx))))

(defn run
  "Runs part 1 and 2 of day 14."
  []
  (let [reactions (reactions)]
    (println "Part 1 - Minimum ORE to produce a single FUEL:"
             (:required (min-ore-to-produce-single-fuel reactions)))
    (println "Part 2 - Max FUEL for 1 trillion ORE:"
             (max-fuel-given-ore reactions 1000000000000))))