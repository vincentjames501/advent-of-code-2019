(ns advent-of-code.day-06
  (:require [clojure.java.io :as io]
            [clojure.string :as str]))

(defn orbits
  "Reads all the orbits and returns a collection of tuples representing
  [x y] where y orbits around x."
  []
  (->> (io/resource "day_06.txt")
       slurp
       str/split-lines
       (map #(str/split % #"\)"))))

(defn child->parent-orbits
  "Builds a map of child object id -> parent object id where the child is
  orbiting around the parent."
  [orbits]
  (->> (group-by last orbits)
       (reduce-kv (fn [acc k v]
                    (assoc acc k (first (mapv first v))))
                  {})))

(defn number-direct-and-indirect-orbits
  "Computes the number of direct and indirect orbits."
  ([orbits]
   (let [child->parent-orbits (child->parent-orbits orbits)]
     (->> (keys child->parent-orbits)
          (map (partial number-direct-and-indirect-orbits child->parent-orbits))
          (reduce +))))
  ([child->parent-orbits object-id]
   (loop [current-object-id object-id
          acc 0]
     (if-let [parent-object-id (get child->parent-orbits current-object-id)]
       (recur parent-object-id (inc acc))
       acc))))

(defn distances-to-com
  "Given a child to parent orbits map and an object id, build a vector of tuples
  representing the parent object and it's distance so far all the way to COM."
  [child->parent-orbits object-id]
  (loop [current-object-id object-id
         acc []
         level 1]
    (if-let [parent-object-id (get child->parent-orbits current-object-id)]
      (recur parent-object-id (conj acc [parent-object-id level]) (inc level))
      acc)))

(defn num-transfers-between-you-and-san
  "Computes the number of transfers required between YOU and SAN by walking from YOU
  and SAN and finding the first common parent node then adding their distances."
  [orbits]
  (let [child->parent-orbits (child->parent-orbits orbits)
        you-distances-to-com (distances-to-com child->parent-orbits "YOU")
        you-distances-lookup (into {} you-distances-to-com)
        san-distances-to-com (distances-to-com child->parent-orbits "SAN")]
    (loop [[[object-id distance] & entries] san-distances-to-com]
      (if-let [distance-2 (get you-distances-lookup object-id)]
        (- (+ distance distance-2) 2)
        (recur entries)))))

(defn run
  "Runs part 1 and 2 of day 6."
  []
  (let [orbits (orbits)]
    (println "Part 1 - Number of direct and indirect orbits:"
             (number-direct-and-indirect-orbits orbits))
    (println "Part 2 - Number of transfers between YOU and SAN:"
             (num-transfers-between-you-and-san orbits))))

