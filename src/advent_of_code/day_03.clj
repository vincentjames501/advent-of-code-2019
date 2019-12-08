(ns advent-of-code.day-03
  (:require [clojure.java.io :as io]
            [clojure.string :as str]
            [clojure.set :as set]))

(defn parse-directions
  "Parses the directions from a direction string splitting on comma and returning
  tuples of direction to amplitude."
  [s]
  (->> (str/split s #",")
       (mapv (fn [direction-str]
               (let [direction (subs direction-str 0 1)
                     amplitude (subs direction-str 1)]
                 [direction (int (Integer/parseInt amplitude))])))))

(defn directions
  "Returns a tuple of both directions."
  []
  (let [text (slurp (io/resource "day_03.txt"))
          [directions-1-str directions-2-str] (str/split text #"\n")]
      [(parse-directions directions-1-str)
       (parse-directions directions-2-str)]))

(defn manhattan-distance
  "Returns the manhattan distance of a given coordinate."
  [[x y]]
  (+ (Math/abs x) (Math/abs y)))

(defn visited-coordinates-with-steps
  "Computes all the visited points with steps given a sequence of directions."
  [directions]
  (loop [[current-direction & next-directions] directions
         [x y] [0 0]
         steps 0
         acc #{}]
    (if-let [[direction amplitude] current-direction]
      (let [iterations (range 1 (inc amplitude))
            points (case direction
                     "R" (mapv #(vector (+ x %) y (+ steps %)) iterations)
                     "L" (mapv #(vector (- x %) y (+ steps %)) iterations)
                     "U" (mapv #(vector x (+ y %) (+ steps %)) iterations)
                     "D" (mapv #(vector x (- y %) (+ steps %)) iterations))
            [new-x new-y] (nth points (dec (count points)))]
        (recur next-directions
               [new-x new-y]
               (+ steps amplitude)
               (apply conj acc points)))
      acc)))

(defn find-smallest-manhattan-distance
  "Finds all intersections and returns the value of the shortest manhattan distance."
  [directions-1 directions-2]
  (let [visited-coordinates-with-steps-1 (visited-coordinates-with-steps directions-1)
        visited-coordinates-with-steps-2 (visited-coordinates-with-steps directions-2)
        visited-coordinates-1 (set (map pop visited-coordinates-with-steps-1))
        visited-coordinates-2 (set (map pop visited-coordinates-with-steps-2))]
    (->> (set/intersection visited-coordinates-1 visited-coordinates-2)
         (map manhattan-distance)
         (apply min))))

(defn find-shortest-intersection-steps
  "Finds the shortest intersection and returns the number of steps taken to get there
  from both paths."
  [directions-1 directions-2]
  (let [visited-coordinates-with-steps-1 (visited-coordinates-with-steps directions-1)
        visited-coordinates-with-steps-2 (visited-coordinates-with-steps directions-2)
        visited-coordinates-1 (set (map pop visited-coordinates-with-steps-1))
        visited-coordinates-2 (set (map pop visited-coordinates-with-steps-2))
        lookups-1 (group-by pop visited-coordinates-with-steps-1)
        lookups-2 (group-by pop visited-coordinates-with-steps-2)]
    (->> (set/intersection visited-coordinates-1 visited-coordinates-2)
         (map (fn [intersection]
                (let [coordinates-with-steps-1 (get lookups-1 intersection)
                      coordinates-with-steps-2 (get lookups-2 intersection)
                      min-steps-1 (apply min (map #(nth % 2) coordinates-with-steps-1))
                      min-steps-2 (apply min (map #(nth % 2) coordinates-with-steps-2))]
                  (+ min-steps-1 min-steps-2))))
         (apply min))))

(defn run
  "Runs part 1 and 2 of day 3."
  []
  (let [[directions-1 directions-2] (directions)]
    (println "Part 1 - Manhattan distance:"
             (find-smallest-manhattan-distance directions-1 directions-2))
    (println "Part 2 - Shortest intersection steps:"
             (find-shortest-intersection-steps directions-1 directions-2))))
