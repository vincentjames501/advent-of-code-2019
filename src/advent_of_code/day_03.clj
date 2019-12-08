(ns advent-of-code.day-03
  (:require [clojure.java.io :as io]
            [clojure.string :as str]
            [clojure.core.reducers :as r]))

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
         acc []]
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
      (->> acc
           (r/reduce
             (fn [acc v]
               (let [k (pop v)]
                 (if (contains? acc k)
                   acc
                   (assoc acc k v))))
             {})))))

(defn find-smallest-manhattan-distance
  "Finds all intersections and returns the value of the shortest manhattan distance."
  [intersections]
  (->> intersections
       (r/map manhattan-distance)
       (into [])
       (apply min)))

(defn find-shortest-intersection-steps
  "Finds the shortest intersection and returns the number of steps taken to get there
  from both paths."
  [visited-coordinates-with-steps-1 visited-coordinates-with-steps-2 intersections]
  (->> intersections
       (r/map (fn [intersection]
                (+ (nth (get visited-coordinates-with-steps-1 intersection) 2)
                   (nth (get visited-coordinates-with-steps-2 intersection) 2))))
       (into [])
       (apply min)))

(defn run
  "Runs part 1 and 2 of day 3."
  []
  (let [[directions-1 directions-2] (directions)
        visited-coordinates-with-steps-1 (visited-coordinates-with-steps directions-1)
        visited-coordinates-with-steps-2 (visited-coordinates-with-steps directions-2)
        intersections (->> (keys visited-coordinates-with-steps-2)
                           (r/filter #(contains? visited-coordinates-with-steps-1 %))
                           (into []))]
    (println "Part 1 - Manhattan distance:"
             (find-smallest-manhattan-distance intersections))
    (println "Part 2 - Shortest intersection steps:"
             (find-shortest-intersection-steps visited-coordinates-with-steps-1 visited-coordinates-with-steps-2 intersections))))
