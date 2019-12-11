(ns advent-of-code.day-10
  (:require [clojure.java.io :as io]
            [clojure.string :as str]
            [clojure.core.reducers :as r])
  (:import (java.util Comparator)))

(defn asteroid-belt
  "Reads the asteroid belt and returns a vector of vectors."
  []
  (->> (io/resource "day_10.txt")
       slurp
       str/split-lines
       (mapv (partial mapv str))))

(defn all-asteroid-locations
  "Returns a collection of all x,y tuples that have an asteroid in them."
  [asteroid-belt]
  (->> (for [x (range (count (first asteroid-belt)))
             y (range (count asteroid-belt))
             :when (= "#" (get-in asteroid-belt [y x]))]
         [x y])
       vec))

(defn distance-and-angle-of-other-asteroids
  "Returns a sequence of vectors with the location, distance, and angle
  in degrees to the other asteroids."
  [all-asteroid-locations asteroid-location]
  (let [[x y] asteroid-location]
    (->> all-asteroid-locations
         (r/filter #(not= % asteroid-location))
         (r/map (fn [[target-x target-y]]
                  (let [delta-x (double (- x target-x))
                        delta-y (double (- y target-y))]
                    [;; Location
                     [target-x target-y]
                     ;; Distance
                     (Math/sqrt (+ (Math/pow delta-x 2) (Math/pow delta-y 2)))
                     ;; Angle in degrees so I can understand things but shifted so straight up is 0.0 degrees
                     (let [rads (Math/atan2 delta-y delta-x)
                           degrees (- (Math/toDegrees rads) 90.0)]
                       (if (neg? degrees)
                         (+ degrees 360.0)
                         degrees))])))
         (into []))))

(defn number-asteroids-visible-from-location
  "Returns the number of asteroids that can be seen at a given location."
  [all-asteroid-locations asteroid-location]
  (->> (distance-and-angle-of-other-asteroids all-asteroid-locations asteroid-location)
       (map #(nth % 2))
       set
       count))

(defn highest-number-of-asteroids
  "Part 1 - Finds the highest number of asteroids that can be seen from a given location and returns
  a tuple of location to max number."
  [asteroid-belt]
  (let [all-asteroid-locations (all-asteroid-locations asteroid-belt)]
    (->> all-asteroid-locations
         (r/map (fn [location]
                  [location (number-asteroids-visible-from-location all-asteroid-locations location)]))
         (into [])
         (sort-by #(nth % 1) (Comparator/reverseOrder))
         first)))

(defn vaporization-order
  "Part 2 - Returns the order by which the asteroid belt will be vaporized given a start location."
  [asteroid-belt [x y]]
  (let [all-asteroid-locations (all-asteroid-locations asteroid-belt)
        distance-and-angles (distance-and-angle-of-other-asteroids all-asteroid-locations [x y])
        angles (group-by #(nth % 2) distance-and-angles)]
    (->> (keys angles)
         sort
         (reduce
           (fn [acc angle]
             (->> (get angles angle)
                  (sort-by #(nth % 1))
                  (map-indexed (fn [i angle]
                                 (conj angle (+ (nth angle 2) (* 360.0 i)))))
                  (apply conj acc)))
           [])
         (sort-by #(nth % 3)))))

(defn run
  "Runs part 1 and 2 of day 10."
  []
  (let [asteroid-belt (asteroid-belt)
        [best-location num-asteroids] (highest-number-of-asteroids asteroid-belt)
        [[target-x target-y]] (first (drop 199 (vaporization-order asteroid-belt best-location)))]
    (println "Part 1 - Highest number of asteroids:" num-asteroids best-location)
    (println "Part 2 - 200th asteroid vaporization:"
             (+ (* 100 target-x) target-y) [target-x target-y])))

