(ns advent-of-code.day-12
  (:require [clojure.string :as str]
            [clojure.java.io :as io]))

(defn moons
  "Reads all the moons for day 12 and places each one in a vector of [x y z vx vy vz]."
  []
  (->> (io/resource "day_12.txt")
       slurp
       str/split-lines
       (mapv (fn [line]
               (as-> line $
                     (str/replace $ #"[^\d|^,|^\-]" "")
                     (str/split $ #",")
                     (mapv #(Integer/valueOf %) $)
                     (apply conj $ [0 0 0])
                     (vec $))))))

(defn velocity-diff
  "Computes the velocity difference between the two coordinates."
  [coord-1 coord-2]
  (if (= coord-1 coord-2) 0 (if (< coord-1 coord-2) 1 -1)))

(defn velocity-offset
  "Given a coordinate and a sequence of other coordinates, compute the
  entire velocity offset."
  [coord-1 coords]
  (reduce + (map (partial velocity-diff coord-1) coords)))

(defn apply-gravity-and-velocity-changes-for-moons
  "Given the moons, computes the new moon after apply gravity and velocity."
  [moons target-moon-idx]
  (let [[x y z vx vy vz] (nth moons target-moon-idx)
        check-idxes (remove #(= target-moon-idx %) (range (count moons)))
        new-vx (+ vx (velocity-offset x (map #(get-in moons [% 0]) check-idxes)))
        new-vy (+ vy (velocity-offset y (map #(get-in moons [% 1]) check-idxes)))
        new-vz (+ vz (velocity-offset z (map #(get-in moons [% 2]) check-idxes)))]
    [(+ x new-vx) (+ y new-vy) (+ z new-vz) new-vx new-vy new-vz]))

(defn generate-gravity-and-velocity-steps
  "Generates a lazy seq of gravity and velocity steps."
  [initial-moons]
  (iterate
    (fn [moons]
      (mapv (partial apply-gravity-and-velocity-changes-for-moons moons)
            (range (count moons))))
    initial-moons))

(defn potential-energy-of-moon
  "Computes the potential energy of each moon by summing the absolute values of the position coordinates."
  [moon]
  (->> (subvec moon 0 3)
       (map #(Math/abs %))
       (reduce +)))

(defn kinetic-energy-of-moon
  "Computes the kinetic energy of each moon by summing the absolute values of the velocity coordinates."
  [moon]
  (->> (subvec moon 3 6)
       (map #(Math/abs %))
       (reduce +)))

(defn total-energy-of-moon
  "Computes the total energy of the moon by multiplying the potential energy by the kinetic energy."
  [moon]
  (* (potential-energy-of-moon moon) (kinetic-energy-of-moon moon)))

(defn total-energy-of-system
  "Computes the total energy of the system by summing the total energy of each moon."
  [moons]
  (reduce + (map total-energy-of-moon moons)))

(defn gcd
  "Finds the gcd between x and y."
  [x y]
  (if (zero? y)
    x
    (recur y (mod x y))))

(defn lcm
  "Finds the lcm between x and y."
  [x y]
  (/ (* x y) (gcd x y)))

(defn position-velocity-pairs
  "Given moons, returns either [x, vx], [y, vy], [z, vz] depending on offset."
  [moons offset]
  (mapv #(vector (nth % offset) (nth % (+ 3 offset))) moons))

(defn steps-until-loop
  "Computes the number of steps until there is a loop by finding the LCM between the
  position/velocity periods for each axis."
  [moons]
  (->> (loop [[moons & rest] (generate-gravity-and-velocity-steps moons)
              step 0
              acc [[#{} nil] [#{} nil] [#{} nil]]]
         (let [periods (mapv #(nth % 1) acc)]
           (if (every? some? periods)
             periods
             (recur rest
                    (inc step)
                    (mapv
                      (fn [idx]
                        (let [[current-pairs period] (nth acc idx)
                              pairs (position-velocity-pairs moons idx)]
                          (if period
                            (nth acc idx)
                            [(conj current-pairs pairs)
                             (or period (when (contains? current-pairs pairs) step))])))
                      (range 3))))))
       (reduce lcm)))

(defn run
  "Runs part 1 and 2 of day 12."
  []
  (let [moons (moons)]
    (println "Part 1 - Total energy of the system after 1000 steps:"
             (total-energy-of-system (first (drop 1000 (generate-gravity-and-velocity-steps moons)))))
    (println "Part 1 - Total steps until first loop:"
             (steps-until-loop moons))))