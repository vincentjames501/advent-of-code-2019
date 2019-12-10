(ns advent-of-code.day-08
  (:require [clojure.java.io :as io]))

(defn process-image
  "Returns an image which is collection of layers by pixels tall by pixels wide"
  [pixels-wide pixels-tall]
  (->> (io/resource "day_08.txt")
       slurp
       (mapv #(Integer/valueOf (str %)))
       (partition-all pixels-wide)
       (mapv vec)
       (partition-all pixels-tall)
       (mapv vec)))

(defn layer-with-fewest-digits
  "Returns a tuple of [layer-idx digit count]."
  [image digit]
  (->> image
       (map-indexed (fn [idx layer] [idx (count (filter #(= digit %) (flatten layer)))]))
       (sort-by #(nth % 1))
       first))

(defn count-digits-for-layer
  "Given a layer, returns the number of occurrences for the given digit on that layer."
  [image layer-idx digit]
  (count (filter #(= digit %) (flatten (nth image layer-idx)))))

(defn checksum
  "Part 1 - Finds the layer with the fewest zeros, then multiplies the number of 1 digits
  on that layer with the number of 2 digits on that layer."
  [image fewest-digits first-digit-count second-digit-count]
  (let [[layer-idx] (layer-with-fewest-digits image fewest-digits)]
    (* (count-digits-for-layer image layer-idx first-digit-count)
       (count-digits-for-layer image layer-idx second-digit-count))))

(defn color-for-image
  "Gets the color for an image at a specific coordinate removing the transparent
  layers first."
  [image x y]
  (->> image
       (map #(get-in % [x y]))
       (drop-while #(= 2 %))
       first))

(defn message-for-image
  "Part 2 - Prints the message for the given image."
  [image pixels-wide pixels-tall]
  (let [message (mapv (fn [x]
                        (mapv (fn [y]
                                (color-for-image image x y))
                              (range pixels-wide)))
                      (range pixels-tall))]
    (doseq [x (range pixels-tall)]
      (doseq [y (range pixels-wide)]
        (print (if (= (get-in message [x y]) 1) "|" " ")))
      (println))))

(defn run
  "Runs part 1 and 2 of day 8."
  []
  (let [pixels-wide 25
        pixels-tall 6
        image (process-image pixels-wide pixels-tall)]
    (println "Part 1 - Checksum:" (checksum image 0 1 2))
    (println "Part 2 - Message:")
    (message-for-image image pixels-wide pixels-tall)))

