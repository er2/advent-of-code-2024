(ns advent-of-code-2024.day1
  (:require [clojure.java.io :as io]
            [clojure.string :as str]))

(defn readlines []
  (str/split-lines (slurp (io/resource "day1.txt"))))

(defn line-to-pair [l]
  (mapv parse-long (str/split l #"\s+")))

(defn to-pairs [lines]
  (map line-to-pair lines))

(defn unzip [pairs]
  (let [l (map first pairs)
        r (map second pairs)]
    [l r]))

(defn sort-lists [pairs]
  (map sort (unzip pairs)))

(defn to-matched-pairs [lists]
  (map vector (first lists) (second lists)))

(defn distance [[l r]]
  (abs (- l r)))

(defn calc-total-distance []
  "Part 1"
  (->> (readlines)
      (to-pairs)
      (sort-lists)
      (to-matched-pairs)
      (map distance)
      (reduce +)))

(defn proc-similarity [[ns occurences]]
  (let [freqs (frequencies occurences)
        calc (fn [n]
               (* n (freqs n 0)))]
    (map calc ns)))

(defn calc-similarity []
  "Part 2"
  (->> (readlines)
       (to-pairs)
       (unzip)
       (proc-similarity)
       (reduce +)))

(comment
  (readlines)
  (line-to-pair "3   4")
  (to-pairs (readlines))
  (sort-lists (to-pairs (readlines)))
  (to-matched-pairs (sort-lists (to-pairs (readlines))))
  (to-matched-pairs (to-pairs (readlines)))
  (distance [1 3])
  (calc-total-distance)
  (frequencies [1 2 2 4])
  (proc-similarity [[1] [1 1]])
  (calc-similarity))