(ns advent-of-code-2024.day2
  (:require [clojure.java.io :as io]
            [clojure.string :as str]))

(defn readlines []
  (str/split-lines (slurp (io/resource "day2.txt"))))

(defn line-to-vec [line]
  (map parse-long (str/split line #"\s+")))

(defn adj-pairs [seq]
  (partition 2 1 seq))

(defn monotonic? [report]
  (or
    (apply <= report)
    (apply >= report)))

(defn safe-gap [[l r]]
  (let [diff (abs (- l r))]
    (apply <= [1 diff 3])))

(defn safe-gaps [reports]
  (let [pairs (adj-pairs reports)]
    (->> pairs
         (map safe-gap)
         (every? true?)
         )))

(defn safe? [report]
  (and
    (safe-gaps report)
    (monotonic? report)))

(def reports
  (map line-to-vec (readlines)))

(defn count-safe-reports [reports]
  "Part 1"
  (->> reports
       (filter safe?)
       (count)))

(comment
  (readlines)
  (partition 2 1 '(1 2 3))
  (adj-pairs [1 2 3 4])
  (every? true? [true true])
  (every? < [[1 2] [4 3]])
  (true? (< 1 0))
  (apply <= [1 2 3 1])
  (monotonic? [1 2 3])
  (monotonic? [3 2 1])
  (monotonic? [1 2 1])
  (map line-to-vec (readlines))
  reports
  (count-safe-reports reports)
  (safe-gaps [7 6 4 2 1])
  (adj-pairs [7 6 4 2 1]))