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
    (<= 1 diff 3)))

(defn safe-gaps [reports]
  (->> reports
       (adj-pairs)
       (map safe-gap)
       (every? true?)))

(defn safe?-pt1 [report]
  (and
    (safe-gaps report)
    (monotonic? report)))

(def reports
  (map line-to-vec (readlines)))

(defn count-safe-reports [safe-fn reports]
  (->> reports
       (filter safe-fn)
       (count)))

(defn count-safe-reports-1 [reports]
  (count-safe-reports safe?-pt1 reports))

(defn remove-at [v i]
  (let [v (vec v)]
    (into (subvec v 0 i) (subvec v (inc i)))))

(defn safe?-pt2 [reports]
  (let [indices (range (count reports))
        remove (partial remove-at reports)
        with-removals (map remove indices)
        report-permutations (conj with-removals reports)]
    (some safe?-pt1 report-permutations)))

(defn count-safe-reports-2 [reports]
  (count-safe-reports safe?-pt2 reports))

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
  (count-safe-reports-1 reports)
  (count-safe-reports-2 reports)
  (safe-gaps [7 6 4 2 1])
  (remove-at [1 2 3 4] (last (range (count [1 2 3 4]))))
  (adj-pairs [7 6 4 2 1]))