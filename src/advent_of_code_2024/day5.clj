(ns advent-of-code-2024.day5
  (:require [clojure.string :as str]))

(defn readfile []
  (slurp (clojure.java.io/resource "day5.txt")))

(defn parse-updates [u]
  (map #(map parse-long %)
       (map #(str/split % #",")
            (str/split-lines u))))

(defn parse-rule [rule]
  (map parse-long (str/split rule #"\|")))

(defn parse-rules [rules]
  (map parse-rule (str/split-lines rules)))

(defn parse-file [f]
  (let [[rules updates] (str/split f #"\n\n")]
    {:updates (parse-updates updates)
     :rules   (parse-rules rules)}))

(defn correct-order [rule update]
  (let [[pre post] rule
        pre-i (.indexOf update pre)
        pre-i (if (= -1 pre-i) Double/NEGATIVE_INFINITY pre-i)
        post-i (.indexOf update post)
        post-i (if (= -1 post-i) Double/POSITIVE_INFINITY post-i)]
    (< pre-i post-i)))

(defn correct-order-all-rules [rules update]
  (every? true?
          (for [rule rules]
            (correct-order rule update))))

(defn middle [seq]
  (get (vec seq) (unchecked-divide-int (count seq) 2)))

(defn part1 [{:keys [rules updates]}]
  (->> updates
       (filter (partial correct-order-all-rules rules))
       (map middle)
       (reduce +)
       ))

(comment
  (prn file)
  (readfile)
  (map str/split-lines (str/split file #"\n\n"))
  (parse-file (readfile))
  (use-thing (parse-file (readfile)))
  (correct-order [1 2] [0 1 2 3])
  (correct-order [ 1 8] [1 2 3])
  (middle '(75 47 61 53 29))
  (or  4  6)
  (middle [1 2 3 4 5])
  (unchecked-divide-int (count [1 2 3]) 2)
  (correct-order-all-rules [[1 2] [2 3] [4 6]] [1 2 3])
  (part1 (parse-file (readfile)))
  )