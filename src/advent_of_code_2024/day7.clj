(ns advent-of-code-2024.day7
  (:require [clojure.java.io :as io]
            [clojure.string :as str]))

(defn readlines []
  (str/split-lines (slurp (io/resource "day7.txt"))))

(defn parse-line [line]
  (let [[targetPart operandPart] (str/split line #": ")
        target (parse-long targetPart)
        operands (map parse-long (str/split operandPart #"\s+"))]
    {:target   target
     :operands (reverse operands)}))

(defn combos [operands]
  (let [[head & tail] operands]
    (if (empty? tail)
      [head]
      (let [prods (map #(* head %) (combos tail))
            sums (map #(+ head %) (combos tail))]
        (into prods sums)))))

(defn test-line [{:keys [target operands]}]
  (if ((into #{} (combos operands)) target)
    target
    nil))

(defn part1 []
  (->> (readlines)
       (map parse-line)
       (map test-line)
       (filter some?)
       (reduce +)
       ))

(comment
  (readlines)
  (map parse-line (readlines))
  (combos [1 2])
  (let [[head & tail] [1 2]]
    (prn head tail))
  (test-line {:target 190 :operands '(10 19)})
  (test-line {:target 3267 :operands '(81 40 27)})
  (combos '(81 40 27))
  (part1))