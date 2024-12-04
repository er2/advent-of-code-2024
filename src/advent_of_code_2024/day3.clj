(ns advent-of-code-2024.day3
  (:require [clojure.java.io :as io]))

(def lines
  (slurp (io/resource "day3.txt")))

(def mul-reg
  #".*?(mul\(\d+,\d+\)).*?")

(def mul-do-dont-reg
  #".*?(do\(\)|don't\(\)|mul\(\d+,\d+\)).*?")

(defn extract-muls [txt]
  (map second (re-seq mul-reg txt)))

(defn parse-muls [s]
  (if-let [nums (re-seq #"\d+" s)]
    (apply * (map parse-long nums))
    0))

(defn part1 []
  (->> lines
       (extract-muls)
       (map parse-muls)
       (reduce +)))

(defn extract-instrs [l]
  (map second (re-seq mul-do-dont-reg l)))

(defn new-enabled [instr old-enabled]
  (cond (= instr "don't()")
        false
        (= instr "do()")
        true
        :else
        old-enabled))

(defn proc-instructions [instructions running-total enabled]
  (if (nil? instructions)
    running-total
    (let [[head & rest] instructions
          enabled (new-enabled head enabled)]
      (if (not enabled)
        (recur rest running-total enabled)
        (let [running-total (+ (parse-muls head) running-total)]
          (recur rest running-total enabled))))))

(defn part2 []
  (-> lines
      (extract-instrs)
      (proc-instructions 0 true)))

(comment
  (part1)
  (part2)
  (re-seq mul-do-dont-reg lines)
  (parse-muls "mul(2,4)")
  (extract-muls lines)
  (map parse-mul (extract-muls lines)))