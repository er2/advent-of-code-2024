(ns advent-of-code-2024.day6
  (:require [clojure.java.io :as io]
            [clojure.string :as str]))

(defn readlines []
  (str/split-lines (slurp (io/resource "day6.txt"))))

(defn parse-obstacles [lines]
  (into #{}
        (for [y (range (count lines))
              x (range (.length (first lines)))
              :let [elem (get (get lines y) x)]
              :when (= elem \#)]
          [x y])))

(defn parse-guard [lines]
  (first (for [y (range (count lines))
               x (range (.length (first lines)))
               :let [elem (get (get lines y) x)]
               :when (#{\v \^ \< \>} elem)]
           {:loc [x y]
            :dir elem})))

(defn parse-map [lines]
  {:obstacles (parse-obstacles lines)
   :guard     (parse-guard lines)
   :walked    #{}
   :max-x     (.length (first lines))
   :max-y     (count lines)})

(def forward
  {\^ (fn [[x y]] [x (dec y)])
   \< (fn [[x y]] [(dec x) y])
   \> (fn [[x y]] [(inc x) y])
   \v (fn [[x y]] [x (inc y)])})

(defn in-map [map]
  (fn [[x y]]
    (and
      (< -1 x (:max-x map))
      (< -1 y (:max-y map)))))

(defn walk-forward [map]
  (let [{guard     :guard
         obstacles :obstacles} map
        {loc :loc
         dir :dir} guard
        forward (forward dir)
        in-map (in-map map)
        unobstructed (fn [coord] (not (obstacles coord)))]
    (->>
      (iterate forward loc)
      (take-while (every-pred in-map unobstructed)))))

(comment
  (readlines)
  (range 10)
  (take 5 (iterate (forward \>) [0 0]))
  (parse-obstacles (readlines))
  (parse-guard (readlines))
  (parse-map (readlines))
  ((in-map (parse-map (readlines))) [0 0])
  (walk-forward (parse-map (readlines))))